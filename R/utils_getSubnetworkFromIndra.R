#' Validate input for MSstatsBioNet getSubnetworkFromIndra
#' @param input dataframe from MSstats groupComparison output
#' @keywords internal
#' @noRd
.validateGetSubnetworkFromIndraInput <- function(input) {
    if (!"HgncId" %in% colnames(input)) {
        stop("Invalid Input Error: Input must contain a column named 'HgncId'.")
    }
    if (nrow(input) >= 400) {
        stop("Invalid Input Error: INDRA query must contain less than 400 proteins.  Consider lowering your p-value cutoff")
    }
}

#' Call INDRA Cogex API and return response
#' @param hgncIds list of hgnc ids
#' @return list of INDRA statements
#' @importFrom jsonlite toJSON
#' @importFrom httr POST add_headers content
#' @keywords internal
#' @noRd
.callIndraCogexApi <- function(hgncIds) {
    indraCogexUrl <-
        "https://discovery.indra.bio/api/indra_subnetwork_relations"

    groundings <- lapply(hgncIds, function(x) list("HGNC", x))
    groundings <- list(nodes = groundings)
    groundings <- jsonlite::toJSON(groundings, auto_unbox = TRUE)

    res <- POST(
        indraCogexUrl,
        body = groundings,
        add_headers("Content-Type" = "application/json"),
        encode = "raw"
    )
    res <- content(res)
    return(res)
}

#' Filter groupComparison result input based on user-defined cutoffs
#' @param input groupComparison result
#' @param pvalueCutoff p-value cutoff
#' @return filtered groupComparison result
#' @keywords internal
#' @noRd
.filterGetSubnetworkFromIndraInput <- function(input, pvalueCutoff) {
    input <- input[!is.na(input$adj.pvalue),]
    if (!is.null(pvalueCutoff)) {
        input <- input[input$adj.pvalue < pvalueCutoff, ]
    }
    input <- input[is.na(input$issue), ]
    input$Protein <- as.character(input$Protein)
    return(input)
}

#' Add additional metadata to an edge
#' @param edge object representation of an INDRA statement
#' @param input filtered groupComparison result
#' @return edge with additional metadata
#' @keywords internal
#' @noRd
.addAdditionalMetadataToIndraEdge <- function(edge, input) {
    edge$evidence_list <- paste(
        "https://db.indra.bio/statements/from_agents?subject=",
        edge$source_id, "@HGNC&object=",
        edge$target_id, "@HGNC&format=html",
        sep = ""
    )
    
    # Convert back to uniprot IDs
    matched_rows_source <- input[input$HgncId == edge$source_id & !is.na(input$Protein), ]
    matched_rows_target <- input[input$HgncId == edge$target_id & !is.na(input$Protein), ]
    
    if (nrow(matched_rows_source) != 1 || nrow(matched_rows_target) != 1) {
        stop(paste0(
            "INDRA Exception: Unexpected number of matches for the following HGNC IDs in the input data: ", 
            edge$source_id, 
            " or ", 
            edge$target_id, 
            ". Each ID must match exactly one entry in the input data, but 0 or multiple matches were found. Please check the input data for duplicates or missing entries."
        ))
    } 
    
    edge$source_uniprot_id <- matched_rows_source$Protein
    edge$target_uniprot_id <- matched_rows_target$Protein
    
    return(edge)
}


#' Collapse duplicate INDRA statements into a mapping of edge to metadata
#' @param res INDRA response
#' @param input filtered groupComparison result
#' @importFrom r2r hashmap keys
#' @return processed edge to metadata mapping
#' @keywords internal
#' @noRd
.collapseDuplicateEdgesIntoEdgeToMetadataMapping <- function(res, input) {
    edgeToMetadataMapping <- hashmap()

    for (edge in res) {
        key <- paste(edge$source_id, edge$target_id, sep = "_")
        if (key %in% keys(edgeToMetadataMapping)) {
            edgeToMetadataMapping[[key]]$data$evidence_count <-
                edgeToMetadataMapping[[key]]$data$evidence_count +
                edge$data$evidence_count
            edgeToMetadataMapping[[key]]$data$stmt_type <- unique(c(
                edgeToMetadataMapping[[key]]$data$stmt_type,
                edge$data$stmt_type))
        } else {
            edge <- .addAdditionalMetadataToIndraEdge(edge, input)
            edgeToMetadataMapping[[key]] <- edge
        }
    }
    
    for (key in keys(edgeToMetadataMapping)) {
        edgeToMetadataMapping[[key]]$data$stmt_type <-
            paste(unique(edgeToMetadataMapping[[key]]$data$stmt_type), 
                  collapse = ", ")
    }

    return(edgeToMetadataMapping)
}

#' Construct edges data.frame from INDRA response
#' @param res INDRA response
#' @param input filtered groupComparison result
#' @importFrom r2r query keys
#' @return edge data.frame
#' @keywords internal
#' @noRd
.constructEdgesDataFrame <- function(res, input) {
    res <- .collapseDuplicateEdgesIntoEdgeToMetadataMapping(res, input)
    edges <- data.frame(
        source = vapply(keys(res), function(x) {
            query(res, x)$source_uniprot_id
        }, ""),
        target = vapply(keys(res), function(x) {
            query(res, x)$target_uniprot_id
        }, ""),
        interaction = vapply(keys(res), function(x) {
            query(res, x)$data$stmt_type
        }, ""),
        evidenceCount = vapply(keys(res), function(x) {
            query(res, x)$data$evidence_count
        }, 1),
        evidenceLink = vapply(keys(res), function(x) {
            query(res, x)$evidence_list
        }, ""),
        stringsAsFactors = FALSE
    )
    return(edges)
}

#' Construct nodes data.frame from groupComparison output
#' @param input filtered groupComparison result
#' @return nodes data.frame
#' @keywords internal
#' @noRd
.constructNodesDataFrame <- function(input) {
    nodes <- data.frame(
        id = input$Protein,
        logFC = input$log2FC,
        pvalue = input$adj.pvalue,
        stringsAsFactors = FALSE
    )
    return(nodes)
}
