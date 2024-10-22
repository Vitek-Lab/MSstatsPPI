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
        edge$target_id, "&format=html",
        sep = ""
    )
    edge$source_uniprot_id <- input[input$HgncId == edge$source_id, ]$Protein
    edge$target_uniprot_id <- input[input$HgncId == edge$target_id, ]$Protein
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
            edgeToMetadataMapping[[key]]$data$stmt_type <- c(
                edgeToMetadataMapping[[key]]$data$stmt_type,
                edge$data$stmt_type)
        } else {
            edge <- .addAdditionalMetadataToIndraEdge(edge, input)
            edgeToMetadataMapping[[key]] <- edge
        }
    }
    
    for (key in keys(edgeToMetadataMapping)) {
        edgeToMetadataMapping[[key]]$data$stmt_type <-
            unique(edgeToMetadataMapping[[key]]$data$stmt_type)
        edgeToMetadataMapping[[key]]$data$stmt_type <-
            paste(edgeToMetadataMapping[[key]]$data$stmt_type, collapse = ", ")
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
