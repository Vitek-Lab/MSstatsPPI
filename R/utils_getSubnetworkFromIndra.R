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
    if (nrow(input) == 0) {
        stop("Invalid Input Error: Input must contain at least one protein after filtering.")
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

#' Call INDRA Cogex API and return response
#' @param res response from INDRA
#' @param interaction_types interaction types to filter by
#' @param evidence_count_cutoff number of evidence to filter on for each paper
#' @return filtered list of INDRA statements
#' @keywords internal
#' @noRd
.filterIndraResponse <- function(res, interaction_types, evidence_count_cutoff) {
    filtered_response = Filter(
        function(statement) statement$data$stmt_type %in% interaction_types, 
        res)
    filtered_response = Filter(
        function(statement) statement$data$evidence_count >= evidence_count_cutoff, 
        filtered_response
    )
    return(filtered_response)
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
    matched_rows_source <- input[which(input$HgncId == edge$source_id), ]
    matched_rows_target <- input[which(input$HgncId == edge$target_id), ]
    
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
            edgeToMetadataMapping[[key]]$data$stmt_type <- unique(c(
                edgeToMetadataMapping[[key]]$data$stmt_type,
                edge$data$stmt_type))
            edgeToMetadataMapping[[key]]$data$paper_count <- 
                edgeToMetadataMapping[[key]]$data$paper_count + 1
        } else {
            edge <- .addAdditionalMetadataToIndraEdge(edge, input)
            edge$data$paper_count <- 1
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
#' @param protein_level_data output of dataProcess
#' @importFrom r2r query keys
#' @importFrom MSstats quantification
#' @importFrom tidyr pivot_wider
#' @return edge data.frame
#' @keywords internal
#' @noRd
.constructEdgesDataFrame <- function(res, input, protein_level_data) {
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
        paperCount = vapply(keys(res), function(x) {
            query(res, x)$data$paper_count
        }, 1),
        evidenceLink = vapply(keys(res), function(x) {
            query(res, x)$evidence_list
        }, ""),
        stringsAsFactors = FALSE
    )
    # add correlation - maybe create a separate function
    if (!is.null(protein_level_data)) {
        protein_level_data <- Filter(function(row) 
            (row$Protein %in% edges$source | row$Protein %in% edges$target), 
            protein_level_data)
        long_format <-  MSstats::quantification(protein_level_data, format="long")
        wide_data <- pivot_wider(long_format, names_from = Protein, values_from = LogIntensity) # This should be in the MSstats quantification function
        wide_data <- wide_data[, -which(names(wide_data) == "Group_Subject")]
        correlations = cor(wide_data, use = "pairwise.complete.obs")
        edges$correlation = lapply(function(edge) correlations[edge$source, edge$target], edges)
    }
    return(edges)
}

#' Construct nodes data.frame from groupComparison output
#' @param input filtered groupComparison result
#' @param edges edges data frame
#' @return nodes data.frame
#' @keywords internal
#' @noRd
.constructNodesDataFrame <- function(input, edges) {
    nodes <- data.frame(
        id = input$Protein,
        logFC = input$log2FC,
        pvalue = input$adj.pvalue,
        hgncName = if ("HgncName" %in% colnames(input) && is.character(input$HgncName)) input$HgncName else NA,
        stringsAsFactors = FALSE
    )
    nodes <- nodes[which(nodes$id %in% edges$source | nodes$id %in% edges$target),]
    return(nodes)
}

#' Filter Edges Data Frame
#' @param edges response from INDRA
#' @param paper_count_cutoff cutoff for number of papers
#' @param correlation_cutoff if protein_level_abundance is not NULL, apply a 
#' cutoff for edges with correlation less than a specified cutoff.
#' @return filtered edges data frame
#' @keywords internal
#' @noRd
.filterEdgesDataFrame <- function(edges, 
                                  paper_count_cutoff,
                                  correlation_cutoff) {
    edges <- edges[which(edges$paperCount >= paper_count_cutoff), ]
    if ("correlation" %in% colnames(edges)) {
        edges <- edges[which(abs(edges$correlation) >= correlation_cutoff), ]
    }
    return(edges)
}
