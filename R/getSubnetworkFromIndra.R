#' Get subnetwork from INDRA database
#'
#' Using differential abundance results from MSstats, this function retrieves
#' a subnetwork of protein interactions from INDRA database.
#'
#' @param input output of \code{\link[MSstats]{groupComparison}} function's 
#' comparisionResult table, which contains a list of proteins and their 
#' corresponding p-values, logFCs, along with additional HGNC ID and HGNC 
#' name columns
#' @param pvalueCutoff p-value cutoff for filtering. Default is NULL, i.e. no
#' filtering
#' @param interaction_types list of interactions to filter on.  Equivalent to
#' statement type in INDRA.  Default is c("IncreaseAmount", "DecreaseAmount").
#'
#' @return list of 2 data.frames, nodes and edges
#'
#' @export
#'
#' @examples
#' input <- data.table::fread(system.file(
#'     "extdata/groupComparisonModel.csv",
#'     package = "MSstatsBioNet"
#' ))
#' subnetwork <- getSubnetworkFromIndra(input, pvalueCutoff = 0.05)
#' head(subnetwork$nodes)
#' head(subnetwork$edges)
#'
getSubnetworkFromIndra <- function(input, 
                                   pvalueCutoff = NULL, 
                                   interaction_types = c("IncreaseAmount", "DecreaseAmount")) {
    input <- .filterGetSubnetworkFromIndraInput(input, pvalueCutoff)
    .validateGetSubnetworkFromIndraInput(input)
    res <- .callIndraCogexApi(input$HgncId)
    res <- .filterIndraResponse(res, interaction_types)
    nodes <- .constructNodesDataFrame(input)
    edges <- .constructEdgesDataFrame(res, input)
    warning(
        "NOTICE: This function includes third-party software components
        that are licensed under the BSD 2-Clause License. Please ensure to
        include the third-party licensing agreements if redistributing this
        package or utilizing the results based on this package.
        See the LICENSE file for more details."
    )
    return(list(nodes = nodes, edges = edges))
}
