#' Get subnetwork from INDRA database
#'
#' Using differential abundance results from MSstats, this function retrieves
#' a subnetwork of protein interactions from INDRA database.
#'
#' @param input output of groupComparison function's comparisionResult table,
#' which contains a list of proteins and their corresponding p-values, logFCs,
#' along with additional HGNC ID and HGNC name columns
#' @param pvalueCutoff p-value cutoff for filtering. Default is NULL, i.e. no
#' filtering
#'
#' @return list of 2 data.frames, nodes and edges
#' @seealso \code{\link[MSstats]{groupComparison}}
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
getSubnetworkFromIndra <- function(input, pvalueCutoff = NULL) {
    input <- .filterGetSubnetworkFromIndraInput(input, pvalueCutoff)
    res <- .callIndraCogexApi(input$HgncId)
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
