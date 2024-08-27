#' Get subnetwork from INDRA database with differential analysis results
#'
#' @param input groupComparison comparisionResult table with additional HGNC ID
#' and HGNC name columns
#' @param pvalue_cutoff p-value cutoff for filtering. Default is NULL, i.e. no
#' filtering
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
#' # subnetwork = getSubnetworkFromIndra(input, pvalue_cutoff = 0.05)
#' # head(subnetwork$nodes)
#' # head(subnetwork$edges)
#'
getSubnetworkFromIndra <- function(input, pvalue_cutoff = NULL) {
    input <- .filterGetSubnetworkFromIndraInput(input, pvalue_cutoff)
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
