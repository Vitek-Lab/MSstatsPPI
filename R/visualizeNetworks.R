#' Create visualization of network in Cytoscape Desktop.  Note that the
#' Cytoscape Desktop app must be open for this function to work.
#'
#' @param nodes dataframe of nodes
#' @param edges dataframe of edges
#' @param pvalueCutoff p-value cutoff for coloring significant proteins.
#' Default is 0.05
#' @param logfcCutoff log fold change cutoff for coloring significant
#' proteins. Default is 0.5
#' @importFrom RCy3 createNetworkFromDataFrames mapVisualProperty
#' createVisualStyle setVisualStyle
#'
#' @export
#'
#' @examples
#' input <- data.table::fread(system.file(
#'     "extdata/groupComparisonModel.csv",
#'     package = "MSstatsBioNet"
#' ))
#' subnetwork <- getSubnetworkFromIndra(input)
#' visualizeNetworks(subnetwork$nodes, subnetwork$edges)
#'
#' @return cytoscape visualization of subnetwork
#'
#'
visualizeNetworks <- function(nodes, edges,
                              pvalueCutoff = 0.05, logfcCutoff = 0.5) {
    # Add additional columns for visualization
    nodes$logFC_color <- nodes$logFC
    nodes$logFC_color[nodes$pvalue > pvalueCutoff |
        abs(nodes$logFC) < logfcCutoff] <- 0

    # Create network
    if (interactive()) {
        createNetworkFromDataFrames(nodes, edges)

        # Apply visual style
        DEFAULT_VISUAL_STYLE <- list(
            NODE_SHAPE = "ROUNDRECT",
            NODE_SIZE = 50,
            NODE_LABEL_FONT_SIZE = 6,
            NODE_LABEL_POSITION = "center",
            EDGE_TARGET_ARROW_SHAPE = "Arrow"
        )
        VISUAL_STYLE_NAME <- "MSstats-Indra Visual Style"

        VISUAL_STYLE_MAPPINGS <- list(
            mapVisualProperty("Node Label", "id", "p"),
            mapVisualProperty(
                "Node Fill Color", "logFC_color", "c",
                c(-logfcCutoff, 0.0, logfcCutoff),
                c("#5588DD", "#5588DD", "#D3D3D3", "#DD8855", "#DD8855")
            )
        )
        createVisualStyle(
            VISUAL_STYLE_NAME,
            DEFAULT_VISUAL_STYLE,
            VISUAL_STYLE_MAPPINGS
        )
        setVisualStyle(VISUAL_STYLE_NAME)
    } else {
        warning("Visualization is not available in non-interactive mode.")
    }
}
