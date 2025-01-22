#' Create visualization of network
#'
#' Use results from INDRA to generate a visualization of the a network on
#' Cytoscape Desktop.  Note that the Cytoscape Desktop app must be open for
#' this function to work.
#'
#' @param nodes dataframe of nodes consisting of columns
#' id (chararacter), pvalue (number), logFC (number)
#' @param edges dataframe of edges consisting of columns
#' source (character), target (character), interaction (character),
#' evidenceCount (number), evidenceLink (character)
#' @param pvalueCutoff p-value cutoff for coloring significant proteins.
#' Default is 0.05
#' @param logfcCutoff log fold change cutoff for coloring significant
#' proteins. Default is 0.5
#' @param node_label_column The column of the nodes dataframe to use as the 
#' node label.  Default is "id".  "hgncName" can be used for gene name.
#' @param main_targets character vector of main targets to stand-out with a 
#' different node shape.  Default is an empty vector c(). IDs of main targets
#' should match the column used by the node_label_column parameter.
#' @importFrom RCy3 createNetworkFromDataFrames mapVisualProperty
#' createVisualStyle setVisualStyle layoutNetwork addAnnotationShape
#' addAnnotationText getNodePosition getNetworkCenter
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
                              pvalueCutoff = 0.05, 
                              logfcCutoff = 0.5,
                              node_label_column = "id",
                              main_targets = c()) {
    .validateVisualizeNetworks(nodes, node_label_column, main_targets)
    
    # Add additional columns for visualization
    nodes$logFC_color <- nodes$logFC
    nodes$logFC_color[nodes$pvalue > pvalueCutoff |
        abs(nodes$logFC) < logfcCutoff] <- 0
    nodes$logFC_abs <- abs(nodes$logFC)
    nodes$is_main_target <- nodes[,node_label_column] %in% main_targets

    # Create network
    if (interactive()) {
        createNetworkFromDataFrames(nodes, edges)

        # Apply visual style
        DEFAULT_VISUAL_STYLE <- list(
            NODE_LABEL_POSITION = "center",
            EDGE_TARGET_ARROW_SHAPE = "Arrow",
            EDGE_LABEL_FONT_SIZE = 6
        )
        VISUAL_STYLE_NAME <- "MSstats-Indra Visual Style"

        VISUAL_STYLE_MAPPINGS <- list(
            mapVisualProperty('Node Shape', 'is_main_target', 'd', c(TRUE, FALSE), c('DIAMOND', 'ROUNDRECT')),
            mapVisualProperty("Node Label", node_label_column, "p"),
            mapVisualProperty(
                "Node Fill Color", "logFC_color", "c",
                c(-logfcCutoff, 0.0, logfcCutoff),
                c("#ADD8E6", "#ADD8E6", "#D3D3D3", "#FFA590", "#FFA590")
            ),
            mapVisualProperty("Edge Label", "interaction", "p"),
            mapVisualProperty("Node Label Font Size", "logFC_abs", "c",
                              c(0, 1, 2),
                              c(6, 12, 18)),
            mapVisualProperty("Node Size", "logFC_abs", "c",
                              c(0, 1, 2),
                              c(50, 100, 150))
        )
        createVisualStyle(
            VISUAL_STYLE_NAME,
            DEFAULT_VISUAL_STYLE,
            VISUAL_STYLE_MAPPINGS
        )
        setVisualStyle(VISUAL_STYLE_NAME)
        layoutNetwork("cose")
        
        # Define legend properties
        legend_items <- list(
            list(color = "#ADD8E6", label = paste0("logFC < ", -logfcCutoff)),  # Blue for negative
            list(color = "#EEEEEE", label = paste0(-logfcCutoff, "< logFC < ", logfcCutoff)), # Light gray for zero
            list(color = "#FFA590", label = paste0("logFC > ", logfcCutoff))  # Red for positive
        )
        .addLegendInCytoscape(legend_items)
    } else {
        warning("Visualization is not available in non-interactive mode.")
    }
}
