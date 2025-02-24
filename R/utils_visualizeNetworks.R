#' Validate input for MSstatsBioNet visualizeNetworks
#' @param nodes dataframe of nodes
#' @param node_label_column column of nodes dataframe for node labeling
#' @param main_targets vector of nodes serving as main targets
#' @keywords internal
#' @noRd
.validateVisualizeNetworks <- function(nodes, node_label_column, main_targets) {
    if (!node_label_column %in% colnames(nodes)) {
        stop("The specified node_label_column does not exist in the nodes dataframe.")
    }
    if (!"logFC" %in% colnames(nodes)) {
        stop("The 'logFC' column is missing from the nodes dataframe.")
    }
    if (length(main_targets) > 0 && !is.character(main_targets)) {
        stop("The main_targets parameter must be a character vector.")
    }
    if (!all(main_targets %in% nodes[,node_label_column])) {
        stop(paste0("Some main_targets do not match any values in the ", node_label_column, " column"))
    }
}

#' Add a legend using annotations
#' @param legend_items list of items and properties
#' @importFrom RCy3 addAnnotationShape addAnnotationText getNodePosition 
#' getNetworkCenter groupAnnotation
#' @keywords internal
#' @noRd
.addLegendInCytoscape <- function(legend_items) {
    if (!all(sapply(legend_items, function(item) all(c("color", "label") %in% names(item))))) {
        stop("Each legend item must contain 'color' and 'label' fields.")
    }
    
    # Starting position for the legend
    node_coordinates <- getNodePosition()
    node_coordinates$x_location <- as.numeric(node_coordinates$x_location)
    x_start <- max(node_coordinates$x_location) + 50  # Place the legend 50 units to the right of the rightmost node
    y_start <-  getNetworkCenter()$y
    box_size <- 20 # Size of the color boxes
    spacing <- 10  # Spacing between legend items
    legend_title <- "Legend"
    title_spacing <- 20  # Additional spacing below the title
    annotation_names <- c()
    
    title_name <- addAnnotationText(
        text = legend_title,
        x.pos = x_start, # Center the title with respect to the legend
        y.pos = y_start,  # Position above the items
        fontSize = 14,
        color = "black"
    )
    annotation_names <- c(annotation_names, title_name)
    
    # Adjust the starting y-coordinate for the legend items
    y_start <- y_start + title_spacing
    
    # Loop to add shapes and text for each legend item
    for (i in seq_along(legend_items)) {
        item <- legend_items[[i]]
        y_pos <- y_start + (i - 1) * (box_size + spacing)  # Adjust position for each item
        
        shape_name <- tryCatch({
            addAnnotationShape(
                type = "rectangle",
                x.pos = x_start,
                y.pos = y_pos,
                width = box_size,
                height = box_size,
                fillColor = item$color,
                borderColor = "black",
                borderThickness = 1
            )
        }, error = function(e) stop("Error adding annotation shape: ", e$message))
        
        text_name <- tryCatch({
            addAnnotationText(
                text = item$label,
                x.pos = x_start + box_size + spacing,
                y.pos = y_pos + box_size / 4,  # Center text vertically with the rectangle
                fontSize = 12,
                color = "black"
            )
        }, error = function(e) stop("Error adding annotation text: ", e$message))
        
        annotation_names <- c(annotation_names, shape_name, text_name)
    }
    # Group all annotations for now
    groupAnnotation(
        names = annotation_names
    )
}