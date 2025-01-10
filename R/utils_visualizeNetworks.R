#' Add a legend using annotations
#' @param legend_items list of items and properties
#' @importFrom RCy3 addAnnotationShape addAnnotationText getNodePosition 
#' getNetworkCenter groupAnnotation
#' @keywords internal
#' @noRd
.addLegendInCytoscape <- function(legend_items) {
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
        
        # Add a colored rectangle for the legend
        shape_name <- addAnnotationShape(
            type = "rectangle",
            x.pos = x_start,
            y.pos = y_pos,
            width = box_size,
            height = box_size,
            fillColor = item$color,
            borderColor = "black",
            borderThickness = 1
        )
        
        # Add corresponding text label
        text_name <- addAnnotationText(
            text = item$label,
            x.pos = x_start + box_size + spacing,
            y.pos = y_pos + box_size / 4,  # Center text vertically with the rectangle
            fontSize = 12,
            color = "black"
        )
        
        annotation_names <- c(annotation_names, shape_name, text_name)
    }
    # Group all annotations for now
    groupAnnotation(
        names = annotation_names
    )
}