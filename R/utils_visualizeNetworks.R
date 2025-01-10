#' Add a legend using annotations
#' @param legend_items list of items and properties
#' @importFrom RCy3 addAnnotationShape addAnnotationText getNodePosition 
#' getNetworkCenter groupAnnotation getAnnotationList
#' @keywords internal
#' @noRd
.addLegendInCytoscape <- function(legend_items) {
    # Starting position for the legend
    node_coordinates = getNodePosition()
    node_coordinates$x_location = as.numeric(node_coordinates$x_location)
    x_start <- max(node_coordinates$x_location) + 50  # Place the legend 50 units to the right of the rightmost node
    y_start <-  getNetworkCenter()$y
    box_size <- 20 # Size of the color boxes
    spacing <- 10  # Spacing between legend items
    
    # Loop to add shapes and text for each legend item
    for (i in seq_along(legend_items)) {
        item <- legend_items[[i]]
        y_pos <- y_start - (i - 1) * (box_size + spacing)  # Adjust position for each item
        
        # Add a colored rectangle for the legend
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
        
        # Add corresponding text label
        addAnnotationText(
            text = item$label,
            x.pos = x_start + box_size + spacing,
            y.pos = y_pos + box_size / 2,  # Center text vertically with the rectangle
            fontSize = 12,
            color = "black"
        )
    }
    # Group all annotations for now
    groupAnnotation(sapply(getAnnotationList(), '[[', 'uuid'))
}