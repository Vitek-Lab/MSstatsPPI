test_that(".addLegendInCytoscape works correctly", {
    legend_items <- list(
        list(color = "#ADD8E6", label = "label1"),
        list(color = "#EEEEEE", label = "label2"),
        list(color = "#FFA590", label = "label3")
    )
    
    mock_getNodePosition <- mock(data.frame(x_location = c("50", "51")))
    stub(
        .addLegendInCytoscape, "getNodePosition",
        mock_getNodePosition
    )
    
    mock_getNetworkCenter <- mock(list(x = 2, y = 5))
    stub(
        .addLegendInCytoscape, "getNetworkCenter",
        mock_getNetworkCenter
    )
    
    mock_addAnnotationShape <- mock("id-12345", cycle = TRUE)
    stub(
        .addLegendInCytoscape, "addAnnotationShape",
        mock_addAnnotationShape
    )
    
    mock_addAnnotationText <- mock("id-12345", cycle = TRUE)
    stub(
        .addLegendInCytoscape, "addAnnotationText",
        mock_addAnnotationText
    )
    
    mock_groupAnnotation <- mock()
    stub(
        .addLegendInCytoscape, "groupAnnotation",
        mock_groupAnnotation
    )
    
    expect_silent(.addLegendInCytoscape(legend_items))
    expect_called(mock_getNodePosition, 1)
    expect_called(mock_getNetworkCenter, 1)
    expect_called(mock_addAnnotationShape, 3)
    expect_called(mock_addAnnotationText, 4)
    expect_called(mock_groupAnnotation, 1)
})

test_that(".addLegendInCytoscape throws validation error without color", {
    legend_items <- list(
        list(label = "label1"),
        list(label = "label2"),
        list(label = "label3")
    )
    
    expect_error(.addLegendInCytoscape(legend_items),
                 "Each legend item must contain 'color' and 'label' fields.")
})

test_that(".addLegendInCytoscape throws validation error without label", {
    legend_items <- list(
        list(color = "#ADD8E6"),
        list(color = "#EEEEEE"),
        list(color = "#FFA590")
    )
    
    expect_error(.addLegendInCytoscape(legend_items),
                 "Each legend item must contain 'color' and 'label' fields.")
})