test_that(".addLegendInCytoscape works correctly", {
    legend_items <- list(
        list(color = "#ADD8E6", label = "label1"),
        list(color = "#EEEEEE", label = "label2"),
        list(color = "#FFA590", label = "label3")
    )
    
    mock_getNodePosition <- mock(data.frame(x_position = c("50", "51")))
    stub(
        .addLegendInCytoscape, "getNodePosition",
        mock_getNodePosition
    )
    
    mock_getNetworkCenter <- mock(list(x = 2, y = 5))
    stub(
        .addLegendInCytoscape, "getNetworkCenter",
        mock_getNetworkCenter
    )
    
    mock_addAnnotationShape <- mock()
    stub(
        .addLegendInCytoscape, "addAnnotationShape",
        mock_addAnnotationShape
    )
    
    mock_addAnnotationText <- mock()
    stub(
        .addLegendInCytoscape, "addAnnotationText",
        mock_addAnnotationText
    )
    
    mock_groupAnnotation <- mock()
    stub(
        .addLegendInCytoscape, "groupAnnotation",
        mock_groupAnnotation
    )
    
    mock_getAnnotationList <- mock(c("id1", "id2"))
    stub(
        .addLegendInCytoscape, "getAnnotationList",
        mock_getAnnotationList
    )
    
    expect_silent(.addLegendInCytoscape(legend_items))
    expect_called(mock_getNodePosition, 1)
    expect_called(mock_getNetworkCenter, 1)
    expect_called(mock_addAnnotationShape, 3)
    expect_called(mock_addAnnotationText, 3)
    expect_called(mock_groupAnnotation, 1)
    expect_called(mock_getAnnotationList, 1)
})