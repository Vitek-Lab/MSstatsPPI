test_that("visualizeSubnetwork works correctly", {
    input <- readRDS(system.file("processed_data/subnetwork.rds",
        package = "MSstatsBioNet"
    ))

    mock_createNetworkFromDataFrames <- mock()
    stub(
        visualizeSubnetwork, "createNetworkFromDataFrames",
        mock_createNetworkFromDataFrames
    )
    mock_mapVisualProperty <- mock()
    stub(
        visualizeSubnetwork, "mapVisualProperty",
        mock_mapVisualProperty
    )
    mock_createVisualStyle <- mock()
    stub(
        visualizeSubnetwork, "createVisualStyle",
        mock_createVisualStyle
    )
    mock_setVisualStyle <- mock()
    stub(
        visualizeSubnetwork, "setVisualStyle",
        mock_setVisualStyle
    )

    expect_silent(visualizeSubnetwork(input$nodes, input$edges))
    expect_called(mock_createNetworkFromDataFrames, 1)
    expect_called(mock_mapVisualProperty, 2)
    expect_called(mock_createVisualStyle, 1)
    expect_called(mock_setVisualStyle, 1)
})


test_that("visualizeSubnetwork with p-value and logFC constraints works", {
    input <- readRDS(system.file("processed_data/subnetwork.rds",
        package = "MSstatsBioNet"
    ))

    mock_createNetworkFromDataFrames <- mock()
    stub(
        visualizeSubnetwork, "createNetworkFromDataFrames",
        mock_createNetworkFromDataFrames
    )
    mock_mapVisualProperty <- mock()
    stub(
        visualizeSubnetwork, "mapVisualProperty",
        mock_mapVisualProperty
    )
    mock_createVisualStyle <- mock()
    stub(
        visualizeSubnetwork, "createVisualStyle",
        mock_createVisualStyle
    )
    mock_setVisualStyle <- mock()
    stub(
        visualizeSubnetwork, "setVisualStyle",
        mock_setVisualStyle
    )

    expect_silent(visualizeSubnetwork(input$nodes, input$edges,
        pvalue_cutoff = 0.01, logfc_cutoff = 2.5
    ))
    expect_called(mock_createNetworkFromDataFrames, 1)
    calls <- mock_args(mock_createNetworkFromDataFrames)
    nodes <- calls[[1]][[1]]
    edges <- calls[[1]][[2]]
    expect_equal(edges, input$edges)
    expect_equal(nodes[input$nodes$id == "BRD2_HUMAN", ]$logFC_color, 0)
    expect_equal(
        nodes[input$nodes$id == "BRD3_HUMAN", ]$logFC_color, 3.33342794
    )
    expect_equal(nodes[input$nodes$id == "BRD4_HUMAN", ]$logFC_color, 0)
})
