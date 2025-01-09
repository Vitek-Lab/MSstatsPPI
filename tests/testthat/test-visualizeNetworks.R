test_that("visualizeNetworks works correctly", {
    input <- readRDS(system.file("extdata/subnetwork.rds",
        package = "MSstatsBioNet"
    ))

    mock_interactive <- mock(TRUE)
    stub(
        visualizeNetworks, "interactive",
        mock_interactive
    )
    mock_createNetworkFromDataFrames <- mock()
    stub(
        visualizeNetworks, "createNetworkFromDataFrames",
        mock_createNetworkFromDataFrames
    )
    mock_mapVisualProperty <- mock()
    stub(
        visualizeNetworks, "mapVisualProperty",
        mock_mapVisualProperty
    )
    mock_createVisualStyle <- mock()
    stub(
        visualizeNetworks, "createVisualStyle",
        mock_createVisualStyle
    )
    mock_setVisualStyle <- mock()
    stub(
        visualizeNetworks, "setVisualStyle",
        mock_setVisualStyle
    )
    mock_layoutNetwork <- mock()
    stub(
        visualizeNetworks, "layoutNetwork",
        mock_layoutNetwork
    )

    expect_silent(visualizeNetworks(input$nodes, input$edges))
    expect_called(mock_createNetworkFromDataFrames, 1)
    expect_called(mock_mapVisualProperty, 2)
    expect_called(mock_createVisualStyle, 1)
    expect_called(mock_setVisualStyle, 1)
    expect_called(mock_layoutNetwork, 1)
})


test_that("visualizeNetworks with p-value and logFC constraints works", {
    input <- readRDS(system.file("extdata/subnetwork.rds",
        package = "MSstatsBioNet"
    ))

    mock_interactive <- mock(TRUE)
    stub(
        visualizeNetworks, "interactive",
        mock_interactive
    )
    mock_createNetworkFromDataFrames <- mock()
    stub(
        visualizeNetworks, "createNetworkFromDataFrames",
        mock_createNetworkFromDataFrames
    )
    mock_mapVisualProperty <- mock()
    stub(
        visualizeNetworks, "mapVisualProperty",
        mock_mapVisualProperty
    )
    mock_createVisualStyle <- mock()
    stub(
        visualizeNetworks, "createVisualStyle",
        mock_createVisualStyle
    )
    mock_setVisualStyle <- mock()
    stub(
        visualizeNetworks, "setVisualStyle",
        mock_setVisualStyle
    )
    mock_layoutNetwork <- mock()
    stub(
        visualizeNetworks, "layoutNetwork",
        mock_layoutNetwork
    )

    expect_silent(visualizeNetworks(input$nodes, input$edges,
        pvalueCutoff = 0.01, logfcCutoff = 2.5
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

test_that("visualizeNetworks returns warning for non-interactive calls", {
    input <- readRDS(system.file("extdata/subnetwork.rds",
        package = "MSstatsBioNet"
    ))

    mock_interactive <- mock(FALSE)
    stub(
        visualizeNetworks, "interactive",
        mock_interactive
    )

    expect_warning(visualizeNetworks(input$nodes, input$edges,
        pvalueCutoff = 0.01, logfcCutoff = 2.5
    ))
})
