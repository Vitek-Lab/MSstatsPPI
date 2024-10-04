test_that("getSubnetworkFromIndra works correctly", {
    input <- data.table::fread(
        system.file("extdata/groupComparisonModel.csv", package = "MSstatsBioNet")
    )
    local_mocked_bindings(.callIndraCogexApi = function(x) {
        return(readRDS(system.file("extdata/indraResponse.rds", package = "MSstatsBioNet")))
    })
    suppressWarnings(subnetwork <- getSubnetworkFromIndra(input))
    expect_equal(nrow(subnetwork$nodes), 7)
    expect_equal(nrow(subnetwork$edges), 2)
})

test_that("getSubnetworkFromIndra with pvalue filter works correctly", {
    input <- data.table::fread(
        system.file("extdata/groupComparisonModel.csv", package = "MSstatsBioNet")
    )
    local_mocked_bindings(.callIndraCogexApi = function(x) {
        return(readRDS(system.file("extdata/indraResponse.rds", package = "MSstatsBioNet")))
    })
    suppressWarnings(
        subnetwork <- getSubnetworkFromIndra(input, pvalueCutoff = 0.45))
    expect_equal(nrow(subnetwork$nodes), 6)
    expect_equal(nrow(subnetwork$edges), 2)
})
