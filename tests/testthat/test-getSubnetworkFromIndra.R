test_that("getSubnetworkFromIndra works correctly", {
    input <- data.table::fread(
        system.file("extdata/groupComparisonModel.csv", package = "MSstatsBioNet")
    )
    local_mocked_bindings(.callIndraCogexApi = function(x) {
        return(readRDS(system.file("extdata/indraResponse.rds", package = "MSstatsBioNet")))
    })
    suppressWarnings(subnetwork <- getSubnetworkFromIndra(input))
    expect_equal(nrow(subnetwork$nodes), 7)
    expect_equal(nrow(subnetwork$edges), 1)
})

test_that("getSubnetworkFromIndra with pvalue filter works correctly", {
    input <- data.table::fread(
        system.file("extdata/groupComparisonModel.csv", package = "MSstatsBioNet")
    )
    local_mocked_bindings(.callIndraCogexApi = function(x) {
        return(readRDS(system.file("extdata/indraResponse.rds", package = "MSstatsBioNet")))
    })
    suppressWarnings(
        subnetwork <- getSubnetworkFromIndra(input, pvalueCutoff = 0.45)
    )
    expect_equal(nrow(subnetwork$nodes), 6)
    expect_equal(nrow(subnetwork$edges), 1)
})

test_that("Exception is thrown for 400+ proteins in dataframe", {
    input_400 <- data.frame(
        Protein = paste0("Protein", 1:400),
        issue = NA,
        adj.pvalue = 0.05
    )
    expect_error(
        getSubnetworkFromIndra(input_400),
        "Invalid Input Error: INDRA query must contain less than 400 proteins.  Consider adding a p-value cutoff"
    )
})
