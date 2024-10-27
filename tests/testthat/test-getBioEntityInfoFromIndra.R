# Test .callGetUniprotIdsFromUniprotMnemonicIdsApi
test_that(".callGetUniprotIdsFromUniprotMnemonicIdsApi works correctly", {
    uniprotMnemonicIds <- list("CLH1_HUMAN")
    result <- .callGetUniprotIdsFromUniprotMnemonicIdsApi(uniprotMnemonicIds)
    expect_type(result, "list")
    expect_true(length(result) == 1)
    expected_value <- list(CLH1_HUMAN = "Q00610")
    expect_equal(result, expected_value)
})

# Test .callGetHgncIdsFromUniprotIdsApi
test_that(".callGetHgncIdsFromUniprotIdsApi works correctly", {
    uniprotIds <- list("Q00610")
    result <- .callGetHgncIdsFromUniprotIdsApi(uniprotIds)
    expect_type(result, "list")
    expect_true(length(result) == 1)
    expected_value <- list("Q00610" = "2092")
    expect_equal(result, expected_value)
})

# Test .callGetHgncNamesFromHgncIdsApi
test_that(".callGetHgncNamesFromHgncIdsApi works correctly", {
    hgncIds <- list("2092")
    result <- .callGetHgncNamesFromHgncIdsApi(hgncIds)
    expect_type(result, "list")
    expect_true(length(result) == 1)
    expected_value <- list("2092" = "CLTC")
    expect_equal(result, expected_value)
})

# Test .callIsKinaseApi
test_that(".callIsKinaseApi works correctly", {
    kinaseGenes <- list("CHEK1")
    result <- .callIsKinaseApi(kinaseGenes)
    expect_type(result, "list")
    expect_true(length(result) == 1)
    expected_value <- list("CHEK1" = TRUE)
    expect_equal(result, expected_value)
})

# Test .callIsPhosphataseApi
test_that(".callIsPhosphataseApi works correctly", {
    phosphataseGenes <- list("MTM1")
    result <- .callIsPhosphataseApi(phosphataseGenes)
    expect_type(result, "list")
    expect_true(length(result) == 1)
    expected_value <- list("MTM1" = TRUE)
    expect_equal(result, expected_value)
})

# Test .callIsTranscriptionFactorApi
test_that(".callIsTranscriptionFactorApi works correctly", {
    transcriptionFactorGenes <- list("STAT1")
    result <- .callIsTranscriptionFactorApi(transcriptionFactorGenes)
    expect_type(result, "list")
    expect_true(length(result) == 1)
    expected_value <- list("STAT1" = TRUE)
    expect_equal(result, expected_value)
})