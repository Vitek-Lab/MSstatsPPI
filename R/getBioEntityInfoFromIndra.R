
#' Call API to get UniProt IDs from UniProt mnemonic IDs
#' @param uniprotMnemonicIds list of UniProt mnemonic ids
#' @return list of UniProt IDs
#' @importFrom jsonlite toJSON
#' @importFrom httr POST add_headers content
#' @keywords internal
#' @noRd
.callGetUniprotIdsFromUniprotMnemonicIdsApi <- function(uniprotMnemonicIds) {
    apiUrl <- "http://localhost:5000/api/get_uniprot_ids_from_uniprot_mnemonic_ids"

    requestBody <- list(uniprot_mnemonic_ids = uniprotMnemonicIds)
    requestBody <- jsonlite::toJSON(requestBody, auto_unbox = TRUE)

    res <- POST(
        apiUrl,
        body = requestBody,
        add_headers("Content-Type" = "application/json"),
        encode = "raw"
    )
    res <- content(res)
    return(res)
}

#' Call API to get HGNC IDs from UniProt IDs
#' @param uniprotIds list of UniProt IDs
#' @return list of HGNC IDs
#' @importFrom jsonlite toJSON
#' @importFrom httr POST add_headers content
#' @keywords internal
#' @noRd
.callGetHgncIdsFromUniprotIdsApi <- function(uniprotIds) {
    apiUrl <- "http://localhost:5000/api/get_hgnc_ids_from_uniprot_ids"

    requestBody <- list(uniprot_ids = uniprotIds)
    requestBody <- jsonlite::toJSON(requestBody, auto_unbox = TRUE)

    res <- POST(
        apiUrl,
        body = requestBody,
        add_headers("Content-Type" = "application/json"),
        encode = "raw"
    )
    res <- content(res)
    return(res)
}

#' Call API to get HGNC names from HGNC IDs
#' @param hgncIds list of HGNC IDs
#' @return list of HGNC names
#' @importFrom jsonlite toJSON
#' @importFrom httr POST add_headers content
#' @keywords internal
#' @noRd
.callGetHgncNamesFromHgncIdsApi <- function(hgncIds) {
    apiUrl <- "http://localhost:5000/api/get_hgnc_names_from_hgnc_ids"

    requestBody <- list(hgnc_ids = hgncIds)
    requestBody <- jsonlite::toJSON(requestBody, auto_unbox = TRUE)

    res <- POST(
        apiUrl,
        body = requestBody,
        add_headers("Content-Type" = "application/json"),
        encode = "raw"
    )
    res <- content(res)
    return(res)
}

#' Call API to check if genes are kinases
#' @param genes list of gene names
#' @return list indicating if genes are kinases
#' @importFrom jsonlite toJSON
#' @importFrom httr POST add_headers content
#' @keywords internal
#' @noRd
.callIsKinaseApi <- function(genes) {
    apiUrl <- "http://localhost:5000/api/is_kinase"

    requestBody <- list(genes = genes)
    requestBody <- jsonlite::toJSON(requestBody, auto_unbox = TRUE)

    res <- POST(
        apiUrl,
        body = requestBody,
        add_headers("Content-Type" = "application/json"),
        encode = "raw"
    )
    res <- content(res)
    return(res)
}

#' Call API to check if genes are phosphatases
#' @param genes list of gene names
#' @return list indicating if genes are phosphatases
#' @importFrom jsonlite toJSON
#' @importFrom httr POST add_headers content
#' @keywords internal
#' @noRd
.callIsPhosphataseApi <- function(genes) {
    apiUrl <- "http://localhost:5000/api/is_phosphatase"

    requestBody <- list(genes = genes)
    requestBody <- jsonlite::toJSON(requestBody, auto_unbox = TRUE)

    res <- POST(
        apiUrl,
        body = requestBody,
        add_headers("Content-Type" = "application/json"),
        encode = "raw"
    )
    res <- content(res)
    return(res)
}

#' Call API to check if genes are transcription factors
#' @param genes list of gene names
#' @return list indicating if genes are transcription factors
#' @importFrom jsonlite toJSON
#' @importFrom httr POST add_headers content
#' @keywords internal
#' @noRd
.callIsTranscriptionFactorApi <- function(genes) {
    apiUrl <- "http://localhost:5000/api/is_transcription_factor"

    requestBody <- list(genes = genes)
    requestBody <- jsonlite::toJSON(requestBody, auto_unbox = TRUE)

    res <- POST(
        apiUrl,
        body = requestBody,
        add_headers("Content-Type" = "application/json"),
        encode = "raw"
    )
    res <- content(res)
    return(res)
}
