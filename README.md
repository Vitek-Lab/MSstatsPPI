# MSstatsBioNet

[![Codecov test coverage](https://codecov.io/github/Vitek-Lab/MSstatsBioNet/graph/badge.svg?token=SCPSPMTOEF)](https://codecov.io/github/Vitek-Lab/MSstatsBioNet)

This package provides a suite of functions to query various network databases, 
filter queries & results, and visualize networks.

## Installation Instructions

To install this package on bioconductor, run the following command:
```
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("MSstatsBioNet")
```

You can install the development version of this package through Github:

```
devtools::install_github("Vitek-Lab/MSstatsConvert", build_vignettes = TRUE)
```

## Databases Supported

- INDRA

## Filtering Options Supported

- P-Value Filter

## Visualization Options Supported

- Cytoscape Desktop 
