# Script to create groupComparisonModel.csv in extdata folder
library(MSstatsConvert)
input = system.file("tinytest/raw_data/Metamorpheus/AllQuantifiedPeaks.tsv", 
                    package = "MSstatsConvert")
input = data.table::fread(input)
annot = system.file("tinytest/raw_data/Metamorpheus/Annotation.tsv", 
                    package = "MSstatsConvert")
annot = data.table::fread(annot)
msstats_imported = MetamorpheusToMSstatsFormat(input, annotation = annot)
head(msstats_imported)

library(MSstats)
QuantData = dataProcess(msstats_imported, use_log_file = FALSE)
groupComparisonResult = groupComparison(contrast.matrix="pairwise", 
                                        data=QuantData, 
                                        use_log_file = FALSE)

uniprot_to_hgnc_mapping = c(
    "B9A064" = "38476",
    "O00391" = "9756",
    "O14818" = "9536",
    "O43598" = "21218",
    "O43707" = "166",
    "P16050" = "11390",
    "P84243" = "4765"
)
groupComparisonResult$ComparisonResult$HgncId = uniprot_to_hgnc_mapping[
    groupComparisonResult$ComparisonResult$Protein
]

write.csv(groupComparisonResult$ComparisonResult, "groupComparison.csv", row.names = FALSE)