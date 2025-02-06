# Script to create groupComparisonModel.csv in extdata folder
library(MSstatsConvert)
input = data.table::fread(system.file(
    "extdata/msstats.csv",
    package = "MSstatsBioNet"
))
msstats_imported = FragPipetoMSstatsFormat(input, use_log_file = FALSE)
head(msstats_imported)

library(MSstats)
QuantData = dataProcess(msstats_imported, use_log_file = FALSE)
groupComparisonResult = groupComparison(contrast.matrix="pairwise", 
                                        data=QuantData, 
                                        use_log_file = FALSE)

library(MSstatsBioNet)
annotated_df = annotateProteinInfoFromIndra(groupComparisonResult$ComparisonResult, "Uniprot")
write.csv(annotated_df, "groupComparisonModel.csv", row.names = FALSE)
