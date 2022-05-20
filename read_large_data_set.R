
# Use regular readr -------------------------------------------------------
# data can be downloaded from
# https://www.uspto.gov/ip-policy/economic-research/research-datasets/artificial-intelligence-patent-dataset

library(readr)
ai_model_predictions <- read_tsv("ai_model_predictions.tsv", 
                                 n_max = 1000)
head(ai_model_predictions)


# Use bigmemory package ---------------------------------------------------
col_names <- colnames(ai_model_predictions)
library(bigmemory)
data_ai <- read.big.matrix("ai_model_predictions.tsv",
                           sep = "\t",
                           header = TRUE)

dim(data_ai)
summary(data_ai) 
head(data_ai) # has some NA generated
rm(data_ai)   # remove the object to save memory


# Use data.table  ---------------------------------------------------------
library(data.table)
data_ai_2 <- fread("ai_model_predictions.tsv", sep = "\t") # faster
dim(data_ai_2) # 13244037       31
summary(data_ai_2)
head(data_ai_2)
table(data_ai_2$analysis_phase)

# free the memory
rm(data_ai_2)
gc()
