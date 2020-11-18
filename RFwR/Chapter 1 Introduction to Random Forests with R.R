# Chapter 1 Introduction to Random Forests with R -------------------------

# 1.5.1 Running Example: Spam Detection
data("spam", package = "kernlab")
set.seed(9146301)
levels(spam$type) <- c("ok", "spam")
yTable <- table(spam$type)
indApp <- c(sample(1:yTable[2], yTable[2]/2),
              sample((yTable[2] + 1):nrow(spam), yTable[1]/2))
spamApp <- spam[indApp, ]
spamTest <- spam[-indApp, ]
head(spam)

# 1.5.2 Ozone Pollution
data("Ozone", package = "mlbench")
head(Ozone)

# 1.5.3 Genomic Data for a Vaccine Study
data("vac18", package = "mixOmics")
head(vac18)
# package 'mixOmics' is not available (for R version 4.0.0)

# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# 
# BiocManager::install("mixOmics")

# 1.5.4 Dust Pollution
data("jus", package = "VSURF")
head(jus)
