# Chapter 2 CART ----------------------------------------------------------

# 2.6.1 Predicting Ozone Concentration
library("rpart")
data("Ozone", package = "mlbench")
OzTreeDef <- rpart(V4 ~ ., data = Ozone)
plot(OzTreeDef)
text(OzTreeDef, xpd = TRUE, cex = 0.9)

# maximal tree
set.seed(727325)
OzTreeMax <- rpart(V4 ~ ., data = Ozone, minsplit = 2, cp = 0)
plotcp(OzTreeMax)

OzIndcpOpt <- which.min(OzTreeMax$cptable[, 4])
OzcpOpt <- OzTreeMax$cptable[OzIndcpOpt, 1]
OzTreeOpt <- prune(OzTreeMax, cp = OzcpOpt)
plot(OzTreeOpt)
text(OzTreeOpt, xpd = TRUE)

# 2.6.2 Analyzing Genomic Data
library(rpart)
data("vac18", package = "mixOmics")
VAC18 <- data.frame(vac18$genes, stimu = vac18$stimulation)

VacTreeDef <- rpart(stimu ~., data = VAC18)
VacTreeDef

plot(VacTreeDef)
text(VacTreeDef, use.n = TRUE, xpd = TRUE)

# max tree
set.seed(788182)
VacTreeMax <- rpart(stimu ~., data = VAC18, minsplit = 2, cp = 0)
plot(VacTreeMax)
text(VacTreeMax, use.n = TRUE, xpd = TRUE)

# prune the tree
set.seed(413745)
VacTreeMaxLoo <- rpart(stimu ~., data = VAC18, minsplit = 2 ,cp = 0, xval = nrow(VAC18))
par(mfrow = c(1, 2))
plotcp(VacTreeMax)
plotcp(VacTreeMaxLoo)

# optimal tree
VacIndcpOpt <- which.min(VacTreeMaxLoo$cptable[, 4])
VaccpOpt <- VacTreeMaxLoo$cptable[VacIndcpOpt, 1]
VacTreeOpt <- prune(VacTreeMaxLoo, cp = VaccpOpt)
dev.off()
plot(VacTreeOpt)
text(VacTreeOpt, use.n = TRUE, xpd = TRUE)
