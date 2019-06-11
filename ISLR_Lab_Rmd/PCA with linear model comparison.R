# PCA with housing data
library(MASS)
head(Boston)

#PCA
pca.house <- prcomp(scale(Boston[, -14]))
pca.house$x
scores.house <- pca.house$x

# linear model with PCA
lm.pca <- lm(Boston$medv ~ scores.house[, 1:3])
summary(lm.pca)

# linear model with original data
lm.full <- lm(medv ~., data = Boston)
summary(lm.full)

# plot for assumption check
plot(lm.pca)
plot(lm.full)
