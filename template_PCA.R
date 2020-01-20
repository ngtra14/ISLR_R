# template PCA

# PCA example -------------------------------------------------------------

data("mtcars")
head(mtcars)

pca.out <- prcomp(mtcars[, c(1:7, 10, 11)],
                     center = TRUE,
                     scale. = TRUE)

biplot(pca.out, scale = 0)
pca.var <- pca.out$sdev^2

pve <- pca.var/sum(pca.var)
plot(pve, xlab = "Principal component", 
     ylab = "Proportion of variation explained",
     ylim = c(0, 1), 
     type = 'b')

plot(cumsum(pve), xlab = "Principal component", 
     ylab = "Accumulative Prop. of variation explained",
     ylim = c(0, 1), 
     type = 'b')
