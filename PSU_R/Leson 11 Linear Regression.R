# Leson 11: Linear Regression
# https://dev.stat.vmhost.psu.edu/stat485/lesson/11/11.1
# 11.1 - The Regression Model and Interpreting the Output ------------------

library(MASS)
data("mammals")
head(mammals)

# linear model
model1 <- lm(brain ~ body, data = mammals)
summary(model1)


# 11.2 - Meeting Regression Assumptions - Normality of Residuals ----------
# check the residuals
par(mfrow=c(2,2))
plot(model1)
dev.off()

# plot the data
with(mammals, plot(body, brain, log="xy"))
model2 <- lm(log(brain) ~ log(body), data = mammals)
summary(model2)

# check residuals again
par(mfrow=c(2,2))
plot(model2)
dev.off()

par(mfrow=c(1,2))
plot(density(model1$residuals), main = "model1")
plot(density(model2$residuals), main = "model2")
