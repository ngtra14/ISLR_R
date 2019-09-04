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


# 11.3 - Meeting Regression Assumptions - Homogeneity of Residuals --------

Weights <- read.csv("Data/WeightData.csv", comm="#")
hist(Weights$age, main = "")
plot(weight ~ age, data = Weights)
par(mfrow=c(1, 2))
plot(log(weight) ~ age, data = Weights)
abline(v=7, lty=2)

plot(log(weight) ~ age, data = subset(Weights, age > 7))

m1 <- lm(weight~age, data = Weights)
m2 <- lm(log(weight) ~ age, data = subset(Weights, age > 7))
par(mfrow=c(2, 2))
plot(m1$fitted.values, m1$residuals)
plot(density(m1$residuals))
plot(m2$fitted.values, m2$residuals)
plot(density(m2$residuals))
