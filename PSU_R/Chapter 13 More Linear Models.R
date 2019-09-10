# Chapter 13: More Linear Models

# 13.1 - Multiple Regression I  -------------------------------------------

data("stackloss")
summary(stackloss)
summary(lm(stack.loss ~ Water.Temp, data = stackloss))$coef
summary(lm(stack.loss ~ Air.Flow, data = stackloss))$coef
summary(lm(stack.loss ~ Acid.Conc., data = stackloss))$coef

# two predictors
summary(lm(stack.loss ~ Air.Flow + Water.Temp, data = stackloss))$coef


# 13.2 - Multiple Regression II -------------------------------------------

# interaction term
summary(lm(stack.loss ~ Air.Flow*Water.Temp, data = stackloss))$coef
summary(lm(stack.loss ~ Air.Flow*Water.Temp*Acid.Conc., 
           data = stackloss))$coef
# kept only second order interaction term
summary(lm(stack.loss ~ (Air.Flow + Water.Temp + Acid.Conc.)^2, 
           data = stackloss))$coef
## non of them significant anymore !


# 13.3 - ANCOVA I  --------------------------------------------------------

## numerical + categorical variables
Rye <- read.csv("Data/Rye-ANCOVA-2008.csv", comm="#")
head(Rye)

# single predictor
summary(lm(RyeDMg ~ Tdate, data = Rye))
summary(lm(RyeDMg ~ Plant, data = Rye))

# ANCOVA
summary(lm(RyeDMg ~ Tdate + Plant, data = Rye))

# interaction term
model1 <- lm(RyeDMg ~ Tdate + Plant + Tdate:Plant, data = Rye)
summary(model1) # two slopes are not different


# 13.4 - ANCOVA II --------------------------------------------------------
plot(RyeDMg ~ Tdate, data = Rye, col=Plant)
mcf <- summary(model1)$coef[, 1]
mcf

abline(a = mcf[1], b = mcf[2])
abline(a = mcf[1] + mcf[3], b = mcf[2] + mcf[4], col="red")

