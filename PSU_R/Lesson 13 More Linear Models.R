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


# 13.5 - A Note About Sums of Square in R  --------------------------------

# F-test - Type I SS by default - sequential - order based


# 13.6 - Resistant Regression ---------------------------------------------
library(MASS)
data("mtcars")
boxplot(hp ~ cyl, data = mtcars)
summary(lm(hp ~ factor(cyl), data = mtcars))
op <- par(mfrow=c(2, 2))
plot(lm(hp ~ factor(cyl), data = mtcars))

# resistant linear model
summary(rlm(hp ~ factor(cyl), data = mtcars))
## no p-values anymore
plot(lm(hp ~ factor(cyl), data = mtcars))


# 13.7 - Specifying Contrasts ---------------------------------------------

data("warpbreaks")
unique(warpbreaks[, 2:3])
summary(lm(breaks ~ wool*tension, data = warpbreaks))$coef
aggregate(warpbreaks$breaks, by=warpbreaks[, 2:3], mean)

m1 <- (lm(breaks~wool*tension, data = warpbreaks))
summary(m1)$coef
# AL = c(1,0,0,0,0,0)
# BL = c(1,1,0,0,0,0)
sum(summary(m1)$coef[,1]*c(0,-1,0,0,0,-1))

cont.mat <- matrix(c(0,-1,0,0,0,0, 0,-1,0,0,-1,0,0,-1,0,0,0,-1), 
                   byrow = TRUE, nrow = 3)
cont.mat

# multcomp
library(multcomp)
tests <- glht(m1, linfct=cont.mat)
summary(tests)

## need to read the tutorial again


# 13.8 - More Complex Designs  --------------------------------------------
# example of a split-plot design
Rye <- read.csv("Data/RyeSplitPlot.csv", comm="#")
head(Rye)

# wrong model
summary(aov(RyeDM ~ Rep + Plant + Tdate, data = Rye))

# split-plot model
summary(aov(RyeDM ~ Rep + Plant + Tdate + Error(Rep/Plant), data = Rye))
# plant not significant
# Tdate significant
# Plant is nested in Rep

