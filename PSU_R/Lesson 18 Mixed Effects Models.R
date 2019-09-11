# Chapter 18: Mixed Effects Models

# 18.1 - What is a Mixed Effects Model? -----------------------------------
## random effects + error
## book : Mixed effects models and extensions in ecology with R


# 18.2 - Repeated Measures Done the Wrong Way -----------------------------

library(nlme)
data("Machines")
head(Machines)
?Machines

# model 1
fm1 <- lm(score ~ Machine, data = Machines)
summary(fm1)

# check residuals
op <- par(mfrow=c(1,2))
plot(fm1, which = 1:2)
par(op)

oneway.test(score ~ Worker, data = Machines)

# model 2, with interaction terms
fm2 <- lm(score ~ Machine*Worker, data = Machines)
anova(fm2)
op <- par(mfrow=c(1,2))
plot(fm2, which = 1:2)
# residuals better, not ideal

# 18.3 - Repeated Measures Using Mixed Effects I ---------------------------
fm3 <- lme(score ~ Machine, random= ~1|Worker, data = Machines)
anova(fm3)
plot(fm3)
summary(fm3) # fixed effect vs random effects


# 18.4 - Repeated Measures Using Mixed Effects II ------------------------
fm4 <- lme(score ~ Machine, random= ~1 + Machine|Worker, data = Machines)
plot(fm4)
qqnorm(fm4, abline = c(0, 1))
anova(fm4)
summary(fm4)


# 18.5 - Split-plot Using Mixed Effects -----------------------------------
Rye <- read.csv("Data/RyeSplitPlot.csv", comm="#")

# split-plot model - aov()
summary(aov(RyeDM ~ Rep + Plant + Tdate + Error(Rep/Plant), data = Rye))

# lme() model
fm5 <- lme(RyeDM ~ Plant + Tdate, random = ~1|Rep/Plant, data = Rye)
anova(fm5)
# you can plot in lme(), but aov() you cannot do it

plot(fm5)
summary(fm5)


# 18.6 - Using anova() to Compare Models ----------------------------------
fm6 <- lme(RyeDM ~ Plant*Tdate, random = ~1|Rep/Plant, data = Rye)
anova(fm6)

fm5.ML <- lme(RyeDM ~ Plant + Tdate, random = ~1|Rep/Plant, 
              data = Rye, method = "ML")
fm6.ML <- lme(RyeDM ~ Plant*Tdate, random = ~1|Rep/Plant, 
              data = Rye, method = "ML")
anova(fm5.ML, fm6.ML)

# na values by default
fm7 <- lme(RyeDM ~ Plant + Tdate, random = ~1|Rep/Plant, 
           data = Rye, na.action = "na.omit" )


