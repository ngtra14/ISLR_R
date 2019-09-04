# Chapter 12: ANOVA

# 12.1 - Categorical Predictors: t.test() vs. oneway.test() vs. lm --------

# y = B0 + B1 * x + e
# y = B0 + B1[x] + e
par(mfrow=c(1, 2))
data("warpbreaks")
head(warpbreaks)

boxplot(breaks ~ wool, data = warpbreaks)
boxplot(breaks ~ tension, data = warpbreaks)

# two sample t.test
t.test(breaks ~ wool, data = warpbreaks) # not significant
oneway.test(breaks ~ tension, data = warpbreaks)

# linear model
summary(lm(breaks ~ tension, data = warpbreaks))
# intercept L, then M, H based on L, differences


# 12.2 - Interpreting Output: summary(), anova(), aov(), and Tukey --------
# anova
anova(lm(breaks ~ tension, data = warpbreaks))
# aov
summary(aov(breaks ~ tension, data = warpbreaks))
# TukeyHSD
TukeyHSD(aov(breaks ~ tension, data = warpbreaks))


# 12.3 - Regression Assumptions in ANOVA  ---------------------------------
par(mfrow=c(2, 2))
plot(lm(breaks ~ tension, data = warpbreaks))

# assumptions not met

kruskal.test(breaks ~ wool, data = warpbreaks)
kruskal.test(breaks ~ tension, data = warpbreaks)


# 12.4 - Models with Multiple Predictors: Specification and Interp --------

# y ~ X1, X2
summary(lm(breaks ~ wool + tension, data = warpbreaks))
# intercept WoolA and tension L

anova(lm(breaks ~ wool + tension, data = warpbreaks))

TukeyHSD(aov(breaks ~ wool + tension, data = warpbreaks))


# 12.5 - Interactions Between Predictors ----------------------------------
summary(lm(breaks ~ wool + tension + wool:tension, data = warpbreaks))
m1 <- lm(breaks ~ wool*tension, data = warpbreaks)
summary(m1)$coeff
sum(summary(m1)$coeff[c(1,2,4,6), 1])

head(warpbreaks)
unique(warpbreaks[, 2:3])

# prediction
m1 <- lm(breaks ~ wool + tension + wool:tension, data = warpbreaks)
m1.pv <- unique(warpbreaks[, 2:3])
m1.pv

m1.pv$predicted <- predict(m1, newdata = unique(warpbreaks[, 2:3]))
m1.pv

# compare with group means
tapply(m1$fitted.values, list(warpbreaks$wool, warpbreaks$tension), mean)


# 12.6 - Visualizing Interactions Between Predictors  ---------------------
anova(lm(breaks ~ wool + tension + wool:tension, data = warpbreaks))
with(warpbreaks, interaction.plot(x.factor = tension, wool, response = breaks))


# 12.7 - TukeyHSD() and Interactions  -------------------------------------
TukeyHSD(aov(breaks ~ wool + tension + wool:tension, data = warpbreaks))


# 12.8 - The HSD.test() Function  -----------------------------------------
# no interactions allowed
library(agricolae) 
data("sweetpotato")
head(sweetpotato)

model <- aov(yield ~ virus, data = sweetpotato)
summary(model)

out <- HSD.test(model, "virus", group = TRUE, 
                main = "Yield difference with virus")

out

# barplot
bar.group(out$groups, ylim=c(0, 45), density=4, border="blue")

model2 <- lm(breaks ~ wool + tension + wool:tension, data = warpbreaks)
HSD.test(model2, trt = "tension", group = TRUE, main = "Wool x tension")$groups
