
# 11.4 - Testing Hypotheses About Regression Parameters (I)  --------------

beans <- read.csv("Data/BeansData.csv", comm="#")
head(beans)
plot(RtDM ~ ShtDM, data = beans, col=P.lev)
m1 <- lm(RtDM ~ ShtDM, data = beans)
summary(m1)

# check residuals
plot(m1$residuals ~ m1$fitted.values)

plot(m1, which = 2)


# 11.5 - Testing Hypotheses About Regression Parameters (II)  -------------
# is the slope = 1 ?
B1 <- summary(m1)$coeff[2, 1:2]
B1

# calculate t
t <- (B1[1] - 1)/B1[2]
t
pt(t, df=22, lower.tail = TRUE) # one side test
pt(t, df=22, lower.tail = TRUE) * 2 # two sides test


# 11.6 - Testing Hypotheses About Regression Parameters (III)  ------------

# 1 * beans$ShtDM
with(beans, RtDM - 1 * ShtDM)
summary(lm(RtDM - 1*ShtDM ~ ShtDM, data = beans))

# alterntative method
summary(lm(RtDM ~ ShtDM + offset(1 * ShtDM), data = beans))

# abline function
plot(RtDM ~ ShtDM, data = beans, xlim=c(0.5, 4), ylim=c(0.4, 2.4))
abline(m1, lty=2)


# 11.7 - Predicting Values and Confidence Intervals from Regressio --------

new.vals <- c(2.0, 2.1, 2.2, 3.5)
predict(m1, newdata = data.frame(ShtDM = new.vals))
preds <- predict(m1, newdata = data.frame(ShtDM = new.vals))
points(new.vals, preds, col="red", pch=24)
ci <- predict(m1, newdata = data.frame(ShtDM = sort(beans$ShtDM)), 
              level = 0.95, interval = "confidence")
head(ci)
lines(sort(beans$ShtDM), ci[, 2], col="red")
lines(sort(beans$ShtDM), ci[, 3], col="red")


# 11.8 - Prediction Intervals from Regressions  ---------------------------
pri <- predict(m1, newdata = data.frame(ShtDM = sort(beans$ShtDM)), 
              level = 0.95, interval = "prediction")
head(pri)
lines(sort(beans$ShtDM), pri[, 2], col="red", lty=2)
lines(sort(beans$ShtDM), pri[, 3], col="red", lty=2)
