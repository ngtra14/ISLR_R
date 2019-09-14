# Chapter 19: Non-linear Models

# 19.1 - A Brief Definition of the Logistic Model -------------------------

# logistic curve
curve(2/(1 + exp(-1*(x-4))), from = -2, to = 10, ylab = "")
legend("bottomright", inset = 0.02, 
       c(expression(italic(L)==2), expression(italic(x)[0]==4),
        expression(italic(k)==1)), bty="n")

# logit , ln(odds) = p/(1 - p)

# 19.2 - Fitting a Logistic Model -----------------------------------------

library(MASS)
data("menarche")
head(menarche)
plot(Menarche/Total ~ Age, data = menarche, 
     ylab = "Proportion passed menarche")

# fit the model
lr1 <- glm(cbind(Menarche, Total-Menarche) ~ Age, data = menarche, family = "binomial")
summary(lr1)

with(data=menarche, cbind(Menarche, Total-Menarche))


# 19.3 - Interpreting the Coefficients of the Logistic Model I ------------

## slope for age
signif(exp(summary(lr1)$coef[2,1]), 4)

## intecept - where the p = 0.5
-summary(lr1)$coef[1,1]/summary(lr1)$coef[2,1]


# 19.4 - Interpreting the Coefficients of the Logistic Model II -----------
## null deviance vs residual deviance
abline(v=13.006, lty=2)
abline(h=0.5, lty=2)
lines(menarche$Age, lr1$fitted.values, type = "l", col="red")
par(mfrow=c(2, 2))
plot(lr1)
dev.off()


# 19.5 - Logistic Regression on Individual Data I -------------------------
## eruption data
data("faithful")
head(faithful)

hist(faithful$eruptions)
faithful$length<-cut(faithful$eruptions,breaks = c(0,3,6),labels = c("S","L"))
with(faithful,plot(waiting, jitter(as.numeric(length)-1,amount = 0.04),
                   ylab="",yaxt="n"))
axis(side=2,at=0:1,labels=c("Short","Long"))

lr2 <- glm(length ~ waiting, family = "binomial", data = faithful)

# slope
signif(exp(summary(lr2)$coef[2,1]), 3)

# odds at 63 mins waiting time
exp(summary(lr2)$coef[1,1] + 63*summary(lr2)$coef[2,1])

## intercept
-summary(lr2)$coef[1,1]/summary(lr2)$coef[2,1]


# 19.6 - Logistic Regression on Individual Data II ------------------------

with(faithful,plot(waiting, jitter(as.numeric(length)-1,amount = 0.04),ylab="",yaxt="n"))
axis(side=2,at=0:1,labels=c("Short","Long"))
lines(cbind(faithful$waiting,lr2$fitted)[order(faithful$waiting),],col="red")
abline(h=0.5,v=66.113,lty=3)

# plot residual - some residuals
plot(lr2, which=1)
