# Chapter 16: Visualizing Data II - Errorbars and Ribbons

# 16.1 - Errorbars - Introducing arrows() ---------------------------------
op <- par(mar=c(4,4,0.5,0.5))
plot(1:10, 1:10, type = "n")
arrows(x0=1, y0=6, x1=1, y1=4)
arrows(1.5, 8, 1.5, 2, code = 3) # both direction
arrows(2, 6, 2, 4, code = 3, angle = 90) # error bar
arrows(2.5, 6.5, 2.5, 4.5, code = 3, angle = 90, length = 0.1)

# plot vectors of arrows
x <- c(3, 5, 7)
y <- c(5, 4.5, 6)
z <- c(2, 1, 1.5)
arrows(x, y-z, x, y+z, code = 3, angle = 90, length = 0.1) # 3 error bars

RyeMeans <- read.delim("Data/Rye ANCOVA.txt", comment="#")
head(RyeMeans)


# 16.2 - Custom Axis Formatting -------------------------------------------

RyeMeans$Term.DOY # not in order
RyeMeans<-RyeMeans[order(RyeMeans$Term.DOY),] # sort the data by Term.DOY

# empty plot first
range(RyeMeans$MeanDM+RyeMeans$DMse) # ~110 to ~1100)
range(RyeMeans$Term.DOY) # find x range (~115-160)
levels(RyeMeans$YrPd)

op<-par(mar=c(5,5,3,1)) # save the par settings and set margin settings.
with(subset(RyeMeans,YrPd=="2008 P1"),
     plot(Term.DOY,MeanDM,ylim=c(100,1100), 
          xlim=range(RyeMeans$Term.DOY),
          ylab=expression(paste("Rye biomass (g ",m^{-2},")")),
          xlab="Date", type="n",xaxt="n")) # subest not necessary here

# for dates on X axis, use this, and add xaxt="n" to plot cmd above
axis(side=1,at=seq(120,160,by=10),
     labels=c("May 1","May 11","May 21","May 31","June 10")) 


# 16.3 - Plotting Errorbars and Preparing to Add Lines --------------------
## add error bars for termination date (Term.DOY) in each treatment group (YrPd).
with(subset(RyeMeans,YrPd=="2008 P1"),
     arrows(Term.DOY, MeanDM+DMse, Term.DOY, MeanDM-DMse, length = 0.05, angle = 90, code=3, lwd=1, col="black"))
with(subset(RyeMeans,YrPd=="2008 P2"),
     arrows(Term.DOY, MeanDM+DMse, Term.DOY, MeanDM-DMse, length = 0.05, angle = 90, code=3, lwd=2, col="black"))
with(subset(RyeMeans,YrPd=="2009 P1"),
     arrows(Term.DOY, MeanDM+DMse, Term.DOY, MeanDM-DMse, length = 0.05, angle = 90, code=3, lwd=1, col="grey57"))
with(subset(RyeMeans,YrPd=="2009 P2"),
     arrows(Term.DOY, MeanDM+DMse, Term.DOY, MeanDM-DMse, length = 0.05, angle = 90, code=3, lwd=2, col="grey57"))
legend("bottomright",inset=0.015,
       legend=c("Sept. 2008","Oct. 2008","Sept. 2009","Oct. 2009"),
       lwd=c(1,2,1,2), col=c("black","black","grey57","grey57"), 
       lty=c(1,1,2,2),title="Rye planting date")
# ADD lines
endpoints<-data.frame(Term.DOY= c(117,155,117,155,118,160,118,160), 
                      YrPd=rep(c("2008 P1","2008 P2","2009 P1","2009 P2"),each=2))
endpoints<-cbind(endpoints, predict(lm(MeanDM~Term.DOY*YrPd,data=RyeMeans),
                                    newdata=endpoints))

# 16.4 - Adding Lines and a Small Table to a Plot -------------------------


