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

lines(endpoints[1:2,1],endpoints[1:2,3],lwd=1,col="black")
lines(endpoints[3:4,1],endpoints[3:4,3],lwd=2,col="black")
lines(endpoints[5:6,1],endpoints[5:6,3],lwd=1, col="grey57", lty=2)
lines(endpoints[7:8,1],endpoints[7:8,3],lwd=2, col="grey57", lty=2)
# ADD ANOVA table
legend(121,1137,legend=c("Effect","Slope(Date)","Cereal rye planting date","Year",expression(R^2)), 
       bty="n",adj=0.5,cex=0.9) # adj=0.5:centered text
legend(130.5,1137,legend=c("p-value","0.0001","0.0138","0.0001","0.84"),bty="n",adj=0.5,cex=0.9)
rect(116,835,135.5,1110);   lines(x=c(116.5,135),y=c(1060,1060),lwd=2)
### dev.off()


# 16.5 - Plotting Confidence Intervals - Preparing the Data ---------------
# Scatter Plots with Confidence Ribbons
pHmeans<-read.table("Data/ph data.txt",header=TRUE,sep="\t")
pHmeans<-pHmeans[pHmeans$side=="down",-3] # simplify the data
head(pHmeans)

xl<-range(pHmeans$dist) # x limits
op<-par(mar=c(4.1,4.1,1,1)) # set par() 
with(subset(pHmeans,trt=="L"),
     plot(dist,mean,xlab="Distance (m)",
          ylab="",type="l",ylim=range(pHmeans$mean), 
          frame=FALSE,las=1)) # plot
with(subset(pHmeans,trt=="S"),lines(dist ,mean,lty=2))
par(op)


# 16.6 - Introducing polygon() and Prepping Data for Ribbons --------------
par(mar=c(3,3,0.5,0.5))
plot(1:40, (1:40)/5,type="n")
polygon(x=c(20,25,30,30,20),y=c(5,6,6.2,4,5))
x<-c(5,10,15,20)
y<-c(4,4.5,4.2,5)
polygon(x=c(x,rev(x)),y=c(y,rev(y+1.5)),col="grey")

L<-with(subset(pHmeans,trt=="L"),c(mean+sterr*1.96,rev(mean-sterr*1.96)))
S<-with(subset(pHmeans,trt=="S"),c(mean+sterr*1.96,rev(mean-sterr*1.96)))
dds<-with(subset(pHmeans,trt=="S"),c(dist,rev(dist)))
ddl<-with(subset(pHmeans,trt=="L"),c(dist,rev(dist)))
ddl

# 16.7 - Polygons with Semi-Transparent Colors ----------------------------

lime=rgb(t(col2rgb("grey44")),alpha=128,max=255) # lime="#70707080"
shal=rgb(t(col2rgb("darkolivegreen2")),alpha=128,max=255) # "#BCEE6880"
xl<-range(pHmeans$dist)
col2rgb("grey44")

op<-par(mar=c(4.1,4.1,1,1))
with(subset(pHmeans,trt=="L"),
     plot(dist,mean,xlab="",ylab="",type="n",
          ylim=c(3.5, 8.25),xlim=xl, las=1,frame=FALSE))
polygon(ddl,L,col=lime, border=NA)
polygon(dds,S,col=shal, border=NA)
with(subset(pHmeans,trt=="L"),lines(dist,mean,xlab="",lty=1,lwd=2))
with(subset(pHmeans,trt=="S"),lines(dist,mean,xlab="",lty=1,lwd=2))
legend("topright",inset=0.1,fill=c(rgb(r=0.5,g=0.5,b=0.5,alpha=0.5),rgb(r=0.73,g=0.93,b=0.41,alpha=0.7)), legend=c("Limestone","Shale"),cex=1.2)
mtext(text="Soil pH",side=2,cex=1.2,line=2.2)
mtext(text="Distance from road (m)",side=1,line=2,cex=1.2)


# 16.8 - Errorbars in 2 Dimensions & Ellipses -----------------------------
# Error Bars in 2 Dimensions
data(mtcars)
cols=c("black","red","blue")
with(mtcars, plot(disp,hp,col=cols[cyl/2-1]))


car.means<-aggregate(mtcars[,3:4],by=mtcars[2],mean)
car.means[,4:5]<-aggregate(mtcars[,3:4],by=mtcars[2],function(x) 1.96*sd(x)/sqrt(length(x)))[2:3]
names(car.means)[4:5]<-c("hp.CI","disp.CI")

with(mtcars, plot(disp,hp,col=cols[cyl/2-1]))
with(car.means, points(disp,hp,pch=24,bg=cols[cyl/2-1]))
with(car.means, arrows(disp,hp-hp.CI,disp,hp+hp.CI,code=3,length=0.1,angle=90,col=cols[cyl/2-1]))
with(car.means, arrows(disp-disp.CI,hp,disp+disp.CI,hp,code=3,length=0.1,angle=90,col=cols[cyl/2-1]))

library(shape)
with(mtcars, plot(disp,hp,col=cols[cyl/2-1]))
with(car.means, points(disp,hp,pch=24,bg=cols[cyl/2-1]))
for(i in 1:3){
  with(car.means, plotellipse(rx=disp.CI[i],ry=hp.CI[i],mid = c(disp[i],hp[i]) ,lcol=cols[i],lwd=01))
}


# 16.9 - Reversing Axes ---------------------------------------------------
## Reversing Axes
par(mfrow=c(1,2))
depth<-seq(0,200,20)
DO<-c(5,2.5,2,1.5,0.5,0.4,0.3,0.1,0.1,0.1,0.5)
plot(DO,depth,type="l")
plot(DO,-depth,type="l")

par(mfrow=c(1,1))
par(mar=c(1,5,4,1),tck=0.02) # set margins
plot(DO,-depth,type="l",xaxt="n",yaxt="n",xlab="",ylab="")
axis(side=2,at=-depth,labels = depth,las=1)
axis(side=3)
mtext(side=c(2,3), line=2.5, c("Depth (m)","Dissolved Oxygen (mg/l)"))


# Fun with R graphics
random.color.boxes=function(n=10){
  cols<-colors()[c(1,26,552,652,454)]
  cols<-col2rgb(cols) # to find out the rgb code for a color
  als<-c(rep(0,5),rep(10,5),seq(0,220,20))
  # rgb(red,green,blue,alpha,max) # to specify color
  
  par(mar=c(1,1,1,1))
  plot(0:10,0:10,type="n",xaxt="n",yaxt="n",xlab="",ylab="")
  cs<-sample(1:5,n,rep=T);as<-sample(als,n,rep=T)
  a<-runif(n)*10;b<-runif(n)*10;c<-runif(n)*10;d<-runif(n)*10
  rect(a,b,c,d,border="black",col=rgb(cols[1,cs],cols[2,cs],cols[3,cs],as,max=255))
  rect(a,b,c,d,border="black") # replot borders
}
random.color.boxes()

