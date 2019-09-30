# Chapter 15: Visualizing Data I - Enhancing Scatter Plots

# 15.1 - Basic Use of plot() ----------------------------------------------
# first way
DT <- read.delim("Data/DataVizEx1.txt")
with(DT, plot(SeedDen, Den1))

# second way
plot(Den1 ~ SeedDen, data = DT, col=Manag)

palette()


# 15.2 - Introducing lines() and Formatting Axis Labels -------------------

with(DT, lines(SeedDen, Den1))
with(DT, plot(SeedDen, Den1, col=Manag))
lines(x=c(1000, 2000, 500), y=c(30, 45, 27))
# order is important

sym <- c(21, 24)
with(DT, plot(SeedDen, Den1, pch=sym[Manag], 
        ylab = expression(paste("Weed density (plants)",m^-2,")")),
        xlab = expression(paste("Weed seed bank(seeds ", m^-2, ")"))))


# 15.3a - Putting Together a Publication-Quality Scatter Plot -------------

lines(DT$SeedDen[DT$Manag=="C"], DT$Den1[DT$Manag=="C"], lty=1) # trt C
lines(DT$SeedDen[DT$Manag=="O"], DT$Den1[DT$Manag=="O"], lty=2) # trt C
legend("bottomright", inset = 0.025, pch = sym, 
       lty = 1:2, c("Conventional", "Hebicide Free"))


# 15.3b - Introducing layout() for Multi-Panel Plots ----------------------
layout(matrix(c(1,2,3,3,0,4), nrow=3, byrow = TRUE), heights = c(1,1.5,1),
       widths = c(1,1.5))
layout.show(4)


# 15.4 - Controlling Plot Dimensions - quartz() and x11() -----------------

# 15.5 - Creating a Finished Multi-Panel Plot - Part I --------------------

layout(matrix(c(1,2,3),nrow=3),heights=c(1,1,1.3))
sym<-c(21,24) # plotting characters to use
par(mar=c(0.1,4.3,0.1,1),bty="l") # set margins and plot frame type
## plot 1
with(DT,plot(SeedDen,DT[,7],pch=sym[Manag],xaxt="n",xlab="",ylab=""))
lines(DT$SeedDen[DT$Manag=="C"],DT[DT$Manag=="C",7],lty=1)  
lines(DT$SeedDen[DT$Manag=="O"],DT[DT$Manag=="O",7],lty=2) 
text(300,max(DT[,7])*0.97,"Mid season density",pos=4,cex=1.2)
mtext(side=2,line=2.5,at=-1,text=expression(paste("Weed density (plants ",m^-2,")")),cex=0.9)
legend("bottomright",inset=0.025,pch=sym,lty=1:2,c("Conventional","Herbicide free"))


# 15.6 - Creating a Finished Multi-Panel Plot - Part II -------------------
## plot 2
with(DT,plot(SeedDen,DT[,8],pch=sym[Manag],xaxt="n",xlab="",ylab=""))
lines(DT$SeedDen[DT$Manag=="C"],DT[DT$Manag=="C",8],lty=1)  
lines(DT$SeedDen[DT$Manag=="O"],DT[DT$Manag=="O",8],lty=2) 
text(300,max(DT[,8])*0.97,"Final density",pos=4,cex=1.2)

## plot 3
par(mar=c(4.1,4.3,0.1,1)) # change margins
with(DT,plot(SeedDen,DT[,4],pch=sym[Manag],xaxt="n",xlab="",ylab=""))
lines(DT$SeedDen[DT$Manag=="C"],DT[DT$Manag=="C",4],lty=1)  
lines(DT$SeedDen[DT$Manag=="O"],DT[DT$Manag=="O",4],lty=2) 
text(300,max(DT[,4])*0.97,"Final biomass",pos=4,cex=1.2)
axis(side=1,at=seq(500,3000,500))
mtext(side=1,line=2.5,text=expression(paste("Weed seedbank (seeds ",m^-2,")")),cex=0.9)
mtext(side=2,line=2.5,text=expression(paste("Biomass (g ",m^-2,")")),cex=0.9)


# 15.7 - Using Loops for Multi-Panel Figures ------------------------------
## Loops for multipanel plots
par(mfrow=c(3,1),oma=c(4.1,0,3,0),mar=c(0.1,4.3,0.1,1),bty="l")
vars<-c(7,8,4) # the column numbers for each panel's response variable
sym<-c(21,24) # plotting characters to use
labs<-c("","","","Final biomass","Rye","Rye termination",
        "Mid season density","Final density") # plot labels
for (i in vars){ # begin loop for panels
  # plot the ith column
  with(DT,plot(SeedDen,DT[,i],pch=sym[Manag],xaxt="n",xlab="",ylab="")) 
  lines(DT$SeedDen[DT$Manag=="C"],DT[DT$Manag=="C",i],lty=1) # add lines
  lines(DT$SeedDen[DT$Manag=="O"],DT[DT$Manag=="O",i],lty=2) 
  text(300,max(DT[,i])*0.97,labs[i],pos=4,cex=1.2)
  if(i==4){ # add x axis for the last plot only (i==4)
    axis(side=1,at=seq(500,3000,500))
    mtext(side=1,line=2.5,text=expression(paste("Weed seedbank (seeds ",m^-2,")")),cex=0.9)
    mtext(side=2,line=2.5,text=expression(paste("Biomass (g ",m^-2,")")),cex=0.9)
  }
  if(i==7) {
    # y axis label for first 4 plots
    mtext(side=2,line=2.5,at=-1,text=expression(paste("Weed density (plants ",m^-2,")")),cex=0.9) 
    legend(1100,22,legend=c("Conventional","Herbicide free"),pch=sym,lty=c(1,2),ncol=2,xpd=NA)
  } # y axis for last plot
} # end loop for panels


# 15.8 - Adding a Secondary y-axis ----------------------------------------
## Secondary axis
op<-par(mar=c(4.1,4.1,2,4.1))
beans<-read.csv("Data/BeansData.csv",comm="#") # load data
with(beans,plot(ShtDM,RtDM,xlab="Shoot biomass (g)",ylab="Root biomass (g)"))
par(new=TRUE)
with(beans,plot(ShtDM,rt.len,xaxt="n",yaxt="n",ylab="", xlab="", pch=24,bg="grey"))
axis(side=4)
mtext(side=4,line=3,"Root length (m)")
par(op)
legend("bottomright",inset=0.01,pch=c(21,24),pt.bg=c("white","grey"),
       legend=c("Biomass","Density"),xpd=NA)



