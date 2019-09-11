# Chapter 20: Writing Functions and Packages

# 20.1 - Creating a Function ----------------------------------------------

# simple demo - no braces if codes are on one line
fn.demo <- function(x, y) x^2 - y
fn.demo(4, 5)

# list workspace
ls()


# 20.2 - Specifying Default Values for Arguments --------------------------
fn.demo <- function(x, y=5) x^2 - y
fn.demo(4)
fn.demo(2, 4)

fn.demo<-function(x,y=5,verbose=FALSE) {
  z<- x^2-y
  if(verbose==TRUE) {cat(paste(x," squared minus ",y," = ",z,sep=""))} else {return(z)}
}

fn.demo(5)
fn.demo(5, verbose = TRUE)


# 20.3 - Returning Values -------------------------------------------------

## work space considerations
zz <- fn.demo(5)

## return multiple values
fn.demo <- function(x, y=5){
  c(x, y, x^2 - y)
}

fn.demo(6)
a <- fn.demo(6)
a


# 20.4 - Functions That Change Objects Are Risky --------------------------

## bad example
a <- 5
bad.fn <- function(b=7){a<<-b}
a
bad.fn(14)
a


# 20.5 - Debugging Errors and Checking Inputs -----------------------------

fn.demo <- function(x, y=5){
  z <- x + 10 - y
  return(z)
}

# error input
fn.demo(x="black")

# debug
debug(fn.demo)
fn.demo(x="black")

# use stop() function
fn.demo<-function(x,y=5) {
  if (is.numeric(x)==FALSE | is.numeric(y)==FALSE) stop ("x & y must be numeric")
  z<- x+10-y
  return(z)
}
fn.demo(4)
fn.demo(x="black")


# 20.6 - Sneaky Errors - NA Values ----------------------------------------
# no na actions
mean.se<-function(x){
  m<-mean(x)
  se<-sd(x)/(sqrt(length(x)))
  cat(paste("mean = ",m,"; se = ",se,"\n"))
}

a<-c(2,3,4,5,6)
mean.se(a)

b<-a; b[3]<-NA
mean.se(b)

# with na actions - wrong
mean.se<-function(x){
  m<-mean(x,na.rm=TRUE)
  se<-sd(x,na.rm=TRUE)/(sqrt(length(x)))
  cat(paste("mean = ",m,"; se = ",se,"\n"))
}
mean.se(a)
mean.se(b)

# correct way
mean.se<-function(x){
  m<-mean(x,na.rm=TRUE)
  se<-sd(x,na.rm=TRUE)/(sqrt(sum(is.na(x)==FALSE)))
  cat(paste("mean = ",m,"; se = ",se,"\n"))
}
mean.se(a)
mean.se(b)


# 20.7 - Creating a Package Part I ----------------------------------------
# load tools and libraries
library(devtools)
library(roxygen2)

# create a package
create("easy.demo")


# 20.8 - Creating a Package Part II ---------------------------------------

# documentations
setwd("~/Examples_R/easy.demo")
document()
setwd("~/Examples_R")

# install package
install("easy.demo")
library(easy.demo)
?meanANDse

# install.packages("easy.demo", repos=NULL, type="source)
# DO NOT USE "." OR DOT