#' Mean and Standard Error
#' 
#' This function returns meand and standard error of the mean
#' @param x An object whose mean and se will be returned.
#' @keywords  summary
#' @export
#' @examples 
#' a <- rnorm(n=15, mean=6, sd=4)
#' mean_se(a)

meanANDse<-function(x){
  m<-mean(x,na.rm=TRUE)
  se<-sd(x,na.rm=TRUE)/(sqrt(sum(is.na(x)==FALSE)))
  cat(paste("mean = ",m,"; se = ",se,"\n"))
}
