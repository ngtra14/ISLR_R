# 150 pennies found in my attic in 2014
# a sample of 150 out of 2400
# 2 rolls were broken

yrs<-59:73
gp1<-scan(text="0 0 0 1 1 4 1 0 0 3 3 1 7  8 21") # 50
gp2<-scan(text="2 2 0 1 4 6 1 1 1 6 9 7 6 10 10") # 66
gp3<-scan(text="0 0 0 2 0 5 1 1 2 2 5 4 6  0  6") # 34
year<-c(rep(yrs,gp1),rep(yrs,gp2),rep(yrs,gp3))
roll<-rep(c("R1","R2","R3"),c(50,66,34))
pennies<-data.frame(year,roll)
rm(yrs,year,roll,gp1,gp2,gp3)

hist(pennies$year)
stem(pennies$year)
# odd distribution. Why so many 73? 