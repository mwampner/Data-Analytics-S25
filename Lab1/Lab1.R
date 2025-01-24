
# read and view file
setwd("C:/Users/wampnm/OneDrive/Data Analytics/")
EPI_data <- read.csv("epi2024results06022024.csv") 
View(EPI_data)

# Tips
attach(EPI_data)
EPI.new
NAs <- is.na(EPI.new)
EPI.new.noNAs <- EPI.new[!NAs]

# Exercise 1
summary(EPI.new) # stats 
fivenum(EPI.new,na.rm=TRUE)
stem(EPI.new) # stem and leaf plot 
hist(EPI.new) 
hist(EPI.new, seq(20., 80., 1.0), prob=TRUE) 
lines(density(EPI.new,na.rm=TRUE,bw=1.)) # or try bw=“SJ” 
rug(EPI.new)
# other functions
boxplot(EPI.new, APO.new) 
hist(EPI.new, seq(20., 80., 1.0), prob=TRUE)
lines (density(EPI.new,na.rm=TR, bw=1.)) 
rug(EPI.new) 

x<-seq(20,80,1) 
q<- dnorm(x,mean=42, sd=5,log=FALSE) 
lines(x,q)
lines(x,.4*q) 
q<-dnorm(x,mean=65, sd=5,log=FALSE) 
lines(x,.12*q) 


# Exercise 2
plot(ecdf(EPI.new), do.points=FALSE, verticals=TRUE) 
qqnorm(EPI.new); qqline(EPI.new) 
qqplot(rnorm(250), EPI.new, xlab = "Q-Q plot for norm dsn") 
qqline(EPI.new)
qqplot(rt(250, df = 5), EPI.new, xlab = "Q-Q plot for t dsn") 
qqline(EPI.new)

# Exercise 2a
# Variable 1
ECO.new
NAs <- is.na(ECO.new)
ECO.new.noNAs <- ECO.new[!NAs]

summary(ECO.new) # stats 
fivenum(ECO.new,na.rm=TRUE)
stem(ECO.new) # stem and leaf plot 
hist(ECO.new) 
hist(ECO.new, seq(20., 84., 1.0), prob=TRUE) 
lines(density(ECO.new,na.rm=TRUE,bw=1.)) # or try bw=“SJ” 
rug(ECO.new)
# other functions
boxplot(ECO.new, APO.new) 
hist(ECO.new, seq(20., 84., 1.0), prob=TRUE)
lines (density(ECO.new,na.rm=TR, bw=1.)) 
rug(ECO.new) 

x<-seq(20,84,1) 
q<- dnorm(x,mean=42, sd=5,log=FALSE) 
lines(x,q)
lines(x,.4*q) 
q<-dnorm(x,mean=65, sd=5,log=FALSE) 
lines(x,.12*q) 


# Exercise 2
plot(ecdf(ECO.new), do.points=FALSE, verticals=TRUE) 
qqnorm(ECO.new); qqline(ECO.new) 
qqplot(rnorm(250), ECO.new, xlab = "Q-Q plot for norm dsn") 
qqline(ECO.new)
qqplot(rt(250, df = 5), ECO.new, xlab = "Q-Q plot for t dsn") 
qqline(ECO.new)

# Variable 2
ECO.old
NAs <- is.na(ECO.old)
ECO.old.noNAs <- ECO.old[!NAs]

summary(ECO.old) # stats 
fivenum(ECO.old,na.rm=TRUE)
stem(ECO.old) # stem and leaf plot 
hist(ECO.old) 
hist(ECO.old, seq(20., 84., 1.0), prob=TRUE) 
lines(density(ECO.old,na.rm=TRUE,bw=1.)) # or try bw=“SJ” 
rug(ECO.old)
# other functions
boxplot(ECO.old, APO.new) 
hist(ECO.old, seq(20., 84., 1.0), prob=TRUE)
lines (density(ECO.old,na.rm=TR, bw=1.)) 
rug(ECO.old) 

x<-seq(20,84,1) 
q<- dnorm(x,mean=42, sd=5,log=FALSE) 
lines(x,q)
lines(x,.4*q) 
q<-dnorm(x,mean=65, sd=5,log=FALSE) 
lines(x,.12*q) 


# Exercise 2
plot(ecdf(ECO.old), do.points=FALSE, verticals=TRUE) 
qqnorm(ECO.old); qqline(ECO.old) 
qqplot(rnorm(250), ECO.old, xlab = "Q-Q plot for norm dsn") 
qqline(ECO.old)
qqplot(rt(250, df = 5), ECO.old, xlab = "Q-Q plot for t dsn") 
qqline(ECO.old)

