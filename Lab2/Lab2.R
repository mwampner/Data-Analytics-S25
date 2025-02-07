library("ggplot2")
library("readr")
# read and view file
#setwd("C:/Users/wampnm/OneDrive/Data Analytics/Lab2")
NY_House_data <- read.csv("NY-House-dataset.csv") 
View(NY_House_data)
attach(NY_House_data)

#initial cleaning (NAs)
NY_House_data[is.na(NY_House_data) | NY_House_data=="Inf"] = NA
# variables
TYPE
NAs <- is.na(TYPE)
TYPE.noNAs <- TYPE[!NAs]

PRICE
NAs <- is.na(PRICE)
PRICE.noNAs <- PRICE[!NAs]

BEDS
NAs <- is.na(BEDS)
BEDS.noNAs <- BEDS[!NAs]

BATH
NAs <- is.na(BATH)
BATH.noNAs <- BATH[!NAs]

PROPERTYSQFT
NAs <- is.na(PROPERTYSQFT)
PROPERTYSQFT.noNAs <- PROPERTYSQFT[!NAs]

# PROPERTYSQFT
ggplot(NY_House_data, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point()

## filter data
NY_House_data <- NY_House_data[NY_House_data$PRICE<195000000,]

NY_House_data <- NY_House_data[NY_House_data$PROPERTYSQFT!=2184.207862,]

NY_House_data$PROPERTYSQFT[NY_House_data$BROKERTITLE=="Brokered by Douglas Elliman - 575 Madison Ave"][85]

## column names
names(NY_House_data)

## fit linear model
lmod <- lm(PRICE~PROPERTYSQFT, data = NY_House_data)

lmod <- lm(log10(PRICE)~log10(PROPERTYSQFT), data = NY_House_data)

## print model output
summary(lmod)

## scatter plot of 2 variables
plot(PRICE~PROPERTYSQFT, data = NY_House_data)
abline(lmod)

plot(log10(PRICE)~log10(PROPERTYSQFT), data = NY_House_data)
abline(lmod)

## scatter plot of 2 variables
ggplot(NY_House_data, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point()

ggplot(NY_House_data, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

ggplot(NY_House_data, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")


# BATH
#ggplot(NY_House_data, aes(x = log10(BATH), y = log10(PRICE))) +
ggplot(NY_House_data, aes(x = BATH, y = log10(PRICE))) +
  geom_point()

## filter data
NY_House_data <- NY_House_data[NY_House_data$PRICE<195000000,]

NY_House_data <- NY_House_data[NY_House_data$BATH<=20,]
NY_House_data <- NY_House_data[NY_House_data$BATH>0,]

NY_House_data$PROPERTYSQFT[NY_House_data$BROKERTITLE=="Brokered by Douglas Elliman - 575 Madison Ave"][85]

## column names
names(NY_House_data)

## fit linear model
lmod <- lm(PRICE~BATH, data = NY_House_data)

lmod <- lm(log10(PRICE)~log10(BATH), data = NY_House_data)

## print model output
summary(lmod)

## scatter plot of 2 variables
plot(PRICE~BATH, data = NY_House_data)
abline(lmod)

plot(log10(PRICE)~log10(BATH), data = NY_House_data)
abline(lmod)

## scatter plot of 2 variables
ggplot(NY_House_data, aes(x = BATH, y = PRICE)) +
  geom_point()

ggplot(NY_House_data, aes(x = BATH, y = PRICE)) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

ggplot(NY_House_data, aes(x = log10(BATH), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

# BEDS
#ggplot(NY_House_data, aes(x = log10(BEDS), y = log10(PRICE))) +
ggplot(NY_House_data, aes(x = BEDS, y = log10(PRICE))) +
  geom_point()

## filter data
NY_House_data <- NY_House_data[NY_House_data$PRICE<195000000,]

NY_House_data <- NY_House_data[NY_House_data$BEDS<=20,]

NY_House_data$PROPERTYSQFT[NY_House_data$BROKERTITLE=="Brokered by Douglas Elliman - 575 Madison Ave"][85]

## column names
names(NY_House_data)

## fit linear model
lmod <- lm(PRICE~BEDS, data = NY_House_data)

lmod <- lm(log10(PRICE)~log10(BEDS), data = NY_House_data)

## print model output
summary(lmod)

## scatter plot of 2 variables
plot(PRICE~BEDS, data = NY_House_data)
abline(lmod)

plot(log10(PRICE)~log10(BEDS), data = NY_House_data)
abline(lmod)

## scatter plot of 2 variables
ggplot(NY_House_data, aes(x = BEDS, y = PRICE)) +
  geom_point()

ggplot(NY_House_data, aes(x = BEDS, y = PRICE)) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

ggplot(NY_House_data, aes(x = log10(BEDS), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

