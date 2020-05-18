#create a vector
vect<- c(1,3,5,7)

# find length of vector
length(vect)

#list all of objects and then remove objects
ls()
rm(list = ls())

#convert discrete values into ordered categorical values
temperature_vector <- c("High", "Low", "High", "Low", "Medium")
factor_temperature_vector <- factor(temperature_vector, order = TRUE, 
                                    levels = c("Low", "Medium", "High"))

#create a matrix 
mtr<- matrix(c(1,2,3,4),nrow=2,ncol=2)
mtr
sqrt(mtr)

A<- matrix(1:16,4,4)
dim(A)
A[3,2]
A[2:3,3:4]

#set random variables
set.seed(34)
y<- rnorm(50,mean = 10,sd=.3)
mean(y)
var(y)
sd(y)

#plotting
a<- rnorm(100)
b<-rnorm(100)
plot(a,b,xlab='x axis',ylab='y axis',main ='Randomized',col='blue')
pdf('Figure_1.pdf')

#create a sequence of numbers
z<- seq(1,23)

#load Auto data
library(ISLR)
head(Auto)
Auto[100:125,4:7]
which (is.na(Auto))
attach(Auto)
colnames(Auto)
pairs(~mpg+displacement+horsepower+weight+acceleration)
hist(mpg,main = 'Auto',col=3,breaks = 15)
plot(cylinders,mpg,col='red',varwidth=T,horizontal=T)
str(Auto)
summary(Auto)
names(Auto)
dim(Auto)
plot(Auto$cylinders,Auto$mpg,xlab = 'Mpg',ylab = 'Cyclinders',main = 'Automobile',col='dark blue')

#final
savehistory()
