#Import packages
library(datasets)
library(dplyr)
library(class)
library(ggplot2)

#Import datasets
data(iris)
head(iris)

#Exploratory Data Analysis
summary(iris)
ggplot(iris, aes(Species,Petal.Length)) + geom_boxplot(fill = "red")+
  scale_y_continuous("Sepal.Length", breaks= seq(0,15000, by=500))+
  labs(title = "Box Plot", x = "Outlet Identifier")

#Determine a seed for reproducibility
set.seed(7)

#Create a sample
ran <- sample(1:nrow(iris), 0.9 * nrow(iris))

#Normalize data
norm <-function(x) { (x -min(x))/(max(x)-min(x)) }
iris_norm <- as.data.frame(lapply(iris[,c(1,2,3,4)], norm))
summary(iris_norm)

#Seperate outputs from inputs
iris_target_category <- iris[ran,5]
iris_test_category <- iris[-ran,5]

#Splitting data
iris_train <- iris_norm[ran,] 
iris_test <- iris_norm[-ran,] 

#Run kNN model
pr <- knn(iris_train,iris_test,cl=iris_target_category,k=13)

#Check performance
tab <- table(pr,iris_test_category)
accuracy<- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
tab
accuracy(tab)
