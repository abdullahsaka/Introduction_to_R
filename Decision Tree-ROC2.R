# Fitting Classification Tree Models
# We are going to use the Carseats dataset from the ISLR library

library(ISLR) # data library for statistical usage
library(tree) #to fit the decision trees
library(ROCR)
library(pROC)
library(caTools)
library(rpart)
library(rpart.plot)

attach(Carseats) #dataset to use
head(Carseats) #shows a data sample
Carseats #show the original data itself 
str(Carseats)
range(Sales) #shows the range of values in the column

#Create a categorical variables bases on Sales
High=ifelse(Sales>=8,1,0)
High
length(High)
dim(Carseats)

#Carseats$Sales=as.factor(Carseats$Sales) 336 levels 
Carseats= data.frame(Carseats,High)
names(Carseats)
str(Carseats)

Carseats=Carseats[,-1] #drop the column you do not need which is Sales
names(Carseats)
dim(Carseats)

#Split data into testing and training using

set.seed(1000)
train=sample.split(Carseats$High,SplitRatio=0.6)
train
summary(train)
training_data=Carseats[train,] #rows, columns
testing_data=Carseats[!train,]
Testing_High=High[!train]

#fit the tree model using training data

DecisionTreeModel<-rpart(High~.,training_data,method = "class")
DecisionTreeModel
plot(DecisionTreeModel)
text(DecisionTreeModel,pretty = 0)
summary(DecisionTreeModel)

#Predictions
PredictionsWithClass<-predict(DecisionTreeModel,testing_data,type = "class")
PredictionsWithClass
t<-table(predictions=PredictionsWithClass,actual=Testing_High)
t

##accuracy metric

sum(diag(t))/sum(t)

##Plotting Roc curve and calculating AUC metric

PredictionsWithProbs<-predict(DecisionTreeModel,testing_data,type="prob")
PredictionsWithProbs
auc<-auc(Testing_High,PredictionsWithProbs[,2])
auc
plot(roc(Testing_High,PredictionsWithProbs[,2]))


