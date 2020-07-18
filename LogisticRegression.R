#Required packages for decision tree and ROC curve
library(tree)
library(ggplot2)

#importing dataset
Titanic_testsurvived <- read.csv("/Users/sakahome/Documents/Rdataset/Titanic/gender_submission.csv",header=TRUE,sep=",")
Titanic_testdata <- read.csv("/Users/sakahome/Documents/Rdataset/Titanic/test.csv",header=TRUE,sep=",")
Titanic_traindata <- read.csv("/Users/sakahome/Documents/Rdataset/Titanic/train.csv",header=TRUE,sep=",")
Titanic_newtestdata<- merge(Titanic_testdata,Titanic_testsurvived)

#changing output attribute to categorical variable
Titanic_traindata$Survived=as.factor(Titanic_traindata$Survived)
Titanic_newtestdata$Survived=as.factor(Titanic_newtestdata$Survived)
Titanic_traindata$Sex=as.factor(Titanic_traindata$Sex)
Titanic_newtestdata$Sex=as.factor(Titanic_newtestdata$Sex)

#initialize seed to reproduce the same result
set.seed(9)

#implement logistic regression
model<- glm(Survived~Sex+Pclass+Fare+SibSp+Parch,data=Titanic_traindata,family = "binomial")
summary(model)
fitted.results<-predict(model, Titanic_newtestdata,type = 'response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != Titanic_newtestdata$Survived)
table(Titanic_newtestdata$Survived,fitted.results)

#fit the tree model using training data
treemodel=tree(Survived~Sex,Titanic_traindata)
treemodel
plot(treemodel,col="blue")
text(treemodel,cex=0.75) 
summary(treemodel)
  
#check how the model is performing using the test data
treeprediction=predict(treemodel,Titanic_newtestdata,type = 'class')
t<- table(Titanic_newtestdata$Survived,treeprediction)
accuracy<- sum(diag(t))/sum(t)
