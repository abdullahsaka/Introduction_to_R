# Fitting Classification Tree Models
# We are going to use the Carseats dataset from the ISLR library

library(ISLR) # data library for statistical usage
library(tree) #to fit the decision trees
library(ROCR)
library(pROC)


attach(Carseats) #dataset to use
head(Carseats) #shows a data sample
Carseats #show the original data itself 
str(Carseats)
range(Sales) #shows the range of values in the column

#Create a categorical variables bases on Slaes
High=ifelse(Sales>=8,"Yes", "No") #OR
#High=ifelse(Sales>=8,1,0)
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

set.seed(2)  #it is important to have the same number in seed to get the same results for splitting
train=sample(1:nrow(Carseats),nrow(Carseats)/2) #we are sampling from the dataset and selesct the random half of it for training
summary(train)
test=-train #the remaining part from the train sample
training_data=Carseats[train,] #rows, columns
testing_data=Carseats[test,]
Testing_High=High[test]


#fit the tree model using training data

tree_model=tree(High~.,training_data)
plot(tree_model)
text(tree_model,pretty = 0) #we added pretty to see categorical data branches
summary(tree_model)

#check how the model is performing using the test data

tree_pred=predict(tree_model,testing_data,type="class")
mean(tree_pred!=Testing_High)  #misclasification error 
tree_pred
##Prune the tree
#Cross validation to check where to stop pruning

set.seed(3)
cv_tree=cv.tree(tree_model,FUN = prune.misclass)
names(cv_tree) #includes tree size, error rate ...

plot(cv_tree$size,cv_tree$dev, type="b") #x axis variable, y axis variable
#min error rate is tree size of 9
#prune the tree size to 9

pruned_model=prune.misclass(tree_model,best=9)
plot(pruned_model)
text(pruned_model,pretty = 0)
summary(pruned_model)

#check how it is doing

tree_pred=predict(pruned_model,testing_data,type="class")
mean(tree_pred!=Testing_High)

#Predictions

t<-table(predictions=tree_pred,actual=Testing_High)
t

##accuracy metric

sum(diag(t))/sum(t)

##Plotting Roc curve and calculating AUC metric
pred_model<-predict(pruned_model,testing_data,type="tree")
pred_model


