# data types
renk=c("sar˝", "k˝rm˝z˝","mavi")
class(renk)
renk2=factor(c("sar˝", "k˝rm˝z˝","mavi"))
class(renk2)
is.factor(renk)
is.factor(renk2)
renk=as.factor(renk)
class(renk)
edu=factor(c("ilk","orta","lise","bs","ms","ilk","orta","ilk"),levels=c("ilk","orta","lise","bs","ms"))
edu
str(edu)
summary(edu)
edu=factor(c("ilk","orta","lise","bs","ms","ilk","orta","ilk"),levels=c("ilk","orta","lise","bs","ms"),ordered=TRUE)
edu
str(edu)
summary(edu)

# playing with 'iris' data
data(iris)
iris$Sepal.Height=iris$Sepal.Length+iris$Sepal.Width
str(iris)
Petal.Height=iris$Petal.Length+iris$Petal.Width
iris=cbind(iris,Petal.Height)
str(iris)
iris$Sepal.Height=NULL
str(iris)
iris$Petal.Height=NULL
##Classification Trees with "tree" package
install.packages("tree")
library(tree)
iris_tr=tree(Species~Petal.Length+Sepal.Width,data=iris)
iris_tr=tree(Species~.,data=iris)
plot(iris_tr)
text(iris_tr,cex=0.8)
summary(iris_tr)
iris_tr
pred_ir=predict(iris_tr)
pred_ir=predict(iris_tr,type="class")
table(iris$Species,pred_ir)

## tree has three parameters
## mincut,minsize, mindev
# mincut: The minimum number of observations to include in either child node.
# The default is 5.
# minsize: The smallest allowed node size. The default is 10.
# mindev: The within-node deviance must be at least this times that of the root node for the node to be split.
iris_tr2=tree(Species~.,data=iris,minsize=2)
iris_tr3=tree(Species~.,data=iris,mindev=0.0)
iris_tr4=tree(Species~.,data=iris,minsize=2,mindev=0.0)
plot(iris_tr4)
text(iris_tr4,cex=0.8)
summary(iris4_tr)
pred_iris4=predict(iris_tr4,type="class")
table(iris$Species,pred_iris4)

## tree construction with rpart
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

iris_rpart1=rpart(Species~.,method="class",data=iris,control=rpart.control(minbucket=1))
prp(iris_rpart1)
iris_rpart2=rpart(Species~.,method="class",data=iris,control=rpart.control(minbucket=1,cp=0.0001))
prp(iris_rpart2)
pred_iris5=predict(iris_rpart2,type="class")
table(iris$Species,pred_iris5)
## minsplit = 20, minbucket = round(minsplit/3), cp = 0.01

## reading from a file
bank=read.csv("UniversalBank.csv")
str(bank)
summary(bank)
bt=tree(Personal.Loan~.,data=bank)
plot(bt)
text(bt,cex=0.8)
bt
## Changing the output attribute to a categorical attribute
bank$Personal.Loan=as.factor(bank$Personal.Loan)
str(bank)
## levels = 0 or 1 to levels = rejected or accepted
levels(bank$Personal.Loan)=c("rejected",'accepted') 
## back to the original
levels(bank$Personal.Loan)=c('0','1')
str(bank)

set.seed(1000)
train=sample(1:5000,3000)
## randomly chosen 3000 observations assigned train data 
## remaining goes to test data
banktr=bank[train,]
bankte=bank[-train,]
table(banktr$Personal.Loan)
table(bankte$Personal.Loan)

## Personal.Loan=1 (select %60) ve =0 (select %60)
## install caTools package
install.packages("caTools")
library(caTools)
set.seed(1000)
split=sample.split(bank$Personal.Loan,SplitRatio=0.6)
split
summary(split)

## split data into train and test
banktr=subset(bank,split==TRUE)
bankte=subset(bank,split==FALSE)
## build decision tree
bt=tree(Personal.Loan~.,data=banktr)
plot(bt)
text(bt,pretty=0)
bt1
## predictions are 1 or 0
predbank=predict(bt,newdata=bankte,type="class")
## create a confusion matrix 
table(bankte$Personal.Loan,predbank)
predbank2=predict(banktree,newdata=bankte1)
## snip tree
bts1=snip.tree(bt,2)
plot(bts1)
text(bts,pretty=0)
bts2=snip.tree(bt,nodes=c(2,12))
plot(bts2)
text(bts2,pretty=0)

##  A large tree
bt2=tree(Personal.Loan~.,data=banktr,mindev=0.00001)
plot(bt2)
text(bt2,pretty=0,cex=0.5)