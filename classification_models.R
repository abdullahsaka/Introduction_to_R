library(tree)
library(rpart)
library(rpart.plot)
library(caTools)
bank=read.csv("UniversalBank.csv")
bank$Personal.Loan=as.factor(bank$Personal.Loan)
set.seed(485)
split=sample.split(bank$Personal.Loan,SplitRatio=0.6)
banktr=subset(bank,split==TRUE)
bankte=subset(bank,split==FALSE)
bt=tree(Personal.Loan~.,data=banktr)
plot(bt)
text(bt,pretty=0)
predbank=predict(bt,newdata=bankte,type="class")
table(bankte$Personal.Loan,predbank)
tbl=table(bankte$Personal.Loan,predbank)
100*(1-sum(diag(tbl))/sum(tbl))
(tbl=table(bankte$Personal.Loan,predbank))

##  A large tree
bt2=tree(Personal.Loan~.,data=banktr,mindev=0)
plot(bt2)
text(bt2,pretty=0,cex=0.5)
predbank2=predict(bt2,newdata=bankte,type="class")
(tbl2=table(bankte$Personal.Loan,predbank2))
100*(1-sum(diag(tbl2))/sum(tbl2))

## rpart
rp1=rpart(Personal.Loan~.,data=banktr,control=rpart.control(minbucket=1))
prp(rp1,type=1,extra=101)
prp(rp1,type=2,extra=101)
rp2=rpart(Personal.Loan~.,data=banktr,control=rpart.control(cp=0.3))
prp(rp2,type=2,extra=101)
rp3=rpart(Personal.Loan~.,data=banktr,control=rpart.control(cp=0.1))
prp(rp3,type=2,extra=101)
rp4=rpart(Personal.Loan~.,data=banktr,control=rpart.control(cp=0.001))
prp(rp4,type=2,extra=101)

library(MASS)
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
trBos=Boston[train,]
tree.boston=tree(medv~.,data=trBos)
## tree.boston=tree(medv~.,data=Boston,subset=train)
summary(tree.boston)
tree.boston
sum((trBos$medv-mean(trBos$medv))^2
    plot(tree.boston)
    text(tree.boston,pretty=0)
    yhat=predict(tree.boston,newdata=Boston[-train,])
    boston.test=Boston[-train,"medv"]
    plot(yhat,boston.test)
    abline(0,1)
    mean((yhat-boston.test)^2)
    
##Pruning
cv.boston=cv.tree(tree.boston)
cv.boston
plot(cv.boston$size,cv.boston$dev,type='b')
prune.boston=prune.tree(tree.boston,best=5)
plot(prune.boston)
text(prune.boston,pretty=0)
    
    
### Another sample
library(ISLR)
Carseats
dim(Carseats)
str(Carseats)
summary(Carseats)
## Cross Validation with Classification Trees
High=ifelse (Carseats$Sales <=8," No"," Yes ")
car2=data.frame(Carseats,High)
tree.car2=tree(High~.-Sales ,data=car2)
plot(tree.car2)
text(tree.car2,cex=0.6)
tree.car2
table(car2$High)
tree.car2.pred=predict(tree.car2,type ="class")
table(car2$High,tree.car2.pred)
set.seed(3)
cv.car2 = cv.tree(tree.car2, K=10)
plot(cv.car2, pch=21, bg=5, type="p", cex=1.5, ylim=c(0,700))
car2.cut = prune.tree(tree.car2, best=3)
plot(car2.cut)
text(car2.cut)
    
cv.car3 = cv.tree(tree.car2,FUN=prune.misclass)
plot(cv.car3$size ,cv.car3$dev ,type="b")
plot(cv.car3$k ,cv.car3$dev ,type="b")
prune.car3 =prune.misclass (tree.car2 ,best =9)
plot(prune.car3 )
text(prune.car3 ,pretty =0)