## bootstrap aggregation - bagging
library(ISLR)
## Portfolio data set

alpha.fn=function(data,index){
 X=data$X[index]
 Y=data$Y[index]
 return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
 }
alpha.fn(Portfolio,1:100)
set.seed(1)
alpha.fn(Portfolio,sample(100,100,replace=T))
boot(Portfolio,alpha.fn,R=1000)

## bootstrap aggregation - bagging
boot.fn=function(data,index){
it=tree(Species~.,data=iris,subset=index)
yhat=predict(it,type="class")
tbl=table(iris$Species[index],yhat)
hata=100*(1-sum(diag(tbl))/sum(tbl))
return(hata)
}
boot.fn(iris,sample(150,150,replace=T))
boot(iris,boot.fn,R=1000)

## Pruning in regression trees
library(MASS)
set.seed(1)
tree.boston=tree(medv~.,data=Boston)
yhat=predict(tree.boston)
rmse=sqrt(mean((yhat-Boston$medv)^2))
rmse
mae=mean(abs(yhat-Boston$medv))
mae
mape=mean(100*abs(yhat-Boston$medv)/Boston$medv)
mape
# A large tree
tree.boston2=tree(medv~.,data=Boston,mindev=0,minsize=2)
summary(tree.boston2)

## Cross validation
library(ISLR)
Carseats
dim(Carseats)
str(Carseats)
summary(Carseats)
## Cross Validation with Classification Trees
High=ifelse (Carseats$Sales <=8," No"," Yes ")
car=data.frame(Carseats,High)
tree.car=tree(High~.-Sales ,data=car)
plot(tree.car)
text(tree.car,cex=0.6)
tree.car
table(car$High)
tree.car.pred=predict(tree.car,type ="class")
table(car$High,tree.car.pred)
set.seed(3)
cv.car = cv.tree(tree.car, K=10)
plot(cv.car, pch=21, bg=5, type="p", cex=1.5, ylim=c(0,700))
car.cut = prune.tree(tree.car, best=4)
plot(car.cut)
text(car.cut)
text(car.cut,pretty=0)

cv.car2 = cv.tree(tree.car,FUN=prune.misclass)
plot(cv.car2$size ,cv.car2$dev ,type="b")
plot(cv.car2$k ,cv.car2$dev ,type="b")
prune.car2 =prune.misclass (tree.car ,best =7)
plot(prune.car2)
text(prune.car2,pretty =0)

## Cross validation  with linear regression
set.seed(1)
str(Auto)
plot(Auto[,-9])
train=sample(392,196)
lm1=lm(mpg~horsepower,data=Auto,subset=train)
lm1
summary(lm1)
#RMSE on training set
sqrt(mean((Auto$mpg-predict(lm1,newdata=Auto))[train]^2))
#RMSE on test set
sqrt(mean((Auto$mpg-predict(lm1,newdata=Auto))[-train]^2))
lm2=lm(mpg~horsepower+I(horsepower^2),data=Auto,subset=train)
sqrt(mean((Auto$mpg-predict(lm2,newdata=Auto))[train]^2))
sqrt(mean((Auto$mpg-predict(lm2,newdata=Auto))[-train]^2))

# Leave-One-Out Cross-Validation
glm1=glm(mpg~horsepower,data=Auto)
coef(glm1)
lm1=lm(mpg~horsepower,data=Auto)
coef(lm1)
cv.err=cv.glm(Auto,glm1)
cv.err$delta
cv.error=rep(0,5)
for (i in 1:5){
 glm1=glm(mpg~poly(horsepower,i),data=Auto)
 cv.error[i]=cv.glm(Auto,glm1)$delta[1]
 }
cv.error

# k-Fold Cross-Validation

set.seed(17)
cv.error.10=rep(0,10)
for (i in 1:10){
 glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
 cv.error.10[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]
 }
cv.error.10

