#Importing package
library(ISLR)
library(InformationValue)
library(ROCR) 
library(caret)
library(Metrics)
library(ggcorrplot)
library(PRROC)

#Carry out descriptive data analysis(EDA)
weekly <- Weekly
dim(weekly)
str(weekly)
summary(weekly)
sum(is.na(weekly))

week_corr <- cor(weekly[,2:8])
ggcorrplot(week_corr, method="circle",type = "lower")

#Splitting data into training and testing
train_set <- weekly[(weekly$Year>=1990) & (weekly$Year<=2008),]
test_set <- weekly[(weekly$Year>=2009) & (weekly$Year<=2010),]

#Run a Logistic Regression (part c)
model <- glm(Direction~ Lag1 + Lag2 + 
               Lag3 + Lag4 + Lag5 + Volume, 
               data=train_set, family=binomial(link="logit"))
summary(model) 

pred_train <- predict(model, train_set, type="response")
pred_train[1:5] # probability of first five instances
pred_train <- ifelse(pred_train > 0.5,"Up", "Down") # default threshold is 0.5
traincf <- table(pred_train,train_set$Direction) # look at confusion matrix
traincf

predicted <- predict(model, test_set, type="response")
predicted[1:5] # probability of first five instances
predicted <- ifelse(predicted > 0.5,"Up", "Down") # default threshold is 0.5
cf <- table(predicted,test_set$Direction) # look at confusion matrix
result <- confusionMatrix(cf) #accuracy is 46.15%
result$byClass
F_meas(cf)
cf

#Carry out different models (part d)
model_1 <- glm(Direction~ Lag1, 
             data=train_set, family=binomial(link="logit"))
summary(model_1) 

pred_train <- predict(model_1, train_set, type="response")
pred_train[1:5] # probability of first five instances
pred_train <- ifelse(pred_train > 0.5,"Up", "Down") # default threshold is 0.5
traincf_1 <- table(pred_train,train_set$Direction) # look at confusion matrix
confusionMatrix(traincf_1) #accuracy of train is 55.43%

pred_1 <- predict(model_1, test_set, type="response")
pred_1[1:5] # probability of first five instances
pred_1 <- ifelse(pred_1 > 0.5,"Up", "Down") # default threshold is 0.5
cf_1 <- table(pred_1,test_set$Direction) # look at confusion matrix
#confusionMatrix(cf_1) #accuracy of test is 56.73%
precision(cf_1)
recall(cf_1) 
F_meas(cf_1)
#-------------------------------------------------
model_2 <- glm(Direction~ Lag2, 
               data=train_set, family=binomial(link="logit"))
summary(model_2) 

pred_train2 <- predict(model_2, train_set, type="response")
pred_train2[1:5] # probability of first five instances
pred_train2 <- ifelse(pred_train2 > 0.5,"Up", "Down") # default threshold is 0.5
traincf_2 <- table(pred_train2,train_set$Direction) # look at confusion matrix
confusionMatrix(traincf_2) #accuracy of train is 55.53%
F_meas(traincf_2)

pred_2 <- predict(model_2, test_set, type="response")
pred_2[1:5] # probability of first five instances
pred_2 <- ifelse(pred_2 > 0.5,"Up", "Down") # default threshold is 0.5
cf_2 <- table(pred_2, test_set$Direction) # look at confusion matrix
confusionMatrix(cf_2) #accuracy is 62.5%
F_meas(cf_2)
#-------------------------------------------------
model_3 <- glm(Direction~ Lag3, 
               data=train_set, family=binomial(link="logit"))
summary(model_3) 

pred_train3 <- predict(model_3, train_set, type="response")
pred_train3[1:5] # probability of first five instances
pred_train3 <- ifelse(pred_train3 > 0.5,"Up", "Down") # default threshold is 0.5
traincf_3 <- table(pred_train3,train_set$Direction) # look at confusion matrix
mean(pred_train3==train_set$Direction) #accuracy of train is 55.23%

pred_3 <- predict(model_3, test_set, type="response")
pred_3[1:5] # probability of first five instances
pred_3 <- ifelse(pred_3 > 0.5,"Up", "Down") # default threshold is 0.5
cf_3 <- table(pred_3,test_set$Direction) # look at confusion matrix
cf_3
mean(pred_3==test_set$Direction)  #accuracy is 58.65%
#F_meas(cf_3)
#-------------------------------------------------
model_4 <- glm(Direction~ Lag4, 
               data=train_set, family=binomial(link="logit"))
summary(model_4) 

pred_train4 <- predict(model_4, train_set, type="response")
pred_train4[1:5] # probability of first five instances
pred_train4 <- ifelse(pred_train4 > 0.5,"Up", "Down") # default threshold is 0.5
traincf_4 <- table(pred_train4,train_set$Direction) # look at confusion matrix
mean(pred_train4==train_set$Direction) #accuracy of train is 55.3%

pred_4 <- predict(model_4, test_set, type="response")
pred_4[1:5] # probability of first five instances
pred_4 <- ifelse(pred_4 > 0.5,"Up", "Down") # default threshold is 0.5
cf_4 <- table(pred_4,test_set$Direction) # look at confusion matrix
cf_4
mean(pred_4==test_set$Direction) #accuracy is 58.65%
#F_meas(cf_4)
#-------------------------------------------------
model_5 <- glm(Direction~ Lag5, 
               data=train_set, family=binomial(link="logit"))
summary(model_5) 

pred_train5 <- predict(model_5, train_set, type="response")
pred_train5[1:5] # probability of first five instances
pred_train5 <- ifelse(pred_train5 > 0.5,"Up", "Down") # default threshold is 0.5
traincf_5 <- table(pred_train5,train_set$Direction) # look at confusion matrix
confusionMatrix(traincf_5) #accuracy of train is 55.03%

pred_5 <- predict(model_5, test_set, type="response")
pred_5[1:5] # probability of first five instances
pred_5 <- ifelse(pred_5 > 0.5,"Up", "Down") # default threshold is 0.5
cf_5 <- table(pred_5,test_set$Direction) # look at confusion matrix
result5<-confusionMatrix(cf_5) #accuracy is 55.77%
result5$byClass
F_meas(cf_5) #Cannot calculate since precision is 0
#-------------------------------------------------
pred_full <- predict(model, test_set, type="response")
roc_full <- roc.curve(scores.class0 = pred_full, scores.class1 = 1-pred_full, curve = T)
plot(roc_full,color='black', main="ROC Curves")

pred_1 <- predict(model_1, test_set, type="response")
roc_1 <- roc.curve(scores.class0 = pred_1, scores.class1 = 1-pred_1, curve = T)
plot(roc_1,color='red', add=TRUE)

pred_2 <- predict(model_2, test_set, type="response")
roc_2 <- roc.curve(scores.class0 = pred_2, scores.class1 = 1-pred_2, curve = T)
plot(roc_2,color='green',add=TRUE)

pred_3 <- predict(model_3, test_set, type="response")
roc_3 <- roc.curve(scores.class0 = pred_3, scores.class1 = 1-pred_3, curve = T)
plot(roc_3,color='blue',add=TRUE)

pred_4 <- predict(model_4, test_set, type="response")
roc_4 <- roc.curve(scores.class0 = pred_4, scores.class1 = 1-pred_4, curve = T)
plot(roc_4,color='yellow',add=TRUE)

pred_5 <- predict(model_5, test_set, type="response")
roc_5 <- roc.curve(scores.class0 = pred_5, scores.class1 = 1-pred_5, curve = T)
plot(roc_5,color='pink',add=TRUE)

legend(0.2,0.8, legend=c("Full Model", "Lag 1", "Lag 2", "Lag 3", "Lag 4", "Lag 5"),
       col=c("black", "red", "green", "blue", "yellow", "pink"), lty=1, cex=0.8)

#-------------------------------------------------

pr_1 <- pr.curve(scores.class0 = pred_1, scores.class1 = 1-pred_1, curve = T)
plot(pr_1,color='red')

pr_2 <- pr.curve(scores.class0 = pred_2, scores.class1 = 1-pred_2, curve = T)
plot(pr_2,color='darkblue')

pr_3 <- pr.curve(scores.class0 = pred_3, scores.class1 = 1-pred_3, curve = T)
plot(pr_3,color='green')

pr_4 <- pr.curve(scores.class0 = pred_4, scores.class1 = 1-pred_4, curve = T)
plot(pr_4,color='black')

pr_5 <- pr.curve(scores.class0 = pred_5, scores.class1 = 1-pred_5, curve = T)
plot(pr_5,color='yellow')

AUC <- c(pr_1$auc.integral, pr_2$auc.integral, pr_3$auc.integral, pr_4$auc.integral, pr_5$auc.integral)
Model <- c("Lag 1", "Lag 2", "Lag 3", "Lag 4", "Lag 5")
x <- cbind(AUC, Model)
x
