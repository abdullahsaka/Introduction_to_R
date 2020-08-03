library(readr)
library(dplyr)
library(party)
library(rpart)
library(rpart.plot)
library(ROCR)
library(partykit)
library(pROC)

set.seed(100)

titanic3 <- "https://goo.gl/At238b" %>%
  read_csv %>% # read in the data
  select(survived, embarked, sex,sibsp, parch, fare) %>%
  mutate(embarked = factor(embarked),sex = factor(sex))
#load("/Users/robertness/Downloads/titanic.Rdata")

#pairs(titanic3~.survived,lower.panel = NULL)


.data <- c("training", "test") %>%
  sample(nrow(titanic3), replace = T) %>%
  split(titanic3, .)

rtree_fit <- rpart(survived ~ .,.data$training) 
rpart.plot(rtree_fit)

tree_fit <- ctree(survived ~ .,data = .data$training)
plot(tree_fit)

tree_roc <- tree_fit %>%
  predict(newdata = .data$test) %>%
  prediction(.data$test$survived) %>%
  performance("tpr", "fpr")
rtree_roc <- rtree_fit %>%
  predict(newdata = .data$test) %>%
  prediction(.data$test$survived) %>%
  performance("tpr", "fpr")



plot(tree_roc)
plot(rtree_roc)

plot(tree_roc, col="blue")
#plot(tree_roc, colorize = TRUE)
plot(rtree_roc, add = TRUE, col="red")

legend("bottomright", inset=.05,col=c("blue","red"),
       legend=c("decision tree","conditional tree"), horiz=FALSE)


