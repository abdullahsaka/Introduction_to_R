install.packages("DMwR2")
library(DMwR2)
data(algae)
str(algae)
summary(algae)
mean(algae$Cl)
mean(algae$Cl,na.rm=TRUE)
## missing values
## 1. Remove the instances with missing values
## when the proportion of instances with missing values is small wrt the size
complete.cases(algae)
!complete.cases(algae)
sum(!complete.cases(algae))
algae2=na.omit(algae)
summary(algae2)
apply(algae,1,function(x) sum(is.na(x)))
manyNAs(algae,0.2)
manyNAs(algae,1)
manyNAs(algae,2)
algae=algae[-manyNAs(algae),]
## Fill in the missing values with the most frequent value
algae[48,]
summary(algae$mxPH)
hist(algae$mxPH)
algae[48,"mxPH"]=mean(algae$mxPH,na.rm=TRUE)
summary(algae$Chla)
hist(algae$Chla)
algae[is.na(algae$Chla),"Chla"]=median(algae$Chla,na.rm=TRUE)
data(algae)
algae=algae[-manyNAs(algae),]
algae=centralImputation(algae)
summary(algae)

## Fill in the missing values by exploring the correlations btw attributes
data(algae)
cor(algae[,4:18])
cor(algae[,4:18],use="complete.obs")
install.packages("corrplot")
library(corrplot)
cm=cor(algae[,4:18],use="complete.obs")
corrplot(cm,type="upper",tl.pos="d")
corrplot(cm,add=TRUE, type="lower",method="number",
diag=FALSE,tl.pos="n",cl.pos="n")
data(algae)
algae=algae[-manyNAs(algae),]
lm(PO4~oPO4, data=algae)
algae[28,"PO4"]=42.897+1.293*algae[28,"oPO4"]
