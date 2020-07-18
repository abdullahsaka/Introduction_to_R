# Importing package
library(ISLR)
library(ggcorrplot)

# Loading dataset
auto_data<- Auto
dim(auto_data)
head(auto_data,10)
str(auto_data)
summary(auto_data)

# Perform Exploratory Data Analysis(EDA)
uniq_year <- unique(auto_data$year) # categorical variable starting 70 to 82
uniq_origin <- unique(auto_data$origin) # categorical variable: 1 2 3
uniq_cylinders<- unique(auto_data$cylinders) # categorical variable: 3 4 5 6 8
dim(auto_data) # (dim:392 9) Data has a skinny shape.

boxplot(mpg~origin,
        data=auto_data,
        main="Boxplots for each origin",
        xlab="Origin",
        ylab="mpg",
        col="red",
        border="darkblue"
)

boxplot(mpg~year,
        data=auto_data,
        main="Boxplots for each year",
        xlab="Origin",
        ylab="mpg",
        col="orange",
        border="darkblue"
)

boxplot(mpg~cylinders,
        data=auto_data,
        main="Boxplots for each year",
        xlab="Origin",
        ylab="mpg",
        col="orange",
        border="darkblue"
)

res.aov <- aov(mpg ~ year, data = auto_data)
summary(res.aov)

res.aov <- aov(mpg ~ origin, data = auto_data)
summary(res.aov)

res.aov <- aov(mpg ~ cylinders, data = auto_data)
summary(res.aov)

# Check whether there is a strong association between features and output
ggplot(auto_data, aes(x=displacement, y=mpg)) + geom_point()
ggplot(auto_data, aes(x=horsepower, y=mpg)) + geom_point()
ggplot(auto_data, aes(x=weight, y=mpg)) + geom_point()
ggplot(auto_data, aes(x=acceleration, y=mpg)) + geom_point() # there is much randomness than other attributes

# Check correlation between numerical values to understand relationship between indepedent and dependent variable
auto_cor <- cor(auto_data[1:6]) 
ggcorrplot(auto_cor, method = "circle",
           type="lower",hc.order = TRUE,title="Correlation between features in Auto dataset")

# Apply linear regression model
linearMod1<- lm(mpg ~ cylinders+displacement+horsepower+weight+acceleration+year+origin, data=auto_data)  # build linear regression model on full data
summary(linearMod1)
linearMod1$coefficients
linearMod1$fitted.values
plot(linearMod1)

# Apply new model(Convert mpg as log function)
mpg_log<-log(auto_data$mpg)
auto_log<- auto_data
auto_log$mpg_log<- mpg_log
auto_log$mpg <- NULL
head(auto_log)
linearMod2 <- lm(mpg_log ~ cylinders+displacement+horsepower+weight+acceleration+year+origin, data=auto_data)  # build new linear regression model on full data
linearMod2$coefficients
summary(linearMod2)

ggplot(auto_log, aes(x=horsepower, y=mpg_log)) + geom_point()
ggplot(auto_log, aes(x=weight, y=mpg_log)) + geom_point()
ggplot(auto_log, aes(x=displacement, y=mpg_log)) + geom_point()

# Final model
linearMod3 <- lm(mpg_log ~ cylinders+displacement+horsepower+weight+year+origin, data=auto_data)  # build new linear regression model on full data
linearMod3$coefficients
summary(linearMod3)

# Optional Part
plot(linearMod3)





