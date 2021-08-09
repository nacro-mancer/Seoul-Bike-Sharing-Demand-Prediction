#Simple Linear Regression
#Wine Data


#import dataset
wine_data <- read.csv(choose.files())
wine_data
dim(wine_data)
str(wine_data)

# remove density as it is almost same
wine_data <- wine_data[,-8]
str(wine_data)
#Check missing data
colSums(is.na(wine_data))
# There is no missing data

#Check Outliers
names(wine_data)

head(wine_data)

boxplot(wine_data$fixed.acidity)
Q <- quantile(wine_data$fixed.acidity, probs = c(.25,.75))
iqr <- IQR(wine_data$fixed.acidity)
wine_data$fixed.acidity <- ifelse(wine_data$fixed.acidity >=Q[2]+1.5*iqr,Q[2]+1.5*iqr,wine_data$fixed.acidity)
wine_data$fixed.acidity <- ifelse(wine_data$fixed.acidity <=Q[1]-1.5*iqr,Q[1]-1.5*iqr,wine_data$fixed.acidity)
boxplot(wine_data$fixed.acidity)

boxplot(wine_data$volatile.acidity)
summary(wine_data$volatile.acidity)
Q1 = 0.3900
Q3 = 0.6400
IQR = Q3-Q1
pos_out1 = Q3+1.5*IQR
pos_out1
wine_data$volatile.acidity <- ifelse(wine_data$volatile.acidity <= pos_out1, pos_out1, wine_data$volatile.acidity)
neg_out1 = Q1-1.5*IQR
neg_out1
wine_data$volatile.acidity <- ifelse(wine_data$volatile.acidity >= neg_out1, neg_out1, wine_data$volatile.acidity)
boxplot(wine_data$volatile.acidity)

boxplot(wine_data$citric.acid)
summary(wine_data$citric.acid)
Q1 = 0.090
Q3 = 0.420
IQR = Q3-Q1
pos_out1 = Q3+1.5*IQR
pos_out1
wine_data$citric.acid <- ifelse(wine_data$citric.acid <= pos_out1, 0.900, wine_data$citric.acid)
neg_out1 = Q1-1.5*IQR
neg_out1
wine_data$citric.acid <- ifelse(wine_data$citric.acid >= neg_out1, -0.400, wine_data$citric.acid)

boxplot(wine_data$residual.sugar)
summary(wine_data$residual.sugar)
Q1 = 1.900
Q3 = 2.600
IQR = Q3-Q1
pos_out1 = Q3+1.5*IQR
pos_out1
wine_data$residual.sugar <- ifelse(wine_data$residual.sugar <= pos_out1, 3.5, wine_data$residual.sugar)
neg_out1 = Q1-1.5*IQR
neg_out1
wine_data$residual.sugar <- ifelse(wine_data$residual.sugar >= neg_out1, 1, wine_data$residual.sugar)

boxplot(wine_data$chlorides)
summary(wine_data$chlorides)
Q1 = 0.07000
Q3 = 0.09000
IQR = Q3-Q1
pos_out1 = Q3+1.5*IQR
pos_out1
wine_data$chlorides <- ifelse(wine_data$chlorides <= pos_out1, 0.1, wine_data$chlorides)
neg_out1 = Q1-1.5*IQR
neg_out1
wine_data$chlorides <- ifelse(wine_data$chlorides >= neg_out1, 0.05, wine_data$chlorides)

boxplot(wine_data$free.sulfur.dioxide)
summary(wine_data$free.sulfur.dioxide)
Q1 = 7.00
Q3 = 21.0
IQR = Q3-Q1
pos_out1 = Q3+1.5*IQR
pos_out1
wine_data$free.sulfur.dioxide <- ifelse(wine_data$free.sulfur.dioxide <= pos_out1, 40, wine_data$free.sulfur.dioxide)
neg_out1 = Q1-1.5*IQR
neg_out1
wine_data$free.sulfur.dioxide <- ifelse(wine_data$free.sulfur.dioxide >= neg_out1, -10, wine_data$free.sulfur.dioxide)

boxplot(wine_data$total.sulfur.dioxide)
summary(wine_data$total.sulfur.dioxide)
Q1 = 22
Q3 = 62
IQR = Q3-Q1
pos_out1 = Q3+1.5*IQR
pos_out1
wine_data$total.sulfur.dioxide <- ifelse(wine_data$total.sulfur.dioxide <= pos_out1, 100, wine_data$total.sulfur.dioxide)
neg_out1 = Q1-1.5*IQR
neg_out1
wine_data$total.sulfur.dioxide <- ifelse(wine_data$total.sulfur.dioxide >= neg_out1, -20, wine_data$total.sulfur.dioxide)

#boxplot(wine_data$density)
#summary(wine_data$density)
#Q1 = 0.9956
#Q3 = 0.9978
#IQR = Q3-Q1
#pos_out1 = Q3+1.5*IQR
#pos_out1
#wine_data$density <- ifelse(wine_data$density <= pos_out1, 1, wine_data$density)
#neg_out1 = Q1-1.5*IQR
#neg_out1
#wine_data$density <- ifelse(wine_data$density >= neg_out1, 1, wine_data$density)

boxplot(wine_data$pH)
summary(wine_data$pH)
Q1 = 3.210
Q3 = 3.400
IQR = Q3-Q1
pos_out1 = Q3+1.5*IQR
pos_out1
wine_data$pH <- ifelse(wine_data$pH <= pos_out1, 3.5, wine_data$pH)
neg_out1 = Q1-1.5*IQR
neg_out1
wine_data$pH <- ifelse(wine_data$pH >= neg_out1, 3, wine_data$pH)

boxplot(wine_data$sulphates)
summary(wine_data$sulphates)
Q1 = 0.5500
Q3 = 0.7300
IQR = Q3-Q1
pos_out1 = Q3+1.5*IQR
pos_out1
wine_data$sulphates <- ifelse(wine_data$sulphates <= pos_out1, 1, wine_data$sulphates)
neg_out1 = Q1-1.5*IQR
neg_out1
wine_data$sulphates <- ifelse(wine_data$sulphates >= neg_out1,0.30, wine_data$sulphates)

boxplot(wine_data$alcohol)
summary(wine_data$alcohol)
Q1 = 9.50
Q3 = 11.10
IQR = Q3-Q1
pos_out1 = Q3+1.5*IQR
pos_out1
wine_data$alcohol <- ifelse(wine_data$alcohol <= pos_out1, 12, wine_data$alcohol)
neg_out1 = Q1-1.5*IQR
neg_out1
wine_data$alcohol <- ifelse(wine_data$alcohol >= neg_out1, 8, wine_data$alcohol)

# Outlier exists for all of the columns

head(wine_data)

# outliers removed
boxplot(wine_data$fixed.acidity)
boxplot(wine_data$volatile.acidity)
boxplot(wine_data$citric.acid)
boxplot(wine_data$residual.sugar)
boxplot(wine_data$chlorides)
boxplot(wine_data$free.sulfur.dioxide)
boxplot(wine_data$total.sulfur.dioxide)
boxplot(wine_data$density)
boxplot(wine_data$pH)
boxplot(wine_data$sulphates)
boxplot(wine_data$alcohol)
## Splitting the data into training and test

library(caTools)

set.seed(123)

split <- sample.split(wine_data$quality,SplitRatio = 0.75)
split
table(split)
training <- subset(wine_data,split==TRUE)
nrow(training)
test <- subset(wine_data, split==FALSE)
nrow(test)

# Now we have to build Linear regression model
linear_reg <- lm(quality~.,data = training)
linear_reg
summary(linear_reg)

# my model is absolutely good as Adjusted R-squared:  0.9131 and all variable are statically significant 

reg_pred <- predict(linear_reg, newdata = test)
reg_pred

# combind actual price vs predicted price
reg_pred_cbind <- cbind(test$quality, reg_pred)
reg_pred_cbind

# Model buidling and prediction completed, now we have to check assumption

# assumption 1 :data should be a linear line - satisfied
plot(test$quality, col='blue', type='b', lty=10.7)
lines(reg_pred, col='red', type='o', lty=12.9)

install.packages("lmtest")
library(lmtest)
install.packages("faraway")
library(faraway)
dwtest(linear_reg)

vif(linear_reg)
