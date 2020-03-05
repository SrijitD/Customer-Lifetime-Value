# Setting up the environment and data import
# Understanding the data
# Exploratory Data Analysis
# Linear Regression Model
# Preparation and splitting the data
# DATA PREPARATION:
#   
#   Cleaning the Categorical features
# Standardising Continuous features
# Creating derived features
# Creating dummy variables for factor variables
# Creating the final dataset
# Splitting the data into train and validation set.
# Train and Test the Model
# Train and Test New Model
# Compare the models
# Model Performance
# Applying on new data


#Loading the packages into the library
library(boot) #The boot package provides extensive facilities for bootstrapping and related resampling methods.
library(car)  #Companion to Applied Regression
library(lmtest) #Testing Linear Regression Models. A collection of tests, data sets, and examples for diagnostic checking in linear regression models. Furthermore, some generic tools for inference in parametric models are provided.
library(caTools)  #Contains several basic utility functions including: moving (rolling, running) window statistic functions, read/write for GIF and ENVI binary files, fast calculation of AUC, LogitBoost classifier, base64 encoder/decoder, round-off-error-free sum and cumsum, etc.
library(QuantPsyc) #Contains functions useful for data screening, testing moderation, mediation and estimating power.
library(MASS)  #Functions and datasets to support Venables and Ripley, "Modern Applied Statistics with S"
library(sandwich)  #Model-robust standard error estimators for cross-sectional, time series, clustered, panel, and longitudinal data.
library(vars)  #Estimation, lag selection, diagnostic testing, forecasting, causality analysis, forecast error variance decomposition and impulse response functions of VAR models and estimation of SVAR and SVEC models.
library(nortest)  #nortest: Tests for Normality. Five omnibus tests for testing the composite hypothesis of normality.
library(dplyr) #A Grammar of Data Manipulation. A fast, consistent tool for working with data frame like objects, both in memory and out of memory.
library(summarytools)  #Tools to Quickly and Neatly Summarize Data. Data frame summaries, cross-tabulations, weight-enabled frequency tables and common univariate statistics in concise tables available in a variety of formats (plain ASCII, Markdown and HTML). A good point-of-entry for exploring data, both for experienced and new R users.
library(ggplot2) #Create Elegant Data Visualisations Using the Grammar of Graphics. A system for 'declaratively' creating graphics, based on "The Grammar of Graphics". You provide the data, tell 'ggplot2' how to map variables to aesthetics, what graphical primitives to use, and it takes care of the details.
library(ggcorrplot) #Visualization of a Correlation Matrix using 'ggplot2'. The 'ggcorrplot' package can be used to visualize easily a correlation matrix using 'ggplot2'. It provides a solution for reordering the correlation matrix and displays the significance level on the plot. It also includes a function for computing a matrix of correlation p-values.

install.packages("WVPlots")
library(WVPlots)
#To get the active working directory of the current R project.
getwd()
#To set the working directory to a specific location where we want to save our project and fetch our data from.
setwd('C:\\Users\\SRIJIT\\Desktop\\R')
#To import the data from the working directory into R and load into data object here
data<-read.csv('MCVA.csv')
#Remove the CustomerID & Effective.to.date as it doesnt make any sense in predicting the value of CLTV.
data<-data[,c(-1,-7)]
#To get the whole glimpse of the data. The data story.
glimpse(data)
#Converting the required and necessary categorical variable from int to factors
data$Number.of.Open.Complaints<-as.factor(data$Number.of.Open.Complaints)
data$Number.of.Policies<-as.factor(data$Number.of.Policies)
#To check the sanity of the dataset
str(data)
#Checking the data summary for mean,median,mode and outlier.
summary(data)
#To get the skewness,kurtosis,std deviation of the variable from the dataset
descr(data)
#To check if there is NA value in the dataset
na_counts <- sapply(data, function(y) sum(is.na(y)))
na_counts
#Making na_counts as a dataframe
na_counts <- data.frame(na_counts)
na_counts
#To clean the data and remove NA value(if any)
data <- na.omit(data)
#Again checking the data summary
summary(data)

# The distribution of CLV is positively skewed (as expected) and is heavily Leptokurtic. 
# A distribution that is heavily skewed with a very large tail. 
# There are a LOT of Customers with low CLV. 
# Very few customers with high CLV. 
# This can be visually understood using the Histogram.

hist(data$Customer.Lifetime.Value, col = "blue")

hist(data$Customer.Lifetime.Value, breaks = (max(data$Customer.Lifetime.Value) - min(data$Customer.Lifetime.Value))/100, freq = FALSE, main = "CLV Histogram", border = "red")

# Monthly premiums follow a trend similar to CLV although the distribution is NOT as skewed or as long tailed as CLV. 
# This can be visually seen in the Histogram

hist(data$Monthly.Premium.Auto, col = "green")

# Total Claim amounts also follow a trend similar to CLV and MPA although the distribution is NOT as skewed or as long tailed as MPA. This can be visually seen in the Histogram. 
# This means that variation in data is CLV > MPA > TCA

hist(data$Total.Claim.Amount, col = "orange")

# We can conclude from this that we are very far away from normal distributions and 
# maybe if we want to model these variable we should think of a variable transformation

names(data)


# Checking the correlation between continuous variables
# 
# It seems that the continuous variables are not correlated

options(repr.plot.width =6, repr.plot.height = 4)
telco_cor <- round(cor(data[,c("Customer.Lifetime.Value", "Monthly.Premium.Auto", "Total.Claim.Amount")]), 1)

ggcorrplot(telco_cor,  title = "Correlation")+theme(plot.title = element_text(hjust = 0.5))

#Checking the boxplot of each numerical(int) variable for outlier detection and treatment
#Checking for outliers in the continuous variables, and it seems none of the values are beyond the whiskers here.

boxplot(data$Customer.Lifetime.Value)
quantile(data$Customer.Lifetime.Value, c(0,0.05,0.1,0.25,0.5,0.75,0.90,0.95,0.99,0.995,1))
quantile(data$Customer.Lifetime.Value,seq(0.90,0.95,0.005))
data1<-data[data$Customer.Lifetime.Value<15000,]
boxplot(data1$Customer.Lifetime.Value)
boxplot(data1$Monthly.Premium.Auto)
quantile(data1$Monthly.Premium.Auto, c(0,0.05,0.1,0.25,0.5,0.75,0.90,0.95,0.99,0.995,1))
data2<-data1[data1$Monthly.Premium.Auto<200,]
boxplot(data2$Monthly.Premium.Auto)
boxplot(data2$Total.Claim.Amount)
quantile(data1$Total.Claim.Amount, c(0,0.05,0.1,0.25,0.5,0.75,0.90,0.95,0.99,0.995,1))
data3<-data2[data2$Total.Claim.Amount<1000,]
boxplot(data3$Total.Claim.Amount)
data<-data3

# Splitting the data into train and validation data.

set.seed(123)
indices = sample.split(data$Customer.Lifetime.Value, SplitRatio = 0.8)
train = data[indices,]
validation = data[!(indices),]

#Running the Linear Regression Model based on training data and removing insignificant variable whose alpha(p) is more than .05
# MODEL BUILDING
# Starting with Linear Regression


train_model<-lm(formula = Customer.Lifetime.Value~ State+ Response+ Coverage+ Education+ EmploymentStatus+ Gender+
                  Income+ Location.Code+ Marital.Status+ Monthly.Premium.Auto+ Months.Since.Last.Claim+ Months.Since.Policy.Inception+
                  Number.of.Open.Complaints+ Number.of.Policies+ Policy.Type+ Policy+ Renew.Offer.Type+ Sales.Channel+ 
                  Total.Claim.Amount+ Vehicle.Class+ Vehicle.Size, data = train)
summary(train_model)

train_model<-lm(formula = Customer.Lifetime.Value~ Response+ Coverage+ Education+ EmploymentStatus+ Gender+
                  Income+ Location.Code+ Marital.Status+ Monthly.Premium.Auto+ Months.Since.Last.Claim+ Months.Since.Policy.Inception+
                  Number.of.Open.Complaints+ Number.of.Policies+ Policy.Type+ Policy+ Renew.Offer.Type+ Sales.Channel+ 
                  Total.Claim.Amount+ Vehicle.Class+ Vehicle.Size, data = train)
summary(train_model)

train_model<-lm(formula = Customer.Lifetime.Value~  Coverage+ Education+ EmploymentStatus+ Gender+
                  Income+ Location.Code+ Marital.Status+ Monthly.Premium.Auto+ Months.Since.Last.Claim+ Months.Since.Policy.Inception+
                  Number.of.Open.Complaints+ Number.of.Policies+ Policy.Type+ Policy+ Renew.Offer.Type+ Sales.Channel+ 
                  Total.Claim.Amount+ Vehicle.Class+ Vehicle.Size, data = train)
summary(train_model)

train_model<-lm(formula = Customer.Lifetime.Value~ Education+ EmploymentStatus+ Gender+
                  Income+ Location.Code+ Marital.Status+ Monthly.Premium.Auto+ Months.Since.Last.Claim+ Months.Since.Policy.Inception+
                  Number.of.Open.Complaints+ Number.of.Policies+ Policy.Type+ Policy+ Renew.Offer.Type+ Sales.Channel+ 
                  Total.Claim.Amount+ Vehicle.Class+ Vehicle.Size, data = train)
summary(train_model)


train_model<-lm(formula = Customer.Lifetime.Value~ Education+ I(EmploymentStatus=='Employed')+I(EmploymentStatus=='Unemployed')+ Gender+
                  Income+ Location.Code+ Marital.Status+ Monthly.Premium.Auto+ Months.Since.Last.Claim+ Months.Since.Policy.Inception+
                  Number.of.Open.Complaints+ Number.of.Policies+ Policy.Type+ Policy+ Renew.Offer.Type+ Sales.Channel+ 
                  Total.Claim.Amount+ Vehicle.Class+ Vehicle.Size, data = train)
summary(train_model)

train_model<-lm(formula = Customer.Lifetime.Value~ Education+ I(EmploymentStatus=='Employed')+I(EmploymentStatus=='Unemployed')+ Gender+
                  Income+ I(Marital.Status=='Married')+ Monthly.Premium.Auto+ Months.Since.Last.Claim+ Months.Since.Policy.Inception+
                  Number.of.Open.Complaints+ Number.of.Policies+ Policy.Type+ Policy+ Renew.Offer.Type+ Sales.Channel+ 
                  Total.Claim.Amount+ Vehicle.Class+ Vehicle.Size, data = train)
summary(train_model)

train_model<-lm(formula = Customer.Lifetime.Value~ Education+ I(EmploymentStatus=='Employed')+I(EmploymentStatus=='Unemployed')+ Gender+
                  Income+ I(Marital.Status=='Married')+ Monthly.Premium.Auto+  Months.Since.Policy.Inception+
                  I(Number.of.Open.Complaints==4)+I(Number.of.Open.Complaints==5)+ Number.of.Policies+  I(Vehicle.Class=='Luxury Car')+I(Vehicle.Class=='Luxury SUV'), data = train)
summary(train_model)

# We can use variance inflation factor (vif) to get rid of redundant predictors or the variables that have high multicollinearity between them. 
# Multicollinearity exists when two or more predictor variables are highly related to each other and then it becomes difficult to understand the impact of an independent variable on the dependent variable.
# The Variance Inflation Factor(VIF) is used to measure the multicollinearity between predictor variables in a model. 
# A predictor having a VIF of 2 or less is generally considered safe and it can be assumed that it is not correlated with other predictor variables. Higher the VIF, greater is the correlation of the predictor variable w.r.t other predictor variables. However, Predictors with high VIF may have high p-value(or highly significant), hence, we need to see the significance of the Predictor variable before removing it from our model.




vif(train_model)

train_model<-lm(formula = Customer.Lifetime.Value~ Education+ Gender+
                  Income+ I(Marital.Status=='Married')+ Monthly.Premium.Auto+  Months.Since.Policy.Inception+
                  I(Number.of.Open.Complaints==4)+I(Number.of.Open.Complaints==5)+ Number.of.Policies+  I(Vehicle.Class=='Luxury Car')+I(Vehicle.Class=='Luxury SUV'), data = train)
summary(train_model)
vif(train_model)

#Saving R-squared
r_sq_train <- summary(train_model)$r.squared

#predict data on validation set
prediction_test <- predict(train_model, newdata = validation)
summary(prediction_test)
validation$predicted<-prediction_test

#calculating the residuals
residuals <- validation$Customer.Lifetime.Value - prediction_test

#calculating Root Mean Squared Error
rmse <- sqrt(mean(residuals^2))

#Model Performance
#Plotting linear model for predicted and real values
validation$prediction <- predict(train_model, newdata = validation)
ggplot(validation, aes(x = prediction, y = Customer.Lifetime.Value)) + 
  geom_point(color = "blue", alpha = 0.7) + 
  geom_abline(color = "red") +
  ggtitle("Prediction vs. Real values")

#Histogram of residuals
ggplot(validation, aes(x = residuals)) + 
  geom_histogram(bins = 15, fill = "blue") +
  ggtitle("Histogram of residuals")

validation$residuals <- validation$Customer.Lifetime.Value - validation$prediction

ggplot(data = validation, aes(x = prediction, y = residuals)) +
  geom_pointrange(aes(ymin = 0, ymax = residuals), color = "blue", alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = 3, color = "red") +
  ggtitle("Residuals vs. Linear model prediction")



# Calculation of MAPE(Mean Absolute Percentage Error)
# Here it is 10.62% which is in range and we can take it.
# Get the predicted or fitted values

fitted(train_model)
train$Predicted <- fitted(train_model)
train<-na.omit(train)
attach(train)
((sum((abs(Customer.Lifetime.Value-Predicted))/Customer.Lifetime.Value))/nrow(train))*100


#Darwin watson test for AutoCorrelation or Serial Correlation. 
# p-valueshould be greater than alpha to reject H0(null) hypothesis.
#Here it is 0.64 which is taken.
dwt(train_model)

#Finding the residuals from original to predicted value

resids <- train_model$residuals

#Anderson derling Test for Normality checking.
#Here alpha is less than .05 which violates the H1. H0 Accepted, The residuals not normally distributred.
ad.test(resids)
#Breusch-pegan test
#It is not homoscedastic, heteroscedastic. Since the pvalue is less than alpha(.05). This also violates the H1 hypothesis i.e., H0 is accepted.
bptest(train_model)

# Plot the cumulative gain curve of a sort-order.
names(validation)
GainCurvePlot(validation, "prediction", "Customer.Lifetime.Value", "Model")

# We can see the errors in the model are close to zero so model predicts quite well.


