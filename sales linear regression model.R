library(boot) 
library(car)
library(QuantPsyc)
library(lmtest)
library(sandwich)
library(vars)
library(nortest)
library(MASS)

setwd("G:\\R Language\\session 5")

data<- read.csv("Data.csv")
str(data)
summary(data)


boxplot(data$sales)
quantile(data$sales, c(0,0.05,0.1,0.25,0.5,0.75,0.90,0.95,0.99,0.995,1))

data2 <- data[data$sales <8200, ]
boxplot(data2$sales)

quantile(data2$sales, c(0,0.05,0.1,0.25,0.5,0.75,0.90,0.95,0.97,0.98,0.985,0.99,0.995,1))


nrow(data)-nrow(data2)

data3 <- data[data$sales <5800, ]

nrow(data)-nrow(data3)

boxplot(data3$sales)

data <- data3


## Check the missing value (if any)
sapply(data, function(x) sum(is.na(x)))

data <- na.omit(data)

nrow(data)
names(data)

fit<- lm(sales ~ Item_Weight+	Item_Fat_Content +	Item_Visibility +	Item_Type+	Item_MRP +	
           Outlet_Identifier +	Yrs_since_inception +	Outlet_Size +	Outlet_Location_Type +	
           Outlet_Type, data=data)
summary(fit)


fit<- lm(sales ~ Item_Weight+	Item_Fat_Content +	Item_Visibility +	Item_Type+	Item_MRP +	
           Outlet_Identifier , data=data)
summary(fit)

fit<- lm(sales ~ 		Item_Type+	Item_MRP +	
           Outlet_Identifier , data=data)
summary(fit)

##Final model 
fit<- lm(sales ~ 		I(Item_Type == "Seafood")+I(Item_Type == "Dairy")+	Item_MRP +	
           Outlet_Identifier , data=data)
summary(fit)



#Check Vif, vif>2 means presence of multicollinearity
vif(fit)
#vif is not greater than 2 so multicollinearity is not present
## Get the predicted or fitted values


data$pred
#Calculating MAPE
attach(data)
(sum((abs(sales-pred))/sales))/nrow(data)
#mape value is 0.812 , lies between 0 to 1 lesser the value better the model, but model is not that good
# Checking of Assumption
# residuals should be uncorrelated ##Autocorrelation
# Null H0: residuals from a linear regression are uncorrelated. Value should be close to 2. 
#Less than 1 and greater than 3 -> concern
## Should get a high p value
#Durbin watson test used to check serial correlation 
dwt(fit)
#durbin watson test value is 2.036716 close to 2 so NULL Hypothesis are true
# Checking multicollinearity 
vif(fit)# should be within 2. If it is greater than 10 then serious problem
# Breusch-Pagan test
# Null hypothesis -> error is homogenious (p value should be more than 0.05)
bptest(fit)
#p-value < 2.2e-16 so H0 is rejected and alternate hypothesis H1 is true i.e errors are not Homogenius but heterogenius


## Normality testing Null hypothesis is data is normal.
resids <- fit$residuals
#get Anderson-Darling test for normality (p-value should be more than 0.05)
ad.test(resids)
#p-value < 2.2e-16 H1- data is not normal or error is not normally disributed







