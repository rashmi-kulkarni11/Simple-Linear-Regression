#2) Delivery_time -> Predict delivery time using sorting time 

deliverytime <- read.csv("C://Excelr Data//Assignments//Simple Linear Regression//delivery_time.csv") 
View(deliverytime)

attach(deliverytime)

plot(Delivery.Time,Sorting.Time)

cor(Sorting.Time,Delivery.Time) #cor(y,x)
# r = 0.8259973 is less than 0.85 poorly correlated


## Building my model
reg<-lm(Sorting.Time~Delivery.Time) #lm(y~x)
summary(reg)
# Multiple R-squared: 0.6823 , Adjusted R-squared:  0.6655 , pvalue: 3.983e-06< 0.05

plot(Sorting.Time,Delivery.Time)

reg$fitted.values # fited means predicted value

confint(reg,level = 0.95)

predict(reg,inteval="predict")

# Logarthmic transformation
reg_log<-lm(Sorting.Time~log(Delivery.Time))  # Regression using logarthmic transformation

summary(reg_log)
# Multiple R-squared:  0.7109 , Adjusted R-squared:  0.6957  and  p-value: 1.593e-06 < 0.05

confint(reg_log,level=0.95)


predict(reg_log,interval="predict")

reg_exp<-lm(log(Sorting.Time)~Delivery.Time) # regression using Exponential model
summary(reg_exp)
#Multiple R-squared:  0.6954 , Adjusted R-squared:  0.6794 and p-value: 2.642e-06 < 0.05

confint(reg_exp,level=0.95)


predict(reg_exp,interval="predict")


#--------------------------------------------------------------------------------------------
# Multiple R-squared:  0.7109 , Adjusted R-squared:  0.6957  and  p-value: 1.593e-06 < 0.05
# So, X variable is significance That's mean this model will predict the output 71.09% time correct.

