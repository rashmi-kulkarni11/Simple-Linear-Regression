#1) Calories_consumed-> predict weight gained using calories consumed
#2) Delivery_time -> Predict delivery time using sorting time 
#3) Emp_data -> Build a prediction model for Churn_out_rate 
#4) Salary_hike -> Build a prediction model for Salary_hike

#Do the necessary transformations for input variables for getting better R^2 value for the model prepared.

calories <- read.csv("C://Excelr Data//Assignments//Simple Linear Regression//calories_consumed.csv") 

View(calories)

attach(calories)

plot(Weight_gained,Calories_Consumed)
# Correlation coefficient value for Weight gained in grams and calories consumed

cor(Calories_Consumed,Weight_gained) #cor(y,x)
# r = 0.946991 > 0.85 then it is stongly correlated

## Building my model
reg<-lm(Calories_Consumed~Weight_gained) #lm(y~x)
summary(reg)
# Multiple R-squared: 0.8968, Adjusted R-squared:  0.8882,p-value: 2.856e-07

plot(Weight_gained,Calories_Consumed)
  
reg$fitted.values # fited means predicted value

confint(reg,level = 0.95)

predict(reg,inteval="predict")

#--------------------------------------------------------------------------------------------
# Logarthmic transformation
reg_log<-lm(Calories_Consumed~log(Weight_gained)) # Regression using logarthmic transformation
summary(reg_log)
# Multiple R-squared:  0.8776 , Adjusted R-squared:  0.8674 ,p-value: 8.018e-07

confint(reg_log,level=0.95)


predict(reg_log,interval="predict")


reg_exp<-lm(log(Calories_Consumed)~Weight_gained) # regression using Exponential model
summary(reg_exp)
#Multiple R-squared:  0.8077,	Adjusted R-squared:  0.7917 ,p-value: 1.248e-05

confint(reg_exp,level=0.95)

predict(reg_exp,interval="predict")

#------------------------------------------------------------------------------------------
#Hence here the p-value: 2.856e-07 < 0.05 So, X varibale is significance and Multiple R-Square value is 0.8968
# and Adjusted R-squared:  0.8882 That's mean this model will predict the output 89.68% time correct.
