# 3) Emp_data -> Build a prediction model for Churn_out_rate 


Emp_data <- read.csv("C://Excelr Data//Assignments//Simple Linear Regression//emp_data.csv") 
View(Emp_data)

attach(Emp_data)

plot(Salary_hike,Churn_out_rate)

cor(Churn_out_rate,Salary_hike) #cor(y,x)
# r = -0.9117216 is less than 0.85 negatively correlated


## Building my model
reg<-lm(Churn_out_rate ~ Salary_hike) #lm(y~x)
summary(reg)
# Multiple R-squared: 0.8312 , Adjusted R-squared:  0.8101 , pvalue: 0.0002386 < 0.05

plot(Churn_out_rate,Salary_hike)

reg$fitted.values # fited means predicted value

confint(reg,level = 0.95)

predict(reg,inteval="predict")

# Logarthmic transformation
reg_log<-lm(Churn_out_rate~log(Salary_hike))  # Regression using logarthmic transformation

summary(reg_log)
# Multiple R-squared:  0.8486 , Adjusted R-squared:  0.8297  and  p-value: 0.0001532 < 0.05

confint(reg_log,level=0.95)


predict(reg_log,interval="predict")

reg_exp<-lm(log(Churn_out_rate)~ Salary_hike) # regression using Exponential model
summary(reg_exp)
#Multiple R-squared:  0.8735 , Adjusted R-squared:  0.8577  and p-value: 7.377e-05 < 0.05

confint(reg_exp,level=0.95)


predict(reg_exp,interval="predict")


#--------------------------------------------------------------------------------------------
# Here,Multiple R-squared:  0.8735 , Adjusted R-squared:  0.8577  and  p-value: 7.377e-05 < 0.05
# That's mean this model will predict the output 87.35% time correct.

