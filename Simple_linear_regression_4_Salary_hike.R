# 4) Salary_hike -> Build a prediction model for Salary_hike


Salary_hike <- read.csv("C://Excelr Data//Assignments//Simple Linear Regression//Salary_Data.csv") 
View(Salary_hike)

attach(Salary_hike)

plot(YearsExperience,Salary)

cor(Salary,YearsExperience) #cor(y,x)
# r = 0.9782416 is greater than 0.85 positively correlated


## Building my model
reg<-lm(Salary ~ YearsExperience) #lm(y~x)

summary(reg)
# Multiple R-squared: 0.957 , Adjusted R-squared:  0.9554  , pvalue: 2.2e-16 < 0.05

plot(Salary,YearsExperience)

reg$fitted.values # fited means predicted value

confint(reg,level = 0.95)

predict(reg,inteval="predict")

# Logarthmic transformation
reg_log<-lm(Salary~log(YearsExperience))  # Regression using logarthmic transformation

summary(reg_log)
# Multiple R-squared:  0.8539 , Adjusted R-squared:  0.8487   and  p-value: 3.25e-13 < 0.05

confint(reg_log,level=0.95)


predict(reg_log,interval="predict")

reg_exp<-lm(log(Salary)~ YearsExperience) # regression using Exponential model

summary(reg_exp)
#Multiple R-squared:  0.932 , Adjusted R-squared:  0.9295  and p-value: 2.2e-16 < 0.05

confint(reg_exp,level=0.95)


predict(reg_exp,interval="predict")

#--------------------------------------------------------------------------------------------
#Here, Multiple R-squared:  0.957 , Adjusted R-squared:  0.9554   and  p-value: 2.2e-16 < 0.05
# So, X is significance That's mean this model will predict the output 95.7% time correct.

