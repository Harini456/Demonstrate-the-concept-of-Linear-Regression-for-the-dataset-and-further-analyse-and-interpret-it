#1.Import the dataset data_marketing_budget_mo12 and do the exploratory data analysis .
#data=data_marketing_budget
library(ggplot2)
#ggplot(data=data,aes(x=Month,y=Spend)) + geom_bar(stat ='identity',aes(fill=Spend))+ coord_flip() + theme_grey() + scale_fill_gradient(name="Monthly Spending")+labs(title = 'Monthly Spending',y='Spend',x='Month')
#ggplot(data=data,aes(x=Month,y=Sales)) + geom_bar(stat ='identity',aes(fill=Spend))+ coord_flip() + theme_grey() + scale_fill_gradient(name="Monthly Sales")+labs(title = 'Monthly Sales',y='Sales',x='Month')


#2. Use Scatter Plot To Visualize The Relationship
scatter.smooth(x=data$Month, y=data$Spend, main="Month ~ Spend")
#As we can see in the month of September the Spend value is more and in the month of January the Spend is low
scatter.smooth(x=data$Month, y=data$Sales, main="Month ~ Sales")
# As we can see in the month of September the Sales is more and in the month of January the Sales is low
scatter.smooth(x=data$Sales, y=data$Spend, main="Sales ~ Spend")
#As we can see that their is linear relationship between Sales and Spend


#3.Using BoxPlot To Check For Outliers
boxplot(data$Sales, main="Sales")
boxplot(data$Spend, main="Spend")
# As we can see their are no outliers in the data


#4. Using Density Plot To Check If Response Variable Is Close To Normal
#install.packages("e1071")
library(e1071) # for skewness function
#par(mfrow=c(1,2)) # divide graph area in 2 columns
library(e1071)
plot(density(data$Spend), main="Density Plot: Spend", ylab="Frequency")
sub=paste("Skewness",round(e1071::skewness(cars$speed)),2)
# density plot for Sales
polygon(density(data$Spend), col="red")
##the Spend data is following Normally Positively Skewed Distribution
plot(density(data$Sales), main="Density Plot: Sales", ylab="Frequency")
sub=paste("Skewness", round(e1071::skewness(data$Sales))) 
# density plot for Spend
polygon(density(data$Sales), col="red")
##the Sales data is following Normally Positively Skewed Distribution

#4. Check the Correlation Analysis
cor(data$Sales, data$Spend)
#the correlation between Sales and Spend is very Strongly Positively Correlated 

#5. Build the Linear Regression Model
linearMod=lm(Sales ~ Spend, data=data)
linearMod
#b0 value is 1383.47 and the b1(Coefficient of the predictor variable) is 10.62 and the regression equation would look like Sales=1383.47*10.62(Spend)

#6. Using p-value Check For Statistical Significance
modelsummary=summary(linearMod)
modelsummary
modelCoeffs=modelsummary$coefficients
print(modelCoeffs)
estimate=modelCoeffs["Spend","Estimate"]
print(estimate)
std.error=modelCoeffs["Spend","Std. Error"]
print(std.error)
t_value=estimate/std.error
t_value
p1_value=2*pt(-abs(t_value),df=length(data$Sales)-1)
p1_value
#as p-value is less than level of significance , its is Statistically Significant

#7.Capture the summary of the linear model
summary(linearMod)


#8. Also perform the Linear Diagnostics for the given data set(Hint: plot(lmmodel) )
plot(linearMod)
#Residuals vs fitted: the points are randomly dispersed around the horizontal axis, therefor the linear regression model is appropriate for it 
#Normal Q-Q: the points are lined in a straight line, therefore the residuals are normally distributed.
#Scale-Location: the points are Spread towards both x and y axis , therefore the residuals are spread properly around the predicted values.
#Residuals vs Leverage: As we can see their is one extreme point (9th point), that point might or might not influence the regression line.


#9. Create the training and test data (70:30)
dim(data)
trainingRowIndex=sort(sample(nrow(data), nrow(data)*.7))
trainingData=data[trainingRowIndex,]
trainingData
testData=data[-trainingRowIndex,]
testData
#the data is split into 70% of training set and 30% of the test set

#10. Fit the model on training data and predict sales on test data
lmModel=lm(Sales~Spend, data=data)
SalesPred=predict(lmModel,testData)
SalesPred
summary(lmModel)


#11.Review the diagnostic measures
plot(lmModel)
#Residuals vs fitted: the points are randomly dispersed around the horizontal axis, therefor the linear regression model is appropriate for it 
#Normal Q-Q: the points are lined in a straight line, therefore the residuals are normally distributed.
#Scale-Location: the points are Spread towards both x and y axis , therefore the residuals are spread properly around the predicted values.
#Residuals vs Leverage: As we can see their is one extreme point (9th point), that point might or might not influence the regression line.

