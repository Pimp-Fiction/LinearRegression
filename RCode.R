# Building a Simple Linear Regression model

# Importing the dataset
dataset = read.csv('Predicting_Salaries.csv')

# Splitting the dataset into the Training set and Test set
install.packages('caTools')
library(caTools)
set.seed(90909)
split = sample.split(dataset$AnnualSalary, SplitRatio = 3/4)
training_set = subset(dataset, split == TRUE)
testing_set = subset(dataset, split == FALSE)

# Fitting Simple Linear Regression to the Training set
linearregressor = lm(formula = AnnualSalary ~ YearsOfExperience,
                     data = training_set)
summary(linearregressor)

# Predicting the Test set results
Y_pred = predict(linearregressor, newdata= testing_set)
summary(Y_pred)

install.packages('scales')
library(scales)

# Visualising the Training set results
install.packages('ggplot2')
library(ggplot2)
ggplot() +
  geom_point(aes(x=training_set$YearsOfExperience, y=training_set$AnnualSalary),
             colour = 'red') +
  geom_line (aes( x= training_set$YearsOfExperience, y=predict(linearregressor, newdata= training_set)),
             colour = 'navy') +
  ggtitle ('Annual Salaries of Data Scientists vs Experience in Years (Training Set)') +
  xlab ('Years of Experience') +
  ylab ('Annual Salary') +
  scale_x_continuous(limits = c(0, 12)) + 
  scale_y_continuous(limits = c(0, 150000)) 

# Visualising the Test set results
library(ggplot2)
ggplot() +
  geom_point(aes(x=testing_set$YearsOfExperience, y=testing_set$AnnualSalary),
             colour = 'red') +
  geom_line (aes( x= training_set$YearsOfExperience, y=predict(linearregressor, newdata= training_set)),
             colour = 'navy') +
  ggtitle ('Annual Salaries of Data Scientists vs Experience in Years (Test Set)') +
  xlab ('Years of Experience') +
  ylab ('Annual Salary') 
