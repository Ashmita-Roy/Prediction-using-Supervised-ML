#Task 1-Prediction using Supervised ML
#author: Ashmita Roy Medha


#Load the libraries

library(ggplot2)
library(readr)
library(caTools)

#Load dataset from url

df<- read.csv("https://raw.githubusercontent.com/AdiPersonalWorks/Random/master/student_scores%20-%20student_scores.csv")
View(df)
summary(df)

#plot the data (Hours vs Scores)
plot(df$Hours,df$Scores,xlab = "Study Hours", ylab = "Study Scores", pch = 17, col = "red", frame = FALSE)

#plot the regression line
plot(df$Hours,df$Scores,xlab = "Study Hours", ylab = "Study Scores", pch = 17, col = "red", frame = FALSE)
abline(lm(df$Scores ~ df$Hours, data = df), col = "blue")

#Correlation
cor(df$Hours,df$Scores)
#There is a strong correlation between the two variables

#Split the data for train-test 
set.seed(2)
split <- sample.split(df, SplitRatio = 0.8)
split

train<- subset(df,split="TRUE")
test<- subset(df,split="FALSE")
train
test

#Build the Model
model<-lm(df$Scores ~., data = train) 
#lm function stands for linear model

summary(model) 
#summary generates all kinds of output

#Prediction over testing data
pred<- predict(model, test)
pred

#Predicting the percentage of student on study hours
hours <- data.frame(Hours=c(9.25))
percent <- predict(model,hours)
percent

#install.packages("DT")

#comparison of acutual and predicted data
data <- data.frame(Actual=test$Scores, Predicted=pred)
DT::datatable(data)

#Thank you