
#GRIPAPR2022
#SPARKS FOUNDATION 
#TASK 1
#Using supervised ML predict the scores of children who study 9.25 hours a day.



#Reading the data set as provided through a text file 

  data_scores=read.csv("C:/Users/hp/Downloads/Data.txt",header = TRUE)
  data_scores
  View(data_scores)
  colnames(data_scores)

#Statistical Summary
  
  summary(data_scores)
  cor(data_scores)
  attach(data_scores)

#Installing Packages for plotting graphs
  
  install.packages("ggplot2")
  library(ggplot2)
  library(tidyverse)

#Plotting curves to know about the distribution

  den1<-density(data_scores$Scores)
  plot(den1, frame= FALSE, col="blue",main="Density Plot for Scores")


  den2<-density(data_scores$Hours)
  plot(den2,frame=FALSE,col="red" ,main="Density plot for Hours")

#Creating a scatter plot for the student scores and the hours of study 

  ggplot(data_scores, aes(Hours,Scores,color=I("green")))+
  geom_point()

#Installing packages to run a regression test
  
  install.packages("lmtest")
  library(lmtest)

#linear model regression 
  
  reg=lm(Scores~Hours, data=rawdata)
  summary(reg)

#Here we have found out that
#Intercept=2.4837 and Slope =9.7758
  
#the interpretation for such a model is as follows.
#It means that the average increase in scores by an increase in one hour's study is 
#approximately 9.7758 marks.## It also signifies that even when a student does not 
#study i.e. 0 hours of study , he can still score an average of 2.4837 marks.
  
#Adding a regression line in the student scores.
  
  ggplot(data_scores,aes(Hours,Scores))+
  geom_point()+
  stat_smooth(method = lm)
  geom_abline(aes(intercept=2.4837,slope=9.7758,color=I("red")))

#Calculating the scores of students based on the regression line 

  predicted_scores=2.4837+9.7758*Hours
  predict(reg, data.frame(Hours=9.25))
  
#The result thus obtained is that if the student studies 9.25 hours a day he will 
#score an average marks as 92.90985.

  view(predicted_scores)
  attach(predicted_scores)

#Attaching the two tables together 

  student_scores=data.frame(data_scores,predicted_scores)
  student_scores
  View(student_scores)
  attach(student_scores)

#Plotting the two scores against each other 

  plot(x=predicted_scores,y=Scores,xlab="predicted_scores",ylab="Scores",
     main="Predicted vs Actual Values")


#Scatter plot representation for the two scores to see the residuals  

  ggplot(student_scores,aes(Hours))+
  geom_point(aes(y=Scores,col=I("red")))+
  geom_point(aes(y=predicted_scores,col=I("blue")))


#Creating a bar plot and line plot for the two different scores 
  
  barplot(student_scores$Scores,col="yellow",
        main="Actual Scores and Predicted Scores ",
        ylab="Scores")+ lines(student_scores$predicted_scores,col="blue",
                     main="Actual Scores and Predicted Scores",ylab="Scores")