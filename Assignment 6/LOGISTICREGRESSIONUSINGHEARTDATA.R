dataset=read.csv(file = "C:/Users/dell/Desktop/heart.csv", sep = ",")
View(dataset)
nrow(dataset)
library(ggplot2)
gg <- ggplot(dataset, aes(x=chol, y=trestbps)) + 
      geom_point(aes(col=target))
print(gg)             
summary(dataset)
databox=data.frame(dataset$age, dataset$trestbps,dataset$chol,dataset$thalach)
boxplot(databox)
# Scatterplot
library(ggplot2)
gg <- ggplot(dataset, aes(x=chol, y=trestbps)) + 
  geom_point(aes(col=target, size=oldpeak)) + 
  geom_smooth(method="loess", se=F) + 
  xlim(c(100, 430)) + 
  ylim(c(75, 200)) + 
  labs(subtitle="trestbps Vs chol", 
       y="trestbps", 
       x="chol", 
       title="Scatterplot", 
       caption = "Source: midwest", 
       bins = 30)
plot(gg)
#baseline model
table(dataset$target)
499/1025
###data train dan data testing###
library(caTools)
#randomly split data
set.seed(123)
split=sample.split(dataset$target, SplitRatio = 0.75)
split
qualityTrain=subset(dataset,split == TRUE)
qualityTest=subset(dataset,split == FALSE)
nrow(qualityTrain)
nrow(qualityTest)
#logistic regression model
datasetlog=glm(target ~ target+age+trestbps+chol+fbs+restecg+thalach+exang+oldpeak+slope+ca+thal,data=qualityTrain,family = binomial)
summary(datasetlog)
datasetlog2=glm(target ~ age+sex+cp+trestbps+chol+fbs+thalach+exang+oldpeak+slope+ca+thal,data=qualityTrain,family = binomial)
summary(datasetlog2)
datasetlog6=glm(target ~ sex+cp+trestbps+chol+thalach+oldpeak+ca+thal,data=qualityTrain,family = binomial)
summary(datasetlog6)
predictTrain=predict(datasetlog,type="response")
predictTrain

library(ROCR)
ROCRpred=prediction(predictTrain,qualityTrain$target)
ROCRperf=performance(ROCRpred,'tpr','fpr')
plot(ROCRperf)
plot(ROCRperf,colorize=TRUE)
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1),
     text.adj=c(-0.2,1.7))

#Area under the curve
auc = as.numeric(performance(ROCRpred, 'auc')@y.values)
auc
#Accuracy using a threshold of 0.7
predictTest=predict(datasetlog, newdata = qualityTest,type = "response") 
table(qualityTest$target,predictTest >=0.7)
