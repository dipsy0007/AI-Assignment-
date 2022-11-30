install.packages("caret")
install.packages("tidyr")
library(caret)
library(ggplot2)

# attach the iris dataset to the environment
data(iris)
# rename the dataset
dataset <- iris
# create a list of 80% of the rows in the original dataset we can use for training
validation_index <- createDataPartition(dataset$Species, p=0.80, list = FALSE)
# select 20% of the data for validation
validation <- dataset[-validation_index, ]
# use the remaining 80% of data to training and testing the models
dataset <- dataset[validation_index, ]
# dimensions of dataset
dim(dataset)
# dimensions of dataset
dim(dataset)
# list types for each attribute
sapply(dataset, class)
# take a peek at the first 5 rows of the data
head(dataset)
# list the levels for the class
levels(dataset$Species)
# summarize the class distribution
percentage <- prop.table(table(dataset$Species)) * 100
cbind(freq = table(dataset$Species), percentage=percentage)
# summarize attribute distributions
summary(dataset)
# split input and output
x <- dataset[ , 1:4]
y <- dataset[ , 5]
# boxplot for each attribute on one image
par(mfrow=c(1,4))   # show plots in 1 row and 4 columns
for(i in 1:4) {
  boxplot(x[,i], main=names(dataset)[i])
}
# barplot for class breakdown
# par(mfrow=c(1,1))   # show plots in 1 row and 4 columns
plot(y)

# scatterplot matrix
featurePlot(x=x, y=y, plot="ellipse")
# Zoom on one of the graphs
ggplot(data=iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) + geom_point(size=3)
sb.pairplot(iris_data,hue="species",palette="hls")

#handling outlier
data <- iris[,1:4]
dim(iris)
quartiles <- quantile(data$Sepal.Width, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(data$Sepal.Width)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

data_no_outlier <- subset(data, data$Sepal.Width > Lower & data$Sepal.Width < Upper)

dim(data_no_outlier)






