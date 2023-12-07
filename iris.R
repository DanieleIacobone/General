# Exploratory Data Analysis and Machine Learning Model on Iris Data Set

######################
# Loading Iris Dataset
######################

library(datasets)
library(tidyverse)
data("iris")
iris %>%
  view()

######################
# Display Summary Statistics
######################

# head()/ tail()
head(iris, 10)
tail(iris, 10)

# str()
str(iris)

# summary()
summary(iris)

# Are there NAs ? No
sum(is.na(iris))

# Others Summary Statistics with skimr
install.packages("skimr")
library(skimr)
skim(iris)

# Group by "Species"
iris %>%
  group_by(Species) %>%
  skim()

######################
# Quick Data Visualisation
######################

# Panel Plots
plot(iris, col = "blue")

# Scatter Plot
plot(iris$Sepal.Width, iris$Sepal.Length, 
     col = iris$Species,
     xlab = "Sepal Width",
     ylab = "Sepal Lentgth")

# Histogram
hist(iris$Sepal.Width,
     xlab = "Sepal Width",
     col = "green")

# Feature Plot
library(caret)
featurePlot(x = iris[, 1:4],
            y = iris$Species,
            plot = "box")

######################
# Data Splitting
######################

# Perform stratified random split of the data
training_index <- createDataPartition(iris$Species, p = 0.8, list = FALSE)
training_set <- iris[training_index, ]
testing_set <- iris[- training_index, ]


