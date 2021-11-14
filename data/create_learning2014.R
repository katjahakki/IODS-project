
# Katja Häkkinen 3rd of November 2021
# File contains JYTOPKYS2 survey data concerning learning and teaching in a statistical course
# reference to the data source https://www.mv.helsinki.fi/home/kvehkala/JYTmooc/JYTOPKYS2-meta.txt

### RStudio exercise 2 - data wrangling

# read in data
lrn14 <- read.table("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt", sep="\t", header=TRUE)

# dimensions of the data (rows and columns)
dim(lrn14)

# number of rows
nrow(lrn14)

# number of columns
ncol(lrn14)

# first six observations of the data
head(lrn14)

# structure of the data
str(lrn14)

# basic statistics of the data
summary(lrn14)

## Create an analysis dataset with the variables gender, age, attitude, deep,
## stra, surf and points by combining questions in the learning2014 data 

# access the dplyr library
library(dplyr)

## Scale all combination variables to the original scales by taking the mean 

# print the "Attitude" column vector of the lrn14 data
lrn14$Attitude
# create column 'attitude' by scaling the column "Attitude"
lrn14$attitude <- lrn14$Attitude / 10
# print the "attitude" column vector of the lrn14 data
lrn14$attitude

# questions related to deep learning variable
deep_questions <- c("D03", "D11", "D19", "D27", "D07", "D14", "D22",
                    "D30","D06",  "D15", "D23", "D31")
# select the columns related to deep learning and create column 'deep' by averaging
deep_columns <- select(lrn14, one_of(deep_questions))
lrn14$deep <- rowMeans(deep_columns)
# check the new deep variable
lrn14$deep

# questions related to strategic learning variable
strategic_questions <- c("ST01","ST09","ST17","ST25","ST04","ST12","ST20","ST28")
# select the columns related to strategic learning and create column 'stra' by averaging
strategic_columns <- select(lrn14, one_of(strategic_questions))
lrn14$stra <- rowMeans(strategic_columns)
# check the new stra variable
lrn14$stra

# questions related to surface learning variable
surface_questions <- c("SU02","SU10","SU18","SU26", "SU05","SU13","SU21",
                       "SU29","SU08","SU16","SU24","SU32")
# select the columns related to surface learning and create column 'surf' by averaging
surface_columns <- select(lrn14, one_of(surface_questions))
lrn14$surf <- rowMeans(surface_columns)
# check the new surf variable
lrn14$surf

# choose columns for new dataset 
keep_columns <- c("gender","Age","attitude", "deep", "stra", "surf", "Points")

# select the 'keep_columns' to create a new dataset
learning2014 <- select(lrn14, one_of(keep_columns))

# change the name of the second column
colnames(learning2014)[2] <- "age"

# change the name of "Points" to "points"
colnames(learning2014)[7] <- "points"

# print out the new column names of the data
colnames(learning2014)

# exclude observations where the exam points variable is zero
# (select rows where points is greater than zero)
learning2014 <- subset(learning2014, points > 0)

# check rows and columns, first six observations and structure of the new learnin2014 data
dim(learning2014)
head(learning2014)
str(learning2014)
# The data has 166 observations and 7 variables

# set the working directory of this R session the iods project folder
setwd("~/IODS-project")

# save the learning2014 dataset to the ‘data’ folder, using write.csv() 
write.csv(learning2014, '~/IODS-project/data/learning2014.csv', row.names=FALSE)

# read learning2014.csv file, using read.csv()
learning2014 <- read.csv('~/IODS-project/data/learning2014.csv')


### RStudio exercise 2 - The Analysis exercise

# check data
head(learning2014)

# data structure
str(learning2014)

# data dimensions
dim(learning2014)
nrow(learning2014)
ncol(learning2014)

# summaries of the variables
summary(learning2014)

# distribution of female and male students
summary(as.factor(learning2014$gender))

## graphical overview of the data

# Access the ggplot2 library
library(ggplot2)

# initialize plot with data and aesthetic mapping
p1 <- ggplot(learning2014, aes(x = attitude, y = points, col = gender))

# define the visualization type (points)
p2 <- p1 + geom_point()

# draw the plot
p2

# add a regression line
p3 <- p2 + geom_smooth(method = "lm")
p3

# add a main title and draw the plot
p4 <- p3 + ggtitle("Student's attitude versus exam points")
p4

# change variable gender from character to factor in order to draw a scatter plot
learning2014$gender = as.factor(learning2014$gender)

# draw a scatter plot matrix of the variables in learning2014
# [-1] excludes the first column (gender)
pairs(learning2014[-1], col=learning2014$gender)

# access the GGally and ggplot2 libraries
#install.packages("GGally")
library(GGally)
library(ggplot2)

# create a more advanced plot matrix with ggpairs()
# Adjust the argument mapping of ggpairs() by defining col = gender inside aes().
# Adjust the code a little more: add another aesthetic element alpha = 0.3 inside aes().
p <- ggpairs(learning2014, mapping = aes(col = gender, alpha = 0.3), lower = list(combo = wrap("facethist", bins = 20)))

# draw the plot
p

# a scatter plot of points versus attitude
# library(ggplot2)
# qplot(points, attitude, data = learning2014) + geom_smooth(method = "lm")

# fit a regression model, exam points is the target (dependent) variable
my_model <- lm(points ~ age + attitude + stra,  data = learning2014)

# summary of the fitted model
summary(my_model)


# fit a regression model with statistically significant explanatory variables
my_model2 <- lm(points ~ attitude,  data = learning2014)

# summary of the fitted my_model2
summary(my_model2)

# Using a summary of your fitted model, explain the relationship between the chosen explanatory
# variables and the target variable (interpret the model parameters).
# Explain and interpret the multiple R squared of the model.

# Produce the following diagnostic plots: Residuals vs Fitted values,
# Normal QQ-plot and Residuals vs Leverage. #

par(mfrow = c(2,2))

plot(my_model2, which = c(1, 2, 5))

# Explain the assumptions of the model and interpret the validity
# of those assumptions based on the diagnostic plots. 

## muista selittää myös gender jos se on malleissa
## löytyisikö muita extratapoja mallintaa dataa?? 

# gender and age are binary variables
# Points variable contains total exam points
