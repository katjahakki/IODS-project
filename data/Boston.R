# Katja Häkkinen
# 23th of Nov 2021
# Boston data is built-in dataset in R
# details of the Boston dataset can be found here: https://stat.ethz.ch/R-manual/R-devel/library/MASS/html/Boston.html

date()

# access the MASS package
library(MASS)

# read in data
data("Boston")

## explore the dataset
# structure
str(Boston)
# dimensions (rows and columns of the data)
dim(Boston)

#summary
summary(Boston)

# plot matrix of the variables
pairs(Boston)

# access libraries
library(MASS)
library(tidyr)
library(corrplot)

# calculate the correlation matrix and round it
cor_matrix <-  cor(Boston) %>% round(digits = 2)

# print the correlation matrix
cor_matrix

# visualize the correlation matrix
corrplot(cor_matrix, method="circle", type="upper", cl.pos="b", tl.pos="d", tl.cex = 0.6)

# center and standardize variables
boston_scaled <- scale(Boston)

# summaries of the scaled variables
summary(boston_scaled)

# class of the boston_scaled object
class(boston_scaled)

# change the object to data frame
boston_scaled <- as.data.frame(boston_scaled)

# summary of the scaled crime rate
summary(boston_scaled$crim)

# create a quantile vector of crim and print it
bins <- quantile(boston_scaled$crim)
bins

# create a categorical variable 'crime'
crime <- cut(boston_scaled$crim, breaks = bins, include.lowest = TRUE, labels = c("low", "med_low", "med_high", "high"))

# look at the table of the new factor crime
table(crime)

# remove original crim from the dataset
boston_scaled <- dplyr::select(boston_scaled, -crim)

# add the new categorical value to scaled data
boston_scaled <- data.frame(boston_scaled, crime)

# number of rows in the Boston dataset 
n <- nrow(boston_scaled)

# choose randomly 80% of the rows
ind <- sample(n,  size = n * 0.8)

# create train set
train <- boston_scaled[ind,]

# create test set 
test <- boston_scaled[-ind,]

# linear discriminant analysis
lda.fit <- lda(crime ~ ., data = train)

# print the lda.fit object
lda.fit

# the function for lda biplot arrows
lda.arrows <- function(x, myscale = 1, arrow_heads = 0.1, color = "orange", tex = 0.75, choices = c(1,2)){
  heads <- coef(x)
  arrows(x0 = 0, y0 = 0, 
         x1 = myscale * heads[,choices[1]], 
         y1 = myscale * heads[,choices[2]], col=color, length = arrow_heads)
  text(myscale * heads[,choices], labels = row.names(heads), 
       cex = tex, col=color, pos=3)
}

# target classes as numeric
classes <- as.numeric(train$crime)

# plot the lda results
plot(lda.fit, dimen = 2, col = classes, pch = classes)
lda.arrows(lda.fit, myscale = 1)

# save the correct crime categories from test dataset
correct_classes <- test$crime

# remove the categorical crime variable from the test dataset
test <- dplyr::select(test, -crime)

# predict the classes with the LDA model on the test data
lda.pred <- predict(lda.fit, newdata = test)

# cross tabulate the results with the crime categories from the test set
table(correct = correct_classes, predicted = lda.pred$class)

# Reload the Boston dataset and standardize the dataset 

# load MASS and Boston
library(MASS)
data('Boston')

# standardize the dataset 
boston_standardized <- scale(Boston)

## calculate the distances between the observations
# euclidean distance matrix
dist_eu <- dist(boston_standardized)

# look at the summary of the distances
summary(dist_eu)

# manhattan distance matrix
dist_man <- dist(boston_standardized, method = 'manhattan')

# look at the summary of the distances
summary(dist_man)

# k-means clustering
km <- kmeans(boston_standardized, centers = 3)

# plot the Boston dataset with clusters
pairs(boston_standardized, col = km$cluster)

set.seed(123)

# determine the number of clusters
k_max <- 10

# calculate the total within sum of squares
twcss <- sapply(1:k_max, function(k){kmeans(boston_standardized, k)$tot.withinss})

library(plotly)
# visualize the results
qplot(x = 1:k_max, y = twcss, geom = 'line')

# k-means clustering
km <-kmeans(boston_standardized, centers = 2)

# plot the Boston dataset with clusters
pairs(boston_standardized, col = km$cluster)








