
# Katja Häkkinen
# 15th of Nov 2021
# Data source: UCI Machine Learning Repository (http://archive.ics.uci.edu/ml/dataset)
# Metadata available at: https://archive.ics.uci.edu/ml/datasets/Student+Performance
# The data are from two identical questionaires related to secondary school student alcohol
# comsumption in Portugal.
# P. Cortez and A. Silva. Using Data Mining to Predict Secondary School Student Performance.
# paper <- "http://www3.dsi.uminho.pt/pcortez/student.pdf"

## RStudio Exercise 3: Data wrangling

# read in both datasets
math <- read.csv('~/IODS-project/data/student-mat.csv', sep=";", header=T)
por <- read.csv('~/IODS-project/data/student-por.csv', sep=";", header=T)

# structure of the two datasets math and por
str(math)
str(por)
# first six observations of the two datasets math and por
head(math)
head(por)
# variable (column) names of the two datasets math and por
colnames(math)
colnames(por)

# Define own id for both datasets
library(dplyr)
por_id <- por %>% mutate(id=1000+row_number()) 
math_id <- math %>% mutate(id=2000+row_number())

# Which columns vary in datasets
free_cols <- c("id","failures","paid","absences","G1","G2","G3")

# The rest of the columns are common identifiers used for joining the datasets
join_cols <- setdiff(colnames(por_id),free_cols)

pormath_free <- por_id %>% bind_rows(math_id) %>% select(one_of(free_cols))

# Combine datasets to one long data
pormath <- por_id %>% 
  bind_rows(math_id) %>%
  # Aggregate data 
  group_by(.dots=join_cols) %>%  
  # Calculating required variables from two obs  
  summarise(                                                           
    n=n(),
    id.p=min(id),
    id.m=max(id),
    failures=round(mean(failures)),     #  Rounded mean for numerical
    paid=first(paid),                   #    and first for chars
    absences=round(mean(absences)),
    G1=round(mean(G1)),
    G2=round(mean(G2)),
    G3=round(mean(G3))    
  ) %>%
  # Remove lines that do not have exactly one obs from both datasets
  #   There must be exactly 2 observations found in order to joining be succesful
  #   In addition, 2 obs to be joined must be 1 from por and 1 from math
  #     (id:s differ more than max within one dataset (649 here))
  filter(n==2, id.m-id.p>650) %>%  
  # Join original free fields, because rounded means or first values may not be relevant
  inner_join(pormath_free,by=c("id.p"="id"),suffix=c("",".p")) %>%
  inner_join(pormath_free,by=c("id.m"="id"),suffix=c("",".m")) %>%
  # Calculate other required variables  
  ungroup %>% mutate(
    alc_use = (Dalc + Walc) / 2,
    high_use = alc_use > 2,
    cid=3000+row_number()
  )

# explore the new pormath analysis dataset

# chance dataset to dataframe
pormath <- as.data.frame(pormath)
# six first observations of the data
head(pormath)
# rows and columns of the data
dim(pormath)
# structure of the data
str(pormath)
# variable (column) names of the data
colnames(pormath)
# The joined data now has 370 observations (rows) and 51 variables (columns)

# set the working directory of this R session the iods project folder
setwd("~/IODS-project")

# save the pormath dataset to the ‘data’ folder, using write.csv() 
write.csv(pormath, '~/IODS-project/data/pormath.csv', row.names=FALSE)

#####################################################

## RStudio Exercise 3: Analysis
date()

# read pormath data file, using read.csv()
pormath <- read.csv('~/IODS-project/data/pormath.csv')

## introduction of the dataset
# structure of the data
str(pormath)
# show first 6 observations of the data
head(pormath)

# print out names of the variables
colnames(pormath)

# rows and columns in the dataset
dim(pormath)

## frequencies of the variables, cross tables

# low and high alcohol consumption
summary(as.factor(pormath$high_use))

# alcohol consumption and freetime 
table(high_use = pormath$high_use, freetime = pormath$freetime)

# alcohol consumption and absences
table(high_use = pormath$high_use, absences = pormath$absences)

# alcohol consumption and age
table(high_use = pormath$high_use, age = pormath$age)

# alcohol consumption and going out with friends
table(high_use = pormath$high_use, goout = pormath$goout)

# select variables freetime, absences, age, goout for joined summary statistics
myvars <- c("freetime", "absences", "age", "goout")
new_pormath <- pormath[myvars]
# print basic statistic summary
summary(new_pormath)

# access library psych
library(psych)
# create summary table
describe(pormath[ , c('freetime', 'absences', 'age', 'goout')], fast = TRUE)

## bar plots to describe distributions of the variables

# access the libraries tidyr, dplyr, ggplot2 and cowplot
library(tidyr)
library(dplyr)
library(ggplot2)
library(cowplot)

# bar plots for each variable
g1 <- ggplot(pormath, aes(x = high_use, fill = sex)) + geom_bar()
g2 <- ggplot(pormath, aes(x = freetime, fill = sex)) + geom_bar()
g3 <- ggplot(pormath, aes(x = absences, fill = sex)) + geom_bar()
g4 <- ggplot(pormath, aes(x = age, fill = sex)) + geom_bar()
g5 <- ggplot(pormath, aes(x = goout, fill = sex)) + geom_bar()

# draw all bar plots
plot_grid(g1, g2, g3, g4, g5, labels="AUTO")

## box plots to describe distributions of the variables
# box plots of the variables

g6 <- ggplot(pormath, aes(x = high_use, y = freetime, col = sex)) + geom_boxplot() + ylab("freetime")
g7 <- ggplot(pormath, aes(x = high_use, y = absences, col = sex)) + geom_boxplot() + ylab("absences")
g8 <- ggplot(pormath, aes(x = high_use, y = age, col = sex)) + geom_boxplot() + ylab("age")
g9 <- ggplot(pormath, aes(x = high_use, y = goout, col = sex)) + geom_boxplot() + ylab("goout")

# draw boxplots
plot_grid(g6, g7, g8, g9, labels = "AUTO")


# create logistic regression model
model <- glm(high_use ~ freetime + absences + age + goout, data = pormath, family = "binomial")

# print summary of the model
summary(model)

# model with statistically significant variables
model2 <- glm(high_use ~ absences + goout, data = pormath, family = "binomial")

# print the coefficients of the model
coef(model2)

# compute odds ratios (OR)
OR <- coef(model2) %>% exp

# compute confidence intervals (CI)
CI <- confint(model2) %>% exp

# print out the odds ratios with their 95% confidence intervals
cbind(OR, CI)

# fit the model
model2 <- glm(high_use ~ absences, goout, data = pormath, family = "binomial")

# predict the probability of high_use
probabilities <- predict(model2, type = "response")

# add the predicted probabilities to pormath
pormath <- mutate(pormath, probability = probabilities)

# use the probabilities to make a prediction of high_use
pormath <- mutate(pormath, prediction = probability > 0.5)

# tabulate the target variable versus the predictions (2x2 cross table)
table(high_use = pormath$high_use, prediction = pormath$prediction)

# initialize a plot of 'high_use' versus 'probability' in 'pormath'
g <- ggplot(pormath, aes(x = probability, y = high_use, col = prediction))

# define the geom as points and draw the plot
g + geom_point()

# tabulate the target variable versus the predictions
table(high_use = pormath$high_use, prediction = pormath$prediction) %>% prop.table %>% addmargins

# define a loss function (mean prediction error)
loss_func <- function(class, prob) {
  n_wrong <- abs(class - prob) > 0.5
  mean(n_wrong)
}

# call loss_func to compute the average number of wrong predictions in the (training) data
loss_func(class = pormath$high_use, prob = 0)

