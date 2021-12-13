
# Katja Häkkinen
# 7th of Dec 2021

# access the packages dplyr and tidyr
library(dplyr)
library(tidyr)
# access the package ggplot2
library(ggplot2)

# Load the data set RATS (in the wide form)
RATS <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/rats.txt", sep = '\t', header=T)
# structure of the RATS data
str(RATS)
# observations of RATS data
head(RATS, 16)
# column names of RATS data
names(RATS)
# summary statistics of RATS data
summary(RATS)
# rat count in each group
summary(as.factor(RATS$Group))
# RATS data contains 16 observations (rows) and 13 variables (columns)
# The variables are ID (1-16), Group (1, 2 or 3), and WD1-64 (day)
# the body weight values are in WD variables
# nutrition study conducted in three groups of rats, three groups were put
# on different diets and each rat's body weight was recorded repeatedly over a 9-week
# period
# 8 rats in group 1, 4 rats in group 2, 4 rats in group 3
# each rat has one row of information

# factor variables ID and Group (convert the categorical variables to factors) of RATS data
RATS$ID <- factor(RATS$ID)
RATS$Group <- factor(RATS$Group)

# glimpse the RATS data
glimpse(RATS)

# convert RATS data to long form and add a time variable
RATSL <- RATS %>%
  gather(key = WD, value = Weight, -ID, -Group) %>%
  mutate(Time = as.integer(substr(WD,3,4))) 

# structure of the RATSL data
str(RATSL)
# observations of RATSL data
head(RATSL)
# column names of RATSL data
names(RATSL)
# summary statistics of RATSL data
summary(RATSL)
# RATSL data has now 176 observations (rows) and 5 variables (columns)
# the variables are ID (1-16), Group (1, 2 or 3), WD (WD1-WD64), Weight (weight in grams)
# and Time (1-64)
# each rat has now several rows of information, one row has information of one
# weight measurement in one time point
# now Time variable includes only the day number when the weight measurement was done


# set the working directory of this R session the iods project folder
setwd("~/IODS-project")

# save the pormath dataset to the ‘data’ folder, using write.csv() 
write.csv(RATSL, '~/IODS-project/data/RATSL.csv', row.names=FALSE)


# Load the data set BPRS (in the wide form)
BPRS <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/BPRS.txt", sep = "", header=T)
# structure of the BPRS data
str(BPRS)
# observations of the BPRS data
head(BPRS, 40)
# column names of the BPRS data
names(BPRS)
# summary statistics of the BPRS data
summary(BPRS)

# BPRS data has 40 observations (rows) and 11 variables (columns)
# BPRS measurements from 40 subjects
# 40 male subjects were randomly assigned to one of two treatment groups and each subject was rated
# on the psychiatric rating scale (BPRS) measured before treatment began (week 0) and then weekly
# intervals for eight weeks
# the variables are treatment (1, 2), subject (1-20), week0 -week8 (these variables have 
# bprs scores)
# 20 males in treatment 1, 20 males in treatment 2
# each male has one row of information

# let's create new id variable from 1 to 40 to distinguish each individual
BPRS$id <- seq.int(nrow(BPRS))
# check the subject numbers
head(BPRS, 40)
# now males with id 1-20 are in treatment 1 and 
# males with id 21-40 are in treatment 2

# factor treatment & subject (convert the categorical variables to factors) of BPRS data
BPRS$treatment <- factor(BPRS$treatment)
BPRS$subject <- factor(BPRS$subject)
BPRSL$id <- factor(BPRSL$id)

# convert the BPRSL data to long form
BPRSL <-  BPRS %>% gather(key = weeks, value = bprs, -treatment, -subject, -id)

# extract the week number/add a week variable to BPRS
BPRSL <-  BPRSL %>% mutate(week = as.integer(substr(weeks,5,5)))

# check the BPRSL data
str(BPRSL)
head(BPRSL)
names(BPRSL)
summary(as.factor(BPRSL$treatment))
summary(as.factor(BPRSL$subject))
glimpse(BPRSL)
# BPRSL data has 360 observations (rows) and 6 variables (columns)
# now the data has treatment, subject, id, weeks, BPRS value,id (1-40) and
# week number in the same row (per subject),
# column names are treatment, subject, weeks, id, bprs and week
# each male has now several rows of information, one row has information of one
# bprs measurement in one time point

# set the working directory of this R session the iods project folder
setwd("~/IODS-project")

# save the pormath dataset to the ‘data’ folder, using write.csv() 
write.csv(BPRSL, '~/IODS-project/data/BPRSL.csv', row.names=FALSE)


###################################################################

# week 6: analysis
date()

# Implement the analyses of Chapter 8 of MABS using the RATS data
# Analysis of Longitudinal Data I: Graphical Displays and Summary Measure Approach

# read in RATSL data
RATSL <- read.csv('~/IODS-project/data/RATSL.csv')

# check the data
glimpse(RATSL)

# factor variables ID and Group (convert the categorical variables to factors) of RATS data
RATSL$ID <- factor(RATSL$ID)
RATSL$Group <- factor(RATSL$Group)
BPRSL$id <- factor(BPRSL$id)

# check the data
glimpse(RATSL)
head(RATSL)

# draw the plot
ggplot(RATSL, aes(x = Time, y = Weight, linetype = ID)) +
  geom_line() +
  scale_linetype_manual(values = rep(1:10, times=4)) +
  facet_grid(. ~ Group, labeller = label_both) +
  theme(legend.position = "none") + 
  scale_y_continuous(limits = c(min(RATSL$Weight), max(RATSL$Weight)))

# standardize the variable Weight
RATSL <- RATSL %>%
  group_by(Time) %>%
  mutate(stdweight = (Weight - mean(Weight))/sd(Weight) ) %>%
  ungroup()

# glimpse the data
glimpse(RATSL)

# plot again with the standardized Weight
ggplot(RATSL, aes(x = Time, y = stdweight, linetype = ID)) +
  geom_line() +
  scale_linetype_manual(values = rep(1:10, times=4)) +
  facet_grid(. ~ Group, labeller = label_both) +
  scale_y_continuous(name = "standardized weight")

####### summary graphs

# number of days
n <- RATSL$Time %>% unique() %>% length()

# summary data with mean and standard error of weight by group and day 
RATSLS <- RATSL %>%
  group_by(Group, Time) %>%
  summarise( mean = mean(Weight), se = sd(Weight)/sqrt(n) ) %>%
  ungroup()

# glimpse the data
glimpse(RATSLS)

# plot the mean profiles
ggplot(RATSLS, aes(x = Time, y = mean, linetype = Group, shape = Group)) +
  geom_line() +
  scale_linetype_manual(values = c(1,2,3)) +
  geom_point(size=3) +
  scale_shape_manual(values = c(1,2,3)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se, linetype="1"), width=1) +
  theme(legend.position = c(0.9,0.5)) +
  scale_y_continuous(name = "mean(Weight) +/- se(Weight)")
 
# create a summary data by group and ID with mean weight as the summary variable (ignore Time 0)
RATSL8S <- RATSL %>%
  filter(Time > 0) %>%
  group_by(Group, ID) %>%
  summarise( mean=mean(Weight) ) %>%
  ungroup()

# glimpse the data
glimpse(RATSL8S)
#head(RATSL8S, 16)

# draw a boxplot of the mean versus group
ggplot(RATSL8S, aes(x = Group, y = mean)) +
  geom_boxplot() +
  stat_summary(fun = "mean", geom = "point", shape=23, size=4, fill = "white") +
  scale_y_continuous(name = "mean(Weight), days 1-64")

# create a new data by filtering the outliers and adjust the ggplot code the draw the plot again with the new data
 RATSL8S1 <- RATSL8S %>%
 filter(mean < 550) %>% # outlier in group 2
 filter(mean > 250) # outlier in group 1
# remove outlier in group 3 from row 11 
 RATSL8S1 <- RATSL8S1[-c(11),]
# check the RATSL8S1 data
 head(RATSL8S1, 16)
 
# draw a boxplot again without outliers
ggplot(RATSL8S1, mapping = aes(x = Group, y = mean)) +
  geom_boxplot() +
  stat_summary(fun = "mean", geom = "point", shape=23, size=4, fill = "white") +
  scale_y_continuous(name = "mean(Weight), days 1-64")

# Add the baseline from the original RATS data as a new variable to the summary data
RATSL8S2 <- RATSL8S %>%
  mutate(baseline = RATS$WD1)

# Fit the linear model with the mean as the response 
fit <- lm(mean ~ baseline + Group, data = RATSL8S2)

# Compute the analysis of variance table for the fitted model with anova()
anova(fit)

########################################

# Implement the analyses of Chapter 9 of MABS using the BPRS data
# Analysis of Longitudinal Data II: Linear Mixed Effects Models for Normal Response Variables

# read in BPRSL data
BPRSL <- read.csv('~/IODS-project/data/BPRSL.csv')

# check the data
glimpse(BPRSL)

# factor treatment & subject (convert the categorical variables to factors) of BPRS data
BPRSL$treatment <- factor(BPRSL$treatment)
BPRSL$subject <- factor(BPRSL$subject)
BPRSL$id <- factor(BPRSL$id)

# check the data
glimpse(BPRSL)
head(BPRSL)

# Plot the BPRSL data
ggplot(BPRSL, aes(x = week, y = bprs, group = id)) +
  geom_line(aes(linetype = treatment)) +
  scale_x_continuous(name = "Time (weeks)", breaks = seq(0, 8)) +
  scale_y_continuous(name = "BPRS") +
  theme(legend.position = "top")

# box plots

# create a summary data by treatment and id with mean bprs as the summary variable
  BPRSL8S <- BPRSL %>%
  group_by(treatment, id) %>%
  summarise( mean=mean(bprs) ) %>%
  ungroup()

# draw a boxplot of the mean bprs versus treatment
  ggplot(BPRSL8S, aes(x = treatment, y = mean)) +
  geom_boxplot() +
  stat_summary(fun = "mean", geom = "point", shape=23, size=4, fill = "white") +
  scale_y_continuous(name = "mean(bprs), weeks 0-8")
  
# Create a new data by filtering the outlier and adjust the ggplot code the draw the plot again with the new data
  BPRSL8S1 <- BPRSL8S %>%
    filter(mean < 60)

# Draw a boxplot without the outlier
  ggplot(BPRSL8S1, aes(x = treatment, y = mean)) +
    geom_boxplot() +
    stat_summary(fun = "mean", geom = "point", shape=23, size=4, fill = "white") +
    scale_y_continuous(name = "mean(bprs), weeks 0-8")

########

# create a regression model BPRS_reg
BPRS_reg <- lm(bprs ~ week + treatment, data = BPRSL)

# print out a summary of the model
summary(BPRS_reg)

## the random intercept model
# access library lme4
library(lme4)

# Create a random intercept model
BPRS_ref <- lmer(bprs ~ week + treatment + (1 | id), data = BPRSL, REML = FALSE)

# Print the summary of the model
summary(BPRS_ref)

# create a random intercept and random slope model
BPRS_ref1 <- lmer(bprs ~ week + treatment + (week | id), data = BPRSL, REML = FALSE)

# print a summary of the model
summary(BPRS_ref1)

# perform an ANOVA test on the two models
anova(BPRS_ref1, BPRS_ref)

# create a random intercept and random slope model
BPRS_ref2 <- lmer(bprs ~ week * treatment + (week | id), data = BPRSL, REML = FALSE)

# print a summary of the model
summary(BPRS_ref2)

# perform an ANOVA test on the two models
anova(BPRS_ref2, BPRS_ref1)

# Create a vector of the fitted values
Fitted <- fitted(BPRS_ref2)

# Create a new column fitted to BPRSL
BPRSL <- BPRSL %>%
  mutate(Fitted)

# draw the plot of BPRSL
ggplot(BPRSL, aes(x = week, y = Fitted, group = id)) +
  geom_line(aes(linetype = treatment)) +
  scale_x_continuous(name = "Time (weeks)", breaks = seq(0, 8)) +
  scale_y_continuous(name = "Fitted BPRS") +
  theme(legend.position = "top")

# Create a vector of the fitted values
Fitted2 <- fitted(BPRS_ref1)

# Create a new column fitted to BPRSL
BPRSL <- BPRSL %>%
  mutate(Fitted2)

# draw the plot of fitted BPRSL
ggplot(BPRSL, aes(x = week, y = Fitted2, group = id)) +
  geom_line(aes(linetype = treatment)) +
  scale_x_continuous(name = "Time (weeks)", breaks = seq(0, 8)) +
  scale_y_continuous(name = "Fitted BPRS") +
  theme(legend.position = "top")

