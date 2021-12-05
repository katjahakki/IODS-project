# Katja Häkkinen
# 25th of Nov 2021
# meta files for these datasets can be found here:
# http://hdr.undp.org/en/content/human-development-index-hdi
# http://hdr.undp.org/sites/default/files/hdr2015_technical_notes.pdf

# read in the “Human development” data
hd <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human_development.csv", stringsAsFactors = F)

# read in the “Gender inequality” data
gii <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/gender_inequality.csv", stringsAsFactors = F, na.strings = "..")

## explore the datasets

# “Human development” data
# structure
str(hd)
# dimensions (rows and columns)
dim(hd)
# summary statistics
summary(hd)

# “Gender inequality” data
# structure
str(gii)
# dimensions (rows and columns)
dim(gii)
# summary statistics
summary(gii)

# rename the variables with shorter names of hd dataset

names(hd)[names(hd) == 'HDI.Rank'] <- 'HDI_rank'
names(hd)[names(hd) == 'Human.Development.Index..HDI.'] <- 'HDI_index'
names(hd)[names(hd) == 'Life.Expectancy.at.Birth'] <- 'life_exp_birth'
names(hd)[names(hd) == 'Expected.Years.of.Education'] <- 'exp_years_edu'
names(hd)[names(hd) == 'Mean.Years.of.Education'] <- 'mean_years_edu'
names(hd)[names(hd) == 'Gross.National.Income..GNI..per.Capita'] <- 'GNI_per_capita'
names(hd)[names(hd) == 'GNI.per.Capita.Rank.Minus.HDI.Rank'] <- 'GNI_minus_HDI_rank'

# check column names of hd
colnames(hd)

# rename the variables with shorter names of gii dataset

names(gii)[names(gii) == 'GII.Rank'] <- 'GII_rank'
names(gii)[names(gii) == 'Gender.Inequality.Index..GII.'] <- 'GII_index'
names(gii)[names(gii) == 'Maternal.Mortality.Ratio'] <- 'mat_mort_ratio'
names(gii)[names(gii) == 'Adolescent.Birth.Rate'] <- 'adol_birth_rate'
names(gii)[names(gii) == 'Percent.Representation.in.Parliament'] <- 'repr_parl'
names(gii)[names(gii) == 'Population.with.Secondary.Education..Female.'] <- 'sec_edu_female'
names(gii)[names(gii) == 'Population.with.Secondary.Education..Male.'] <- 'sec_edu_male'
names(gii)[names(gii) == 'Labour.Force.Participation.Rate..Female.'] <- 'labour_female'
names(gii)[names(gii) == 'Labour.Force.Participation.Rate..Male.'] <- 'labour_male'

# check column names of gii
colnames(gii)

# access library dplyr
library(dplyr)
# mutate new variable ratio_sec_edu (the ratio of Female and Male populations with 
# secondary education in each country) to gii dataset
gii <- mutate(gii, ratio_sec_edu = sec_edu_female / sec_edu_male)

# mutate new variable ratio_labour (the ratio of labour force participation
# of females and males in each country) to gii dataset
gii <- mutate(gii, ratio_labour = labour_female / labour_male)

# check column names in gii dataset
colnames(gii)

# join together hd and gii datasets by inner join using the variable Country as the
# identifier keeping only the countries in both data sets 
human <- dplyr::inner_join(hd, gii, by = "Country")

# check the new human data
str(human)
# the joined human data has 195 observations and 19 variables

# set the working directory of this R session the iods project folder
setwd("~/IODS-project")

# save the human dataset to the ‘data’ folder, using write.csv() 
write.csv(human, '~/IODS-project/data/human.csv', row.names=FALSE)

####################################################################

# Week 5: Data wrangling
date()

# read in human data
human <- read.csv('~/IODS-project/data/human.csv')

## explore the dataset
# structure
str(human)
# dimensions (rows and columns)
dim(human)
# print out summaries of the variables
summary(human)

# transform the Gross National Income (GNI) variable to numeric and remove the commas from the variable
human <- mutate(human, GNI_per_capita = as.numeric(gsub(",","", GNI_per_capita)))

# check the structure of the GNI column in 'human'
str(human$GNI_per_capita)

# exclude unneeded variables: keep only the columns matching the following variable names
# "Country", "Edu2.FM", "Labo.FM", "Edu.Exp", "Life.Exp",
# "GNI", "Mat.Mor", "Ado.Birth", "Parli.F"

# columns to keep
keep <- c("Country", "ratio_sec_edu", "ratio_labour", "life_exp_birth", "exp_years_edu", "GNI_per_capita", "mat_mort_ratio", "adol_birth_rate", "repr_parl")

# select the 'keep' columns
human <- dplyr::select(human, one_of(keep))

# print out a completeness indicator of the 'human' data
complete.cases(human)

# print out the data along with a completeness indicator as the last column
data.frame(human[-1], comp = complete.cases(human))

# remove all rows with missing NA values
human <- dplyr::filter(human, complete.cases(human))

# look at the last 10 observations
tail(human, 10)

## remove the observations which relate to regions instead of countries
# last indice we want to keep
last <- nrow(human) - 7

# choose everything until the last 7 observations
human <- human[1:last, ]

# add countries as rownames
rownames(human) <- human$Country

# remove the Country variable from the data
human <- dplyr::select(human, -Country)

# check the data
str(human)
head(human)
# the human data now has 155 observations and 8 variables.

# set the working directory of this R session the iods project folder
setwd("~/IODS-project")

# save the human dataset to the ‘data’ folder, including rownames, using write.csv() 
write.table(human, '~/IODS-project/data/human.csv', row.names = TRUE)

#######################################################################

#### Week 5: Analysis exercise
date()

# read in human data
human <- read.table('~/IODS-project/data/human.csv')

## graphical overview of the data
# first six observations of the human data
head(human)
# structure of the human data
str(human)
# basic statistics of the human data
summary(human)

# access GGally
library(GGally)
# access dplyr
library(dplyr)
# access corrplot
library(corrplot)

# visualize the human data variables
ggpairs(human)

# compute the correlation matrix and visualize it with corrplot
cor(human) %>% corrplot

# perform principal component analysis (with the SVD method)
pca_human <- prcomp(human)

# create and print out a summary of pca_human
s <- summary(pca_human)
s

# rounded percentages of variance captured by each PC
pca_pr <- round(100*s$importance[2,], digits = 1) 

# print out the percentages of variance
pca_pr

# create object pc_lab to be used as axis labels
pc_lab <- paste0(names(pca_pr), " (", pca_pr, "%)")

# draw a biplot
biplot(pca_human, cex = c(0.8, 1), col = c("grey40", "deeppink2"), xlab = pc_lab[1], ylab = pc_lab[2])

# standardize the variables
human_std <- scale(human)

# print out summaries of the standardized variables
summary(human_std)

# perform principal component analysis (with the SVD method)
pca_human2 <- prcomp(human_std)

# create and print out a summary of pca_human
s2 <- summary(pca_human2)
s2

# rounded percentages of variance captured by each PC
pca_pr2 <- round(100*s2$importance[2,], digits = 1) 

# print out the percentages of variance
pca_pr2

# create object pc_lab to be used as axis labels
pc_lab2 <- paste0(names(pca_pr2), " (", pca_pr2, "%)")

# draw a biplot
biplot(pca_human2, cex = c(0.8, 1), col = c("grey40", "deeppink2"), xlab = pc_lab2[1], ylab = pc_lab2[2])


# install.packages('FactoMineR')
library(FactoMineR)
library(ggplot2)
library(dplyr)
library(tidyr)
# read in tea data
data(tea)

## explore the data
# structure of the data
str(tea)
# dimensions of the data (rows and columns)
dim(tea)

# column names to keep in the dataset
keep_columns <- c("Tea", "How", "how", "sugar", "where", "lunch")

# select the 'keep_columns' to create a new dataset
tea_time <- dplyr::select(tea, one_of(keep_columns))

# structure of the tea_time data
str(tea_time)
# dimensions of the data (rows and columns)
dim(tea_time)

# visualize the tea_time dataset
gather(tea_time) %>% ggplot(aes(value)) + facet_wrap("key", scales = "free") + geom_bar() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

# multiple correspondence analysis
mca <- MCA(tea_time, graph = FALSE)

# summary of the model
summary(mca)

# visualize MCA
plot(mca, invisible=c("ind"), habillage = "quali", graph.type = "classic")

