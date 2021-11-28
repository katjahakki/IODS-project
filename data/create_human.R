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
