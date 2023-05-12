library(readr)
library(dplyr)
library(ggrepel)
library(leaflet)
library(leaflet.extras)
library(ggplot2)
library(scales)
library(MASS)
library(pls)
library(WOCR)
library(pracma)
library(car)
library(DT)
library(KernSmooth)
library(raster)
library(rgdal)
library(ggforce)
library(treemapify)
library(ggmosaic)
library(forcats)
library(rcompanion)
library(lsr)
library(vcd)
library(DescTools)
library(tidyverse)
library(treemap)

options(scipen=999)  # turn off scientific notation like 1e+06

df <- read.csv("master.csv", header = TRUE, sep=",", na.strings = "")
dim(df)
summary(df)
glimpse(df)

df2 = filter(df, Year != 2022)
dim(df2)

df3 = filter(df2, FedAid != "UTEP's COVID CARES Act Fund")
dim(df3)

table(df3$FedAid)
table(df3$USDAcat)

# Q1:
# USDAcat vs FedAid
ggplot(df3, aes(x = USDAcat, fill = FedAid)) +
  geom_bar(position = "fill") + ylab("proportion") +
  stat_count(geom = "text", 
             aes(label = stat(count)),
             position=position_fill(vjust=0.5), colour="white")

# USDAcat vs Income
ggplot(df3, aes(x = USDAcat, fill = Income)) +
  geom_bar(position = "fill") + ylab("proportion") +
  stat_count(geom = "text", 
             aes(label = stat(count)),
             position=position_fill(vjust=0.5), colour="white")

# Filter by Very Low FS and Less than 10,000
dfFiltered = filter(df3, USDAcat=="Very Low FS", Income =="Less than $10,000")
dim(dfFiltered)
table(dfFiltered$USDAcat)

# Filtered by Very Low FS and Less than 10,000 then frequency by Academic Level
ggplot(dfFiltered, aes(x = fct_infreq(Classification), fill = Classification)) +
  geom_bar() +
  labs(x = "Academic Level") +
  stat_count(geom = "text", 
             aes(label = stat(count)), vjust = 1.5,
             position=position_dodge(width = 0.9), colour="white")

# Filtered by Very Low FS and Less than 10,000 then Academic level vs FedAid
# Remark: Graduate and doctoral have majority of Grants and no Loans, in 
# contrast, seniors, juniors and sophomore have majority of loans and almost
# no grants. My conclusion is that for the group with the lowest income and less
# food security (poorest) it is counter productive to loan, i.e. ask to pay back
# the money. It is best to give the money for free, i.e. grants. Maybe grants
# should go to income level instead of grades.
ggplot(dfFiltered, aes(x = Classification, fill = FedAid)) +
  geom_bar(position = "fill") + ylab("proportion") +
  geom_text(
    aes(label=signif(..count.. / tapply(..count.., ..x.., sum)[as.character(..x..)], digits=3)),
    stat="count",
    position=position_fill(vjust=0.5))

# Additional data
extraDF <- read.csv("extra_questions_withID.csv", header = TRUE, sep=",", na.strings = "")
dim(extraDF)
summary(extraDF)
glimpse(extraDF)

common_colnames <- intersect(names(df), names(extraDF))
full_data <- merge(df, extraDF, by=common_colnames)

dim(full_data)
summary(full_data)
glimpse(full_data)

# Q2:
table(full_data$DelayComplDegree)
table(full_data$DiffConcentrate)
table(full_data$DiffConcentrate, full_data$DelayComplDegree)
table(full_data$index)


# Calculating Chi-squared test, (corrected) contingency coefficient and
# Cramer's V
# Since we get a p-value of less than the significance level of 0.05, 
# we can reject the null hypothesis and conclude that the two variables are, 
# indeed, dependent.
assocstats(xtabs(~full_data$DiffConcentrate + full_data$DelayComplDegree))
chisq.test(full_data$DiffConcentrate, full_data$DelayComplDegree)

# Raster of the 3 variables
ggplot(full_data, aes(x=full_data$DiffConcentrate, y=full_data$DelayComplDegree, fill=full_data$index)) +
  geom_raster() +
  coord_fixed() +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1))

# index vs diffconcentrate or delay are independent also.
chisq.test(full_data$DiffConcentrate, full_data$index)
chisq.test(full_data$index, full_data$DelayComplDegree)
chisq.test(full_data$index, full_data$RateMentalHealth)

# index vs diffconcentrate
ggplot(full_data, aes(x=full_data$index, y=full_data$DiffConcentrate)) +
  geom_jitter()

# MentalHealth vs diffconcentrate
ggplot(full_data, aes(x=full_data$RateMentalHealth, y=full_data$DiffConcentrate)) +
  geom_jitter()

# index vs diffconcentrate vs MentalHealth
ggplot(full_data, aes(x=full_data$RateMentalHealth, y=full_data$DiffConcentrate, fill=full_data$index)) +
  geom_raster() +
  coord_fixed() +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1))

# Using only year2022, Plotting index with difficult to concentrate and delay completing a degree 
# tells me that people with low FS have difficulty concentrating everyday. I want
# to know if this affects their mental health. It does at a certain degree as 
# the majority of students with highest level of difficulty to concentrate (almost
# everyday) said their mental health is Fair. Not the extreme as in poor but fair.
# So this might be an indicator that food security could be an important facor in
# mental health but not too acute. 

# Q3:
# Using only year2022, Females are the vast majority almost 2:1 ratio
ggplot(full_data, aes(fill=Gender, x=DiffConcentrate)) + 
  geom_bar(position="dodge") 
table(full_data$Gender)
# Find dependent or head of household level of Females and compare the same with males


# Women having difficulty concentrating are the ones with 2 or 1 dependents. 
# More prominent when they have 2 dependents.
ggplot(data = full_data) +
  geom_mosaic(aes(x=product(Dependents, DiffConcentrate, Gender), 
                fill = Dependents)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) + 
  labs(y="DiffConcentrate", x="Dependents:Gender", title = "Dependents, DiffConcentrate, Gender")

# With Income. In women, difficulty to concentrate is related to income. 
ggplot(data = full_data) +
  geom_mosaic(aes(x=product(Income, DiffConcentrate, Gender), 
                  fill = Income)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) + 
  labs(y="DiffConcentrate", x="Income:Gender", title = "Income, DiffConcentrate, Gender")

