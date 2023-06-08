# pull demo data from qualtrics
# facemorphs continuous analysis
# 6.6.23 KLS

# load required packages
library(here)
library(tidyverse)

# load source functions

# set hard-coded variables

# read data in
f1 <- read.csv(here('data', 'raw', 'Face Morphs 1 - Copy_June 6, 2023_08.59.csv'))
f2 <- read.csv(here('data', 'raw', 'Face Morphs 2 - Copy_June 6, 2023_09.07.csv'))

# remove first 3-4 rows
f1 <- f1[-1:-4,]
f2 <- f2[-1:-3,]

# only completes
f1 <- f1[which(f1$Finished == "True"),]
f2 <- f2[which(f2$Finished == "True"),]

# cut extraneous entry
f1 <- f1[18:25]
f2 <- f2[18:25]

# assign subnum
f1$subnum <- seq(1:nrow(f1))
f2$subnum <- seq(1:(nrow(f2))) + 100

# add study number
f1$study <- 1
f2$study <- 2

# remove 86 or older from data set
f1 <- f1[-98,]

# combine data frames
dt <- rbind(f1, f2)

# name columns
colnames(dt) <- c('psych_health', 'neuro_health', 'age', 'gender', 'education', 'race', 'income', 'physical_health', 'subnum', 'study')
dt<- dt[c(9:10, 1:8)]

# recode physical health
dt$physical_health <- recode(dt$physical_health, 'Very healthy' = '5', 'Not at all healthy' = '1' )

# change variable types
dt$age <- as.numeric(dt$age)
dt$physical_health <- as.integer(dt$physical_health)
dt <- dt %>% mutate_if(is.character, as.factor)
dt$income <- factor(dt$income, ordered = TRUE, 
       levels = c("less than $10,000", "$10,000-$19,999", "$20,000-$29,999", 
                  "$30,000-$39,000", "$40,000-$49,999", "$50,000-$59,000", 
                  "$60,000-$69,999", "$70,000-$79,999", "$80,000-$89,999", 
                  "$90,000-$99,999", "$100,000-$109,999", "$110,000-$119,999", 
                  "$120,000-$129,999", "$130,000-$139,999", "$140,000-$149,999", 
                  "$150,000 or more"))

# save data
write.csv(dt, here('data', 'demo_data.csv'), row.names = FALSE)

