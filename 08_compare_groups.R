# compare demo data from qualtrics
# facemorphs continuous analysis
# 6.6.23 KLS

# load required packages
library(here)
library(tidyverse)

# load source functions

# set hard-coded variables

# read data in
dt <- read.csv(here('data', 'demo_data.csv'))

# compare ages ####
age <- dt %>% group_by(study) %>% summarize(
  mean_age = round(mean(age), 2),  
  sd_age = round(sd(age), 2)
)
t.test(dt[which(dt$study == 1),]$age, dt[which(dt$study == 2),]$age)
write.csv(age, here('output', 'age.csv'))

# compare gender ####
#gender <- as.matrix(t(table(dt$study, dt$gender)))
gender <- dt %>% count(study, gender) %>% group_by(study) %>% mutate(freq = n / sum(n))
chisq.test(dt$study, dt$gender)
write.csv(gender, here('output', 'gender.csv'))

# compare education ####
#ed <- as.matrix(t(table(dt$study, dt$education)))
ed <- dt %>% count(study, education) %>% group_by(study) %>% mutate(freq = n/sum(n))
chisq.test(dt$study, dt$education, simulate.p.value = TRUE)
write.csv(ed, here('output', 'ed.csv'))

# recode education into number of years
dt$ednum <- recode(dt$education, 'Middle School' = '8', 'High School Diploma' = '12', 
       'Some College' = '14', "Bachelor's Degree" = "16", "Master's Degree" = '18', 
       'Doctoral Degree' = '20')

dt$ednum <- as.integer(dt$ednum)
dt %>% group_by(study) %>% summarize(
  mean_education = mean(ednum)
)
t.test(dt[which(dt$study == 1),]$ednum, dt[which(dt$study == 2),]$ednum)

# compare race ####
#race <- as.matrix(t(table(dt$study, dt$race)))
race <- dt %>% count(study, race) %>% group_by(study) %>% mutate(freq = n/sum(n))
chisq.test(dt$study, dt$race, simulate.p.value = TRUE)
write.csv(race, here('output', 'race.csv'))

# compare income ####
#income <- as.matrix(t(table(dt$study, dt$income)))
income <- dt %>% count(study, income) %>% group_by(study) %>% mutate(freq = n/sum(n))
chisq.test(dt$study, dt$income, simulate.p.value = TRUE)
write.csv(income, here('output', 'income.csv'))

# recode income into number 
dt$incomenum <- recode(dt$income, "less than $10,000" = '5', "$10,000-$19,999" = '15', 
                    "$20,000-$29,999" = '25', "$30,000-$39,000" = '35', 
                    "$40,000-$49,999" = '45', "$50,000-$59,000" = '55', 
                    "$60,000-$69,999" = '65', "$70,000-$79,999" = '75', 
                    "$80,000-$89,999" = '85', "$90,000-$99,999" = '95', 
                    "$100,000-$109,999" = '105', "$110,000-$119,999" = '115', 
                    "$120,000-$129,999" = '125', "$130,000-$139,999" = '135', 
                    "$140,000-$149,999" = '145', "$150,000 or more" = '155')

dt$incomenum <- as.integer(dt$incomenum)
dt %>% group_by(study) %>% summarize(
  mean_income = mean(incomenum)
)
t.test(dt[which(dt$study == 1),]$incomenum, dt[which(dt$study == 2),]$incomenum)

# compare self-rated health ####
health <- dt %>% group_by(study) %>% summarize(
  mean_health = round(mean(physical_health), 2), 
  sd_health = round(sd(physical_health), 2)
)
t.test(dt[which(dt$study == 1),]$physical_health, dt[which(dt$study == 2),]$physical_health)
write.csv(health, here('output', 'health.csv'))

  
  