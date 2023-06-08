# naturalness models
# facemorphs continuous analysis
# 6.7.23 KLS

# H1: OA will report higher valence levels for positive (happy) 
# facial expressions than YA

# load required packages
library(here)
library(tidyverse)
library(lmtest)
library(sjPlot)

# load source functions

# set hard-coded variables

# read data in
dt <- read.csv(here('data', 'ave_faces_ratings_nat_long.csv'))

# add Helmert contrast codes for emotion
dt$emotion <- factor(dt$emotion, levels = c('angry', 'sad', 'happy'))
levels(dt$emotion)
dt$emotion <- C(dt$emotion, helmert)
contrasts(dt$emotion)

# add Helmert contrast codes for magnitude
dt$magnitude <-factor(dt$magnitude, levels = c('low', 'medium', 'full'))
levels(dt$magnitude)
dt$magnitude <- C(dt$magnitude, helmert)
contrasts(dt$magnitude)

# scale age
dt$age <- scale(dt$age)

# add age^2 column
dt$age2 <- dt$age ^ 2

# m1 & 2
nat.model1 <- lm(rating ~ age * emotion, data = dt)
nat.model2 <- lm(rating ~ age * emotion * magnitude, data = dt)

# model comparison
anova(nat.model1, nat.model2) # model 2 wins

# m3
nat.model3 <- lm(rating ~ age + age2 * emotion, data = dt)

# model comparison
coxtest(nat.model2, nat.model3) # model 3 wins

# m4
nat.model4 <- lm(rating ~ age + age2 * emotion * magnitude, data = dt)

# model comparison
anova(nat.model3, nat.model4) # model 3 wins

# results
summary(nat.model3)

# save model output table
tab_model(nat.model1, nat.model2, nat.model3, nat.model4, file = here('output', 'naturalness_tab.xls'))

# means
dt %>% group_by(emotion) %>% summarize(
  m_rating = mean(rating, na.rm = TRUE), sd_rating = sd(rating, na.rm = TRUE)
)
