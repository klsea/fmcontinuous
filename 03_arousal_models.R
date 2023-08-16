# arousal models
# facemorphs continuous analysis
# 5.8.23 KLS

# H2: OA will report lower arousal levels for negative (angry and sad) 
# facial expressions than YA

# load required packages
library(here)
library(tidyverse)
library(lmtest)
library(sjPlot)

# load source functions

# set hard-coded variables

# read data in
dt <- read.csv(here('data', 'ave_faces_ratings_aro_val_long.csv'))

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

# test H2
arsl <- dt %>% filter(dimension == 'arsl')

arsl.model1 <- lm(rating ~ age * emotion, data = arsl)
summary(arsl.model1)

# exploratory 1 - add magnitude
arsl.model2 <- lm(rating ~ age * emotion * magnitude, data = arsl)
#summary(arsl.model2)

# model comparison - model 1 wins
mc1 <- anova(arsl.model1, arsl.model2)

# exploratory 2 - quadratic effects of age
arsl.model3 <- lm(rating ~ age + age2 * emotion, data = arsl)
summary(arsl.model3)

# model comparison - 
mc2 <- coxtest(arsl.model1, arsl.model3)

# exploratory 3 - add magnitude
arsl.model4 <- lm(rating ~ age + age2 * emotion * magnitude, data = arsl)
#summary(arsl.model4)

# model comparisons
mc3 <- anova(arsl.model3, arsl.model4)

# model 3 wins!

# save model output table
tab_model(arsl.model1, arsl.model2, arsl.model3, arsl.model4, file = here('output', 'arsl_tab.xls'))


