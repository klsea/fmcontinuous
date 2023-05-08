# arousal models
# facemorphs continuous analysis
# 5.8.23 KLS

# H2: OA will report lower arousal levels for negative (angry and sad) 
# facial expressions than YA

# load required packages
library(here)
library(tidyverse)

# load source functions

# set hard-coded variables

# read data in
dt <- read.csv(here('data', 'ave_faces_ratings_aro_val_long.csv'))

# add Helmert contrast codes for emotion
dt$emotion <- factor(dt$emotion, levels = c('happy', 'angry', 'sad'))
levels(dt$emotion)
emo.helmert = matrix(c(1, -0.5, -0.5, 0, 1, -1), ncol = 2)
emo.helmert
contrasts(dt$emotion) = emo.helmert

# add Helmert contrast codes for magnitude
dt$magnitude <-factor(dt$magnitude, levels = c('low', 'medium', 'full'))
levels(dt$magnitude)
mag.helmert = matrix(c(-0.5, -0.5, 1, -1, 1, 0), ncol = 2)
mag.helmert
contrasts(dt$emotion) = mag.helmert

# add age^2 column
dt$age2 <- dt$age ^ 2

# test H2
arsl <- dt %>% filter(dimension == 'arsl')

arsl.model1 <- lm(rating ~ age * emotion, data = arsl)
summary(arsl.model1)

# exploratory 1 - add magnitude
arsl.model2 <- lm(rating ~ age * emotion * magnitude, data = arsl)
summary(arsl.model2)

# model comparison - model 1 wins
anova(arsl.model1, arsl.model2)

# exploratory 2 - quadratic effects of age
arsl.model3 <- lm(rating ~ age + age2 * emotion, data = arsl)
summary(arsl.model3)

# exploratory 3 - add magnitude
arsl.model4 <- lm(rating ~ age + age2 * emotion * magnitude, data = arsl)
summary(arsl.model4)

# model comparisons
anova(arsl.model1, arsl.model3, arsl.model4) # model 3 wins
