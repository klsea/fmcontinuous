# valence models
# facemorphs continuous analysis
# 5.4.23 KLS

# H1: OA will report higher valence levels for positive (happy) 
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

# test H1
vln <- dt %>% filter(dimension == 'vln')

vln.model1 <- lm(rating ~ age * emotion, data = vln)
summary(vln.model1)

# exploratory analysis 1 - add magnitude
vln.model2 <- lm(rating ~ age * emotion * magnitude, data = vln)
summary(vln.model2)

vln.model2 <- lm(rating ~ age2 * emotion * magnitude, data = vln)
summary(vln.model2)

# model comparison
anova(vln.model1, vln.model2) # model 2 wins

# exploratory analysis 2 - quadratic effects
vln.model3 <- lm(rating ~ age + age2 * emotion, data = vln)
summary(vln.model3)

# add magnitude
vln.model4 <- lm(rating ~ age + age2 * emotion * magnitude, data = vln)
summary(vln.model4)

# model comparison
anova(vln.model2, vln.model3, vln.model4) # model 4 wins
