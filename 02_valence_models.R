# valence models
# facemorphs continuous analysis
# 5.8.23 KLS

# H1: OA will report higher valence levels for positive (happy) 
# facial expressions than YA

# load required packages
library(here)
library(tidyverse)
library(lmtest)
library(sjPlot)
library(emmeans)

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

# test H1
vln <- dt %>% filter(dimension == 'vln')

vln.model1 <- lm(rating ~ age * emotion, data = vln)
#summary(vln.model1)

# exploratory analysis 1 - add magnitude
vln.model2 <- lm(rating ~ age * emotion * magnitude, data = vln)
summary(vln.model2)

# model comparison
anova(vln.model1, vln.model2) # model 2 wins

# exploratory analysis 2 - quadratic effects
vln.model3 <- lm(rating ~ age + age2 * emotion, data = vln)
#summary(vln.model3)

# model comparison
coxtest(vln.model2, vln.model3)

# add magnitude
vln.model4 <- lm(rating ~ age + age2 * emotion * magnitude, data = vln)
summary(vln.model4)

# model comparison
anova(vln.model3, vln.model4)
  
# save model output table
tab_model(vln.model1, vln.model2, vln.model3, vln.model4, file = here('output', 'valence_tab.xls'))

# simple slopes for emotion x mag 
#em_emo_mag <- emmeans(vln.model4, ~ emotion * magnitude)
#ss_table <- contrast(em_emo_mag, "revpairwise",by="emotion")#,adjust="none")
#write.csv(ss_table, here('output', 'vln_emoxmag_emeans.csv'))
#ss_emo_mag_plot <- emmip(vln.model4, magnitude ~ emotion, CIs=TRUE)
#ggsave(here('figs', 'vln_emo_x_mag_simple_slopes.png'), ss_emo_mag_plot)

