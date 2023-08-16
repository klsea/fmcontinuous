# arousal models
# facemorphs continuous analysis
# 5.8.23 KLS

# H2: OA will report lower arousal levels for negative (angry and sad) 
# facial expressions than YA

# load required packages
library(here)
library(tidyverse)
library(sjPlot)

# load source functions

# set hard-coded variables

# read data in
dt <- read.csv(here('data', 'ave_faces_ratings_aro_val_long.csv'))
cov <- read.csv(here('data', 'cov_long.csv'))[c(2,4:5)]

dt <- full_join(dt, cov, by = c('subnum', 'dimension'))

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
dt$age2 <- dt$age ^ 2# cov addition

# limit to arousal data
arsl <- dt %>% filter(dimension == 'arsl')

# model
arsl.model5 <- lm(rating ~ age + age2 * emotion * CoV, data = arsl)
summary(arsl.model5)

# save model output
tab_model(arsl.model5, file = here('output', 'arsl_cov_tab.xls'))

# graphs

# graph constants
lg = 18 # text size
sm = 14
custom_plot = list(theme(
  plot.title = element_text(size = 24),
  axis.title.x = element_text(size = lg), axis.text.x = element_text(size = sm),
  axis.title.y = element_text(size = lg), axis.text.y = element_text(size = sm), 
  legend.title = element_text(size = lg), legend.text = element_text(size = sm),
  strip.text.x = element_text(size = lg))
)
arsl <- arsl[complete.cases(arsl),]
ggplot(arsl, aes(CoV, rating, color = emotion, fill = emotion)) + 
  geom_point() + geom_smooth(method = 'lm') + 
  scale_fill_brewer(palette="Set1") + scale_color_brewer(palette="Set1") + 
  theme_bw() + theme(legend.position = 'top') + custom_plot
