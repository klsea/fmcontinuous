# graphs
# facemorphs continuous analysis
# 5.4.23 KLS

# load required packages
library(here)
library(tidyverse)
library(ggplot2)

# load source functions

# set hard-coded variables

# read data in
dt <- read.csv(here('data', 'ave_faces_ratings_aro_val_long.csv'))

# create factors for graph
dt$emotion <- factor(dt$emotion)
dt$magnitude <-factor(dt$magnitude, ordered = TRUE, levels = c('low', 'medium', 'full'))

# split data into valence and arousal
arsl <- dt %>% filter(dimension == 'arsl')
vln <- dt %>% filter(dimension == 'vln')

# arousal scatter plot
arslplot <- ggplot(arsl, aes(age, rating, color = emotion, fill = emotion)) + 
  geom_point(shape = 1) + 
  geom_smooth(method = 'lm') + 
  #geom_smooth(method = 'lm', formula = y ~ x + I(x^2), size = 1) +
  theme_bw() + theme(legend.position = 'top') + 
  scale_y_continuous(breaks=seq(1,7,1)) +
  facet_wrap(~magnitude) + ggtitle('arousal') 
ggsave(here('figs', 'arsl_rating_by_age.png'), arslplot)

arslplot2 <- ggplot(arsl, aes(age, rating, color = emotion, fill = emotion)) + 
  geom_point(shape = 1) + 
  #geom_smooth(method = 'lm') + 
  geom_smooth(method = 'lm', formula = y ~ x + I(x^2), size = 1) +
  theme_bw() + theme(legend.position = 'top') + 
  scale_y_continuous(breaks=seq(1,7,1)) +
  facet_wrap(~magnitude) + ggtitle('arousal') 
ggsave(here('figs', 'arsl_rating_by_age_quad.png'), arslplot2)

# valence scatter plot
vlnplot <- ggplot(vln, aes(age, rating, color = emotion, fill = emotion)) + 
  geom_point(shape = 1) + 
  geom_smooth(method = 'lm') + 
  #geom_smooth(method = 'lm', formula = y ~ x + I(x^2), size = 1) +
  theme_bw() + theme(legend.position = 'top') + 
  scale_y_continuous(breaks=seq(1,7,1)) +
  facet_wrap(~magnitude) + ggtitle('valence')
ggsave(here('figs', 'vln_rating_by_age.png'), vlnplot)

vlnplot2 <- ggplot(vln, aes(age, rating, color = emotion, fill = emotion)) + 
  geom_point(shape = 1) + 
  #geom_smooth(method = 'lm') + 
  geom_smooth(method = 'lm', formula = y ~ x + I(x^2), size = 1) +
  theme_bw() + theme(legend.position = 'top') + 
  scale_y_continuous(breaks=seq(1,7,1)) +
  facet_wrap(~magnitude) + ggtitle('valence')
ggsave(here('figs', 'vln_rating_by_age_quad.png'), vlnplot)
