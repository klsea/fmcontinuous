# graph naturalness
# facemorphs continuous analysis
# 6.7.23 KLS

# load required packages
library(here)
library(tidyverse)
library(ggplot2)

# load source functions

# set hard-coded variables

# read data in
dt <- read.csv(here('data', 'ave_faces_ratings_nat_long.csv'))

# create factors for graph
dt$emotion <- factor(dt$emotion)
dt$magnitude <-factor(dt$magnitude, ordered = TRUE, levels = c('low', 'medium', 'full'))

# scale age
dt$age <- as.numeric(dt$age)
dt$age <- scale(dt$age)

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

# arousal scatter plot
natplot <- ggplot(dt, aes(age, rating, color = emotion, fill = emotion)) + 
  geom_point(shape = 1) + 
  geom_smooth(method = 'lm') + 
  theme_bw() + theme(legend.position = 'top') + 
  scale_y_continuous(breaks=seq(1,7,1)) +
  facet_wrap(~magnitude) + ggtitle('naturalness') + custom_plot

natplot2 <- ggplot(dt, aes(age, rating, color = emotion, fill = emotion)) + 
  geom_point(shape = 1) + 
  geom_smooth(method = 'lm', formula = y ~ x + I(x^2), size = 1) +
  theme_bw() + theme(legend.position = 'top') + 
  scale_y_continuous(breaks=seq(1,7,1)) + ylab('Naturalness Rating') + xlab('Age') +
  scale_fill_discrete(name = 'Emotion') + scale_color_discrete(name = 'Emotion') +
  facet_wrap(~magnitude) + ggtitle('naturalness') + custom_plot

png(file = here('figs', 'nat_rating_by_age_quad.png'), width = 1000, height = 425)
natplot2
dev.off()
