# means
# facemorphs continuous analysis
# 5.26.23 KLS

# load required packages
library(here)
library(tidyverse)

# load source functions

# set hard-coded variables

# read data in
dt <- read.csv(here('data', 'ave_faces_ratings_aro_val_long.csv'))

# age means/sd
dt %>% group_by(subnum) %>% summarize(age = mean(age)) %>% summarize(
  m_age = mean(age), sd_age = sd(age)
)

# average arousal ratings
arsl <- dt %>% filter(dimension == 'arsl')

arsl %>% group_by(subnum, emotion) %>% summarize(
  m_rating = mean(rating, na.rm = TRUE),
  ) %>% group_by(emotion) %>% summarize(
    gm_rating = mean(m_rating, na.rm = TRUE), 
    sd_rating = sd(m_rating, na.rm = TRUE)
  )

# overall arousal mean
arsl %>% group_by(subnum, emotion) %>% summarize(
  m_rating = mean(rating, na.rm = TRUE),
) %>% group_by(emotion) %>% summarize(
  gm_rating = mean(m_rating, na.rm = TRUE), 
  sd_rating = sd(m_rating, na.rm = TRUE)
) %>% summarize(
  mean = mean(gm_rating), 
  sd = sd(gm_rating), 
  se = sd/sqrt(207))

