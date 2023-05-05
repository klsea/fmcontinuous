# make data long
# facemorphs continuous analysis
# 5.4.23 KLS

# load required packages
library(here)
library(tidyverse)

# load source functions

# set hard-coded variables

# read data in
dt <- read.csv(here('data', 'ave_faces_ratings_aro_val.csv'))

# make long
dt <- dt %>% pivot_longer(
  cols = contains('arsl') | contains('vln'), 
  names_to = c('emotion', 'magnitude', 'dimension'), 
  names_sep = '\\.',
  values_to = 'rating'
)

# add emotion names
dt$emotion <- recode(dt$emotion, a = 'angry', h = 'happy', s = 'sad')

# add full magnitude names
dt$magnitude <- recode(dt$magnitude, med = 'medium')

# save
write.csv(dt, here('data', 'ave_faces_ratings_aro_val_long.csv'), row.names = FALSE)