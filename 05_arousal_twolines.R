# arousal two lines test 
# facemorphs continuous analysis
# 5.26.23 KLS

# load required packages
library(here)
library(tidyverse)

# load source functions
source(here('scr', 'two_lines.R'))

# set hard-coded variables

# read data in
dt <- read.csv(here('data', 'ave_faces_ratings_aro_val_long.csv'))

# scale age
dt$age <- scale(dt$age)

arsl <- dt %>% filter(dimension == 'arsl')

# two lines test (across all mags)
a = twolines(rating ~ age, data = arsl)

cut_a <- 49.04 + 16.74*-.48
