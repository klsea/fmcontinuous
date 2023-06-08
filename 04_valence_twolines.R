# valence two lines test 
# facemorphs continuous analysis
# 5.9.23 KLS

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

# effect of age on rating - happy expressions only
# subset data frame
posvln <- dt %>% filter(dimension == 'vln' & emotion == 'happy')

# two lines test (across all mags)
a = twolines(rating ~ age, data = posvln)

# effect of age on rating - only negative expressions (angry or sad across all magnitudes)
# subset data frame
negvln <- dt %>% filter(dimension == 'vln' & emotion == 'angry' | dimension == 'vln' & emotion == 'sad')
# two lines test (across all mags)
b = twolines(rating ~ age,data = negvln)

cut_a <- 49.04 + 16.74*
cut_b <- 49.04 + 16.74*

