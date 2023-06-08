# pull naturalness data from qualtrics
# facemorphs continuous analysis
# 6.7.23 KLS

# load required packages
library(here)
library(tidyverse)

# load source functions

# set hard-coded variables

# read data in
f2 <- read.csv(here('data', 'raw', 'Face Morphs 2 - Copy_June 6, 2023_09.07.csv'))
names <- read.csv(here('data', 'raw', 'rating_names.csv'), header = FALSE)

# remove first 3-4 rows
f2 <- f2[-1:-3,]

# only completes
f2 <- f2[which(f2$Finished == "True"),]

# assign subnum
f2$subnum <- seq(1:(nrow(f2))) + 100

# cut extraneous entry
f2 <- f2[c(20, 26:ncol(f2))]

# only naturalness ratings
dt <- f2[c(grep('atural', f2), 171, 1)]
dt <- dt[c(55:56, 1:54)]
colnames(dt) <- c('subnum', 'age', paste0('X',names$V4))

# get only numbers
d1 <- dt %>% 
  mutate_at(vars(X115_a_full:X89_s_med),
  funs(recode(., "1 - Very Unnatural" = "1", 
              "2 - Unnatural"= "2", 
              "3 - Somewhat Natural" = "3", 
              "4 - Somewhat Natural" = "4", 
              "5 - Natural" = "5", 
              "6 - Very Natural" = "6"
  )))
  
# make long
d2 <- d1 %>% pivot_longer(!c(subnum,age), names_to = 'video', values_to = 'rating')

# split names & make numeric
d3 <- d2 %>% separate(video, c('model', 'emotion', 'magnitude'))
d3$rating <- as.numeric(d3$rating)

#summarize across models
d4 <- d3 %>% group_by(subnum, age, emotion, magnitude) %>% summarize(
  rating = mean(rating, na.rm = TRUE)
)
rm(d1,d2,d3,dt,f2,names)

# add emotion names
d4$emotion <- recode(d4$emotion, a = 'angry', h = 'happy', s = 'sad')

# add full magnitude names
d4$magnitude <- recode(d4$magnitude, med = 'medium')

# save
write.csv(d4, here('data', 'ave_faces_ratings_nat_long.csv'), row.names = FALSE)
