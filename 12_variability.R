# variability analyses
# facemorphs continuous analysis - isd and cov
# 7.25.23 KLS

# load required packages
library(here)
library(tidyverse)

# load source functions

# set hard-coded variables

# read data in
d1 <- read.csv(here('data', 'raw', 'Face Morphs 1 - Copy_June 6, 2023_08.59.csv'), skip = 1)
d2 <- read.csv(here('data', 'raw', 'Face Morphs 2 - Copy_June 6, 2023_09.07.csv'), skip = 1)

# filter out unfinished
d1 <- d1[which(d1$Finished == "True" & d1$Response.Type == 'IP Address'),]
d2 <- d2[which(d2$Finished == "True" & d2$Response.Type == 'IP Address'),]

# rename columns
d1 <- rename(d1, id = Response.ID, age = What.is.your.age.)
d2 <- rename(d2, id = Response.ID, age = What.is.your.age.)

# new ids
d1$id <- seq(1,100,1)
d2$id <- seq(101,208,1)

# data cleanup ####
# clean data frames
d1a <- d1[,c(9, 20, grep('Arousal', colnames(d1)))]
d1v <- d1[, c(9, 20, grep('Valence', colnames(d1)))]
d2a <- d2[,c(9, 20, grep('Arousal', colnames(d2)))]
d2v <- d2[, c(9, 20, grep('Valence', colnames(d2)))]

# remove extra columns
d2a <- d2a[,-3]
d2v <- d2v[,-3]
rm(d1, d2)

# recode variables
d1a <- d1a %>% mutate(across(Arousal.Rating:Arousal.Rating.53, ~recode(., "7 - Not At All Aroused" = "7", 
                                                                       "1 - Very Aroused" = "1"))) 
d1v <- d1v %>% mutate(across(Valence.Rating:Valence.Rating.53,
                ~recode(., "7 - Very Negative" = "7", "6 - Moderately Negative" = "6", 
                        "5 - Slightly Negative" = "5", "4 - Neutral" = "4", "3 - Slightly Positive" = "3", 
                        "2 - Moderately Positive" = "2", "1 - Very Positive" = "1")))

d2a <- d2a %>% mutate(across(Arousal.Rating:Arousal.Rating.53, ~recode(., "7 - Not At All Aroused" = "7", 
                                                                       "1 - Very Aroused" = "1")))
d2v <- d2v %>% mutate(across(Valence.Rating:Valence.Rating.53,
                             ~recode(., "7 - Very Negative" = "7", "6 - Moderately Negative" = "6", 
                                     "5 - Slightly Negative" = "5", "4 - Neutral" = "4", "3 - Slightly Positive" = "3", 
                                     "2 - Moderately Positive" = "2", "1 - Very Positive" = "1")))
# make numeric
d1a[2:56] <- d1a[2:56] %>% mutate_if(is.character, as.numeric)
d1v[2:56] <- d1v[2:56] %>% mutate_if(is.character, as.numeric)
d2a[2:56] <- d2a[2:56] %>% mutate_if(is.character, as.numeric)
d2v[2:56] <- d2v[2:56] %>% mutate_if(is.character, as.numeric)

# make long
d1a <- d1a %>% pivot_longer(-c(id, age), names_to = 'Question', values_to = 'Rating')
d1v <- d1v %>% pivot_longer(-c(id, age), names_to = 'Question', values_to = 'Rating')
d2a <- d2a %>% pivot_longer(-c(id, age), names_to = 'Question', values_to = 'Rating')
d2v <- d2v %>% pivot_longer(-c(id, age), names_to = 'Question', values_to = 'Rating')

# variability calculations ####
# d1 = 100 subs, d2 = 108 subs
d1a <- d1a %>% group_by(id, age) %>% summarize(
  a_imean = mean(Rating, na.rm = TRUE),
  a_isd = sd(Rating, na.rm = TRUE), 
  a_cov = a_isd/a_imean
)
d1v <- d1v %>% group_by(id, age) %>% summarize(
  v_imean = mean(Rating, na.rm = TRUE),
  v_isd = sd(Rating, na.rm = TRUE), 
  v_cov = v_isd/v_imean
) 
d2a <- d2a %>% group_by(id, age) %>% summarize(
  a_imean = mean(Rating, na.rm = TRUE),
  a_isd = sd(Rating, na.rm = TRUE), 
  a_cov = a_isd/a_imean
)
d2v <- d2v %>% group_by(id, age) %>% summarize(
  v_imean = mean(Rating, na.rm = TRUE),
  v_isd = sd(Rating, na.rm = TRUE), 
  v_cov = v_isd/v_imean
)

# recombine data frames
d1 <- merge(d1a, d1v)
d2 <- merge(d2a, d2v)
dt <- rbind(d1, d2)
rm(d1,d1a,d1v,d2,d2a,d2v)

# analysis ####
# remove missing
dt <- dt[complete.cases(dt),]

# coefficient of variation
cor(dt$a_cov, dt$v_cov)

ggplot(dt, aes(x = a_cov, y = v_cov)) + 
  geom_point() + geom_smooth(method='lm') + theme_bw()

# correlate with age
cor.test(dt$age, dt$a_cov)  #arousal
cor.test(dt$age, dt$v_cov) #valence

# make long
d1 <- dt %>% pivot_longer(cols = c('a_cov', 'v_cov'), 
                          names_to = 'Rating', values_to = 'CoV')
d1 <- d1[c(1:2,7:8)]
d1$Rating <- recode(d1$Rating, 'a_cov' = 'arousal', 'v_cov' = 'valence')

#graph
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
d1$age <- scale(d1$age)

ggplot(d1, aes(x = age, y = CoV, color = Rating, fill = Rating)) + 
  geom_point() + geom_smooth(method='lm') + scale_fill_brewer(palette="Set2") + 
  scale_color_brewer(palette="Set2") + theme_bw() + theme(legend.position = 'top') + custom_plot

# save cov
# recode for consistency
d1$Rating <- recode(d1$Rating, 'arousal' = 'arsl', 'valence' = 'vln')
d1 <- rename(d1, dimension = Rating, subnum = id)

# save
write.csv(d1, here('data', 'cov_long.csv'))

