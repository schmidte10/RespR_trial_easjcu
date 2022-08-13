#--- loading packages ---#
library(tidyverse)
library(respR)
library(janitor)

#--- import data ---#
Cycle_1 <- read_delim("Dell/Experiment_ 01 August 2022 10 14AM/All slopes/Cycle_1.txt", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)

#--- format data ---# 
#make sure you are looking at the correct chamber
apoly <- Cycle_1 %>%
  rename(time = `Seconds from start for linreg`, 
         oxygen = `ch1 po2`) %>%
  select(c("time","oxygen"))  

#inspect data
apoly.inspect <- inspect(apoly) 
#--- look at background respiration rates ---# 
# import background resp data file 
pre <- read_delim("Dell/Experiment_ 01 August 2022 09 19AM/All slopes/Cycle_1.txt", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)

pre <-  pre %>% 
  rename(time = Time, 
         oxygen = `ch1 po2`) %>%
  select(c("time","oxygen"))  

bg_pre <- subset(pre, from = pre[1,1], to = tail(pre$time, n=1), by ="time") %>% 
  calc_rate.bg()
bg_pre

#--- calculate MMR ---# 
# wait time was 5 second added a 10 sec buffer to start measurements 
# 15 seconds after the fish was put  in to ensure proper mixing of water 
# water in chamber completed a circuit once every 15 seconds
buffer = 10
measure = 230 

# get mmr 
apoly_mmr <- auto_rate(apoly, width = 0.26, method = "rolling") 

# summary 
summary(apoly_mmr)
which.min(apoly_mmr$rate)
min(apoly_mmr$rate)
# plot 
plot(apoly_mmr, pos = 108)

#--- adjusting MMR value for background resp ---# 
apoly_mmr_adj <- adjust_rate(apoly_mmr, 
                             by= bg_pre, 
                             by2= bg_pre,
                             method="linear")

summary(apoly_mmr_adj)

#--- convert MMR value to units that you will report values in ---# 
apoly_mmr_conv <- convert_rate(apoly_mmr_adj, 
                               oxy.unit = "%Air", 
                               time.unit = "secs", 
                               output.unit = "mg/h", 
                               volume = 1.5, # volumr of chamber
                               S = 35, # salinity 
                               t = 30) # temperature 

summary(apoly_mmr_conv)
which.min(apoly_mmr_conv$rate.output) # which regression has the highest rate 
                                      # note because oxygen delcines the highest rate will be the lowest value
min(apoly_mmr_conv$rate.output) #lowest value/highest rate 

mmr_data <- summary(apoly_mmr_conv, pos = 108, export = TRUE)


