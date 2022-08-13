#--- insert libraries ---# 
library(tidyverse)
library(respR) 
library(janitor)

#--- import data ---# 
apoly2 <- import_file("./Experiment_ 01 August 2022 11 37AM/Oxygen data raw/Firesting.txt") %>% 
  clean_names()%>%
  select(c(1:3,5:8))  %>%
  rename(Time = comment,  
         Oxygen = oxygen_ch2) %>% 
  select(c("Time","Oxygen","time_s"))

apoly2$Time <- as.numeric(apoly2$Time)
apoly2$Oxygen <- as.numeric(apoly2$Oxygen)
apoly <- inspect(apoly2)

#--- analysis loop ---#
buffer = 5
measure = 240   
flush = 180 

cycle.time = buffer + measure + flush
## start rows for each rep using sequence function 
reps <- seq(8360.01, 21960.85, cycle.time)
#2835, 19254
starts <- reps + buffer 

ends = reps + buffer + measure

apoly_rmr <- list()

for(i in 1:33){ 
  st <- starts[i] 
  et <- ends[i] 
  
  apoly_rmr[[i]] <- subset_data(apoly, from = st, to = et, by = "Time") %>% 
    auto_rate(method = "linear", plot = TRUE)
}

rmr_rate <- sapply(apoly_rmr, function(z) z$rate[1]) 
plot(rmr_rate, ylim = rev(range(rmr_rate)))

#--- adjusting rate ---# 
apoly_rmr_adj <- lapply(apoly_rmr, function(z) adjust_rate(z, 
                                                           by=bg_pre, 
                                                           by2=bg_pre, 
                                                           method = "linear"))

summary(apoly_rmr_adj[[5]], pos = 1)

summary(apoly_rmr_adj[[28]], pos = 1)

#--- convert rates ---# 
apoly_rmr_conv <- lapply(apoly_rmr_adj, function(z) convert_rate(z, 
                                                                  oxy.unit = "%Air", 
                                                                  time.unit = "secs", 
                                                                  output.unit = "mg/h/kg", 
                                                                  volume = 1.50,  
                                                                  mass = 0.03170,
                                                                  S = 35, 
                                                                  t = 30))
summary(apoly_rmr_conv[[33]], pos = 1)
summary(apoly_rmr_conv[[28]], pos = 1)

apoly_rmr_all <- sapply(apoly_rmr_conv, function(z) z$rate.output[1])
plot(apoly_rmr_all, ylim = rev(range(apoly_rmr_all)))

co <- quantile(apoly_rmr_all, 0.9)
apoly_rmr_final <- mean(apoly_rmr_all[c(1:33)])
apoly_rmr_final
# 5.669