### resp data trial  ###  
### loading packages ###
library(tidyverse)
library(respR)

### intermittent flow respiromatery ###
df <- intermittent.rd
urchin_int <- inspect(intermittent.rd)

head(urchin_int$dataframe)
inspect(intermittent.rd[1:1800,])
inspect(intermittent.rd[1:1800,],width=0.25)

urchin_int_rates <- calc_rate(urchin_int, 
                              from = c(200, 2300, 4100), 
                              to = c(1800, 3200, 4600), 
                              by = "time")

plot(urchin_int_rates, pos = 1)
plot(urchin_int_rates, pos = 2)
plot(urchin_int_rates, pos = 3) 

summary(urchin_int_rates)

calc_rate(urchin_int, from=400, to = 1600)

u_rep1 <- subset_data(urchin_int, from = 0, to = 1900, by = "time")
ar_lin_1 <- auto_rate(u_rep1)
summary(ar_lin_1)

plot(ar_lin_1, pos = 1)

ar_low_1 <- auto_rate(u_rep1, method = "lowest", width = 300, by= "time")
ar_high_1 <- auto_rate(u_rep1, method = "highest", width = 300, by= "time")
head(ar_high_1$rate)
ar_high_1$rate[1]

#converting to absolute rate
rep1_abs_rate <- convert_rate(ar_high_1, 
                              oxy.unit = "mg/L", 
                              time.unit = "s", 
                              output.unit = "mg/h", 
                              volume = 2.379)
rep1_abs_rate
rep1_abs_rate$summary$rate.output.abs <- abs(rep1_abs_rate$summary$rate.output)
rep1_abs_rate$summary

rep1_ms_rate <- convert_rate(ar_high_1, 
                             oxy.unit = "mg/L", 
                             time.unit = "s", 
                             output.unit = "mg/h/g", 
                             volume = 2.379, 
                             mass = 0.006955)
rep1_ms_rate

summary(rep1_ms_rate)
head(rep1_ms_rate$rate.output) 

rep1_df <- summary(rep1_ms_rate, export = TRUE)

### trying example with own data ### 
Cycle_1 <- read.csv("C:/Users/Elliott/OneDrive - James Cook University/PhD dissertation/Data/Resp/Asus/Experiment_ 01 August 2022 05 09PM/All slopes/Cycle_1.txt", sep=";")
apoly <- Cycle_1 %>% 
  rename(time = Seconds.from.start.for.linreg, 
         oxygen = ch1.po2) %>%
  select(c("time","oxygen"))
inspect(apoly)  
inspect(intermittent.rd[1:60,])
inspect(intermittent.rd[1:223,],width=0.25)

apoly_max <- auto_rate(apoly, width = 0.25)
summary(apoly_max)

apoly_high <- auto_rate(apoly, method = "highest", width = 60, by= "time")
head(apoly_high$rate)
apoly_high$rate[1]
### ^for max for cycle data ### 
Firesting <- read_table2("C:/Users/Elliott/OneDrive - James Cook University/PhD dissertation/Data/Resp/Dell/Experiment_ 01 August 2022 05 19PM/Oxygen data raw/Firesting.txt")[-c(1:16),]
names(Firesting)[2] <- "DateTime" 

apoly2 <- Firesting %>% 
  row_to_names(row_number = 1, remove = TRUE, remove_rows_above = TRUE)%>% 
  clean_names()%>%
  select(1:7)  %>%
  rename(date.time = time, 
         time = `hh_mm_ss`, 
         Ch4 = ch1,
         Ch1 = time_2, 
         Ch2 = `s`, 
         Ch3 = comment)
 

apoly2$Ch1 <- as.numeric(apoly2$Ch1)
apoly2$Ch4 <- as.numeric(apoly2$Ch4)
apoly2$time <- as.numeric(apoly2$time)

### long resp example ### 

zeb <- inspect(zeb_intermittent.rd)
inspect(zeb_intermittent.rd[1:18000, ])

bg_pre <- subset_data(zeb, from = 0, to = 4999, by = "time") %>%
  calc_rate.bg()
bg_pre
bg_post <- subset_data(zeb, from = 75140, to = 79251, by = "time") %>%
  calc_rate.bg()
bg_post
buffer <- 120   # 2 mins buffer (wait period)
measure <- 600  # 10 mins measure (measure period)
zeb_rep_1 <- subset_data(zeb, 
                         from = 5000 + buffer, 
                         to = 5000 + buffer + measure, 
                         by = "time")
zeb_mmr <- auto_rate(zeb_rep_1)
zeb_mmr <- auto_rate(zeb_rep_1, width = 0.4)
summary(zeb_mmr)
plot(zeb_mmr, pos = 2)
plot(zeb_mmr, pos = 6)

zeb_mmr <- auto_rate(zeb_rep_1, width = 0.4, method="highest")
plot(zeb_mmr, pos = 1)

# define buffer and measure periods
buffer <- 120  # 2 mins buffer
measure <- 420  # 7 mins measure

## start rows for each rep using sequence function (from, to, by)
reps <- seq(5840, 74480, 660)
## data starts - apply buffer
starts <- reps + buffer
## data ends - apply buffer and measure period
ends <- reps + buffer + measure

## Empty list for saving results
zeb_smr <- list()

## loop
for (i in 1:105) {
  st <- starts[i]  # start time
  et <- ends[i]  # end time
  
  ## subset replicate and pipe the result into auto_rate
  zeb_smr[[i]] <- subset_data(zeb, from = st, to = et, by = "time") %>%
    auto_rate(method = "lowest", width = 180, by = "time", plot = FALSE)
}
smr_rate <- sapply(zeb_smr, function(z) z$rate[1])
plot(smr_rate, ylim = rev(range(smr_rate)))

## adjust values for background 

zeb_smr_adj <- lapply(zeb_smr, function(z) adjust_rate(z, 
                                                       by = bg_pre, 
                                                       by2 = bg_post,
                                                       method = "linear"))
summary(zeb_smr_adj[[10]], pos = 1)
summary(zeb_smr_adj[[90]], pos = 1)

zeb_smr_conv <- lapply(zeb_smr_adj, function(z) convert_rate(z,
                                                             oxy.unit = "mg/L", 
                                                             time.unit = "secs",
                                                             output.unit = "mg/h/g",
                                                             volume = 0.12,         
                                                             mass = 0.0009))



zeb_smr_all <- sapply(zeb_rmr_conv, function(z) z$rate.output[1])
plot(zeb_smr_all, ylim = rev(range(zeb_smr_all)))

# 10% quantile cutoff value
co <- quantile(zeb_smr_all, 0.9)

zeb_smr_final <- mean(zeb_smr_all[zeb_smr_all > co])
zeb_smr_final




# Import and inspect raw data ---------------------------------------------
# Importing would normally be the first step, e.g. import_file("path/to/file")
zeb <- inspect(zeb_intermittent.rd)
zeb_df <- as.data.frame(zeb$dataframe)
# Background --------------------------------------------------------------
bg_pre <- subset_data(zeb, from = 0, to = 4999, by = "time") %>%
  calc_rate.bg()
bg_post <- subset_data(zeb, from = 75140, to = 79251, by = "time") %>%
  calc_rate.bg()

# Replicate structure -----------------------------------------------------
buffer <- 120   # 2 mins buffer
measure <- 420  # 7 mins measure
reps <- seq(5840, 74480, 660) ## start rows 
starts <- reps + buffer ## data starts
ends <- reps + buffer + measure ## data ends

# Subset each replicate ---------------------------------------------------
zeb_smr_subsets <- apply(cbind(starts,ends), 1, function(z) subset_data(zeb,
                                                                        from = z[1],
                                                                        to = z[2],
                                                                        by = "time"))

# auto_rate on each replicate ---------------------------------------------
zeb_smr <- lapply(zeb_smr_subsets, function(z) auto_rate(z,
                                                         method = "lowest", 
                                                         width = 180, 
                                                         by = "time", 
                                                         plot = FALSE))

# Adjust ------------------------------------------------------------------
zeb_smr_adj <- lapply(zeb_smr, function(z) adjust_rate(z, 
                                                       by = bg_pre, 
                                                       by2 = bg_post,
                                                       method = "linear"))

# Convert -----------------------------------------------------------------
zeb_smr_conv <- lapply(zeb_smr_adj, function(z) convert_rate(z,
                                                             oxy.unit = "mg/L", 
                                                             time.unit = "secs",
                                                             output.unit = "mg/h/g",
                                                             volume = 0.12,         
                                                             mass = 0.0009))

# Extract rates -----------------------------------------------------------
zeb_smr_all <- sapply(zeb_smr_conv, function(z) z$rate.output[1])

# Calculate final SMR -----------------------------------------------------
co <- quantile(zeb_smr_all, 0.9) # 10% quantile cutoff value
zeb_smr_final <- mean(zeb_smr_all[zeb_smr_all > co]) # Mean of all rates above cutoff value
zeb_smr_final


##########
apoly2 <- apoly2 %>% 
  select(time, Ch4)
apoly <- inspect(apoly2) 

#7511 

bg_pre <- subset_data(apoly, from = 25000, to = 28816, by = "time") %>% 
  calc_rate.bg()

wait <- 5
flush <- 180 
measure <- 240 
reps <- seq(28816,44387, 425)
starts <- reps + 13 
ends <- reps + measure 

apoly_smr_subsets <- apply(cbind(starts, ends), 1, function(z) subset_data(apoly, 
                                                                      from = z[1], 
                                                                      to = z[2], 
                                                                      by = "time"))

apoly_smr <- lapply(apoly_smr_subsets, function(z) auto_rate(z,
                                                         method = "linear", 
                                                         width = 60,
                                                         by = "time", 
                                                         plot = TRUE))

####################################################################################
apoly_smr <- list()

## loop
for (i in 1:37) {
  st <- starts[i]  # start time
  et <- ends[i]  # end time
  
  ## subset replicate and pipe the result into auto_rate
  apoly_smr[[i]] <- subset_data(apoly2, from = st, to = et, by = "time") %>%
    auto_rate(method = "lowest", width = 60, by = "time", plot = F)
}


smr_rate <- sapply(apoly_smr, function(z) z$rate[1])
plot(smr_rate, ylim = rev(range(smr_rate)))

####################################################################################

apoly_smr_adj <- lapply(apoly_smr, function(z) adjust_rate(z, 
                                                       by = bg_pre,
                                                       by2 = bg_pre,
                                                       method = "linear"))

apoly_smr_conv <- lapply(apoly_smr_adj, function(z) convert_rate(z,
                                                             oxy.unit = "%Air", 
                                                             time.unit = "secs",
                                                             output.unit = "mg/h",
                                                             t=30,
                                                             S=35,
                                                             volume = 1.5))


apoly_smr_all <- sapply(apoly_smr_conv, function(z) z$rate.output[1])

co <- quantile(apoly_smr_all, 0.9) # 10% quantile cutoff value
apoly_smr_final <- mean(apoly_smr_all[apoly_smr_all > co]) # Mean of all rates above cutoff value
apoly_smr_final
abs(apoly_smr_final)
