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