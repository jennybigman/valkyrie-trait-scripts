###### CODE SUMMARY #####
# Description: Predict min and max swim speed based on avg length so juveniles and adults 

# Output: .csv file of trait values, including min and max swim speed
#
# Programmer: Jennifer Bigman (NOAA/NMFS/OST/ Marine Ecosystems Division) 
# 
# Date: February 10, 2025
#
# Modified: 
############################################################################################
	library(brms)
	library(FishLife)
	library(rfishbase)
	library(tidyverse)
  library(here)

	all_traits <- read_csv("all_traits.csv")

	# predict swim speed
	load(file = "swim_speed_mod.rda")

	# predict using model fit
	
	# adult swim speed
	ad_new_dat <- crossing(
		length_cm = all_traits$ad_avg_length,
		speed_type = c(0,1)
	)
	
	ad_p <- predict(mod5, newdata = ad_new_dat, re_formula = NA) |>
		as_tibble()
	
	ad_p <- bind_cols(ad_p, ad_new_dat)
	
	ad_p_trim <- ad_p |>
		select(Estimate, length_cm, speed_type)
	
	ad_p_wide <- ad_p_trim |>
		pivot_wider(names_from = speed_type, values_from = Estimate) |>
		rename(ad_avg_length = length_cm)

	ad_p_wide <- ad_p_wide	|>
		rename('AdltMinSwimSpeed_bl/s' = '0',
					 'AdltMaxSwimSpeed_bl/s' = '1')
	
	# juvenile swim speed
	jv_new_dat <- crossing(
		length_cm = all_traits$jv_avg_length,
		speed_type = c(0,1)
	)
	
	jv_p <- predict(mod5, newdata = jv_new_dat, re_formula = NA) |>
		as_tibble()
	
	jv_p <- bind_cols(jv_p, jv_new_dat)
	
	jv_p_trim <- jv_p |>
		select(Estimate, length_cm, speed_type)
	
	jv_p_wide <- jv_p_trim |>
		pivot_wider(names_from = speed_type, values_from = Estimate) |>
		rename(jv_avg_length = length_cm)

	jv_p_wide <- jv_p_wide	|>
		rename('JvnlMinSwimSpeed_bl/s' = '0',
					 'JvnlMaxSwimSpeed_bl/s' = '1')
	
	# add back to dataset
	all_traits <- left_join(all_traits, ad_p_wide, by = "ad_avg_length")
	all_traits <- left_join(all_traits, jv_p_wide, by = "jv_avg_length")
	
	#### convert body length per second to cm/s ####
	all_traits <- all_traits |>
		mutate(`AdltMinSwimSpeed_cm/s` = `AdltMinSwimSpeed_bl/s` * ad_avg_length,
					 `AdltMaxSwimSpeed_cm/s` = `AdltMaxSwimSpeed_bl/s` * ad_avg_length,
					 `JvnlMinSwimSpeed_cm/s` = `JvnlMinSwimSpeed_bl/s` * jv_avg_length,
					 `JvnlMaxSwimSpeed_cm/s` = `JvnlMaxSwimSpeed_bl/s` * jv_avg_length)
	
	write_csv(all_traits, "all_traits.csv")
