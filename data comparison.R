	
	# order columns and make sure Ron's dataset and mine sync

	setwd("~/Library/CloudStorage/Dropbox/NOAA OST/IBM")

	library(tidyverse)

	# load my trait data
	all_traits <- read_csv("all_traits.csv")


	# change col names to match Ron's file
#	all_traits <- all_traits |>
#		select(-contains("...")) |>
#		select(-contains("bl")) |>
#		rename(
#			AdltMinSwimSpeed_cm.s = `AdltMinSwimSpeed_cm/s`,
#			AdltMaxSwimSpeed_cm.s = `AdltMaxSwimSpeed_cm/s`,
#    	JvnlMinSwimSpeed_cm.s = `JvnlMinSwimSpeed_cm/s`,
#    	JvnlMaxSwimSpeed_cm.s = `JvnlMaxSwimSpeed_cm/s`
#		) |>
#		mutate(LarvMinSwimSpeed_cm.s = 0,
#					 LarvMaxSwimSpeed_cm.s = 0)
	
	# read in Ron's file
	old_d <- read.csv("speciesData.csv")
	name_order <- names(old_d)
	
	# fow now, add number of batches from Ron's file
#	num_batch <- old_d |>
#		select(Species, NumBatches)
#	
#	all_traits <- left_join(all_traits, num_batch)
	
	# what cols are different
	new_d <- names(all_traits)

	setdiff(new_d, name_order)
	setdiff(name_order, new_d)
	
	# order columns
	all_traits <- all_traits |>
		select(all_of(name_order), everything())
	
	write_csv(all_traits, "all_traits.csv")
	
	# check if same
	all_traits_trim <- all_traits |>
		select(-WeightAsymptotic, -ad_avg_length, -jv_avg_length, -MaxDepth, -MinDepth)

	identical(all_traits_trim, old_d)

	all.equal(all_traits_trim, old_d)
	
	