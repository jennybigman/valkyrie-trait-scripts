###### CODE SUMMARY #####
# Description: downloads trait values from FishBase using rfishbase for 6 species currently used in Valkyrie

# Output: .csv file of trait values
#
# Programmer: Jennifer Bigman (NOAA/NMFS/OST/ Marine Ecosystems Division) 
# 
# Date: February 7, 2025
#
# Modified: 
############################################################################################

	library(rfishbase)
	library(tidyverse)
	library(conflicted)
	library(FishLife)
  library(here)

	# set up
	#options("duckdbfs_use_nightly" = FALSE) # for rfishbase pkg compatibility
	
	conflicts_prefer(dplyr::select)
	conflicts_prefer(dplyr::filter)
	
	# read in FishLife traits for later merging
	FL_traits_trim <-	read_csv(file = "FishLife_traits.csv")

	
	#fb_tables() will tell you all of the tables in fishbase

	# length-weight regression function
	lwr_fun <- function(mass, a, b){
		
		l = (log10(mass) - log10(a))/b
		length = 10^l
		return(length)
	}
	
	############################
	
	# fishbase species codes
	common_names_list <- c("Atlantic Cod", "Atlantic Mackerel", 
											   "Black Sea Bass", "Haddock",
											   "Scup", "Yellowtail Flounder")

	sp_df <- common_to_sci(common_names_list)
	
	rows_keep <- c(1, 3, 4, 9, 16, 17)
	
	sp_df <- sp_df |> 
		slice(rows_keep) |>
		dplyr::select(-Language) |>
		rename(species_code = SpecCode)
	
	# edit taxonomy 
	sp_df$Species[sp_df$Species == "Myzopsetta ferruginea"] <- "Limanda ferruginea"

	spec_codes <- unique(sp_df$species_code)

	# spawning df
	spawn_df <- fb_tbl("spawning") |>
		filter(SpecCode %in% spec_codes) |>
		select(FecundityMin, FecundityMax, 
					 SpawningCycles, SpecCode,
					 GestationMin, GestationMax,
					 Dailyspawnmax, Dailyspawnmin)
	
	spawn_df <- spawn_df |>
		group_by(SpecCode) |>
  	summarise(across(where(is.numeric), mean, na.rm = TRUE)) |>
		mutate(species_code = SpecCode)

	# spawn temps from same df (separate b/c will join all of the spawning-like temp data together at end)
	spawn_df_temp <- 
		fb_tbl("spawning") |>
		filter(SpecCode %in% spec_codes) |>
		select(SpecCode, TempHigh, TempLow) |>
		rename(temp_max = TempHigh,
					 temp_min = TempLow) |>
		mutate(df = "spawning",
					 species_code = SpecCode)
	
	
	# eggs df
	eggs_df <- fb_tbl("eggs") |>
		filter(Speccode %in% spec_codes) |>
		select(Waterdepthmin, Waterdepthmax, 
					 Speccode) |>
		group_by(Speccode) |>
  	summarise(across(where(is.numeric), mean, na.rm = TRUE)) |>
		mutate(species_code = Speccode)
	
	# temp from same df
	eggs_df_temp <- fb_tbl("eggs") |>
		filter(Speccode %in% spec_codes) |>
		select(Speccode, Watertempmax, Watertempmin) |>
		rename(temp_max = Watertempmax,
					 temp_min = Watertempmin) |>
		mutate(df = "eggs",
					 species_code = Speccode)
	
	# salinity from same df
	eggs_df_sal <- fb_tbl("eggs") |>
		filter(Speccode %in% spec_codes) |>
		select(Speccode, Salinitymax, Salinitymin) |>
		rename(sal_max = Salinitymax,
					 sal_min = Salinitymin) |>
		mutate(df = "eggs",
					 species_code = Speccode)
	
	# eggdev df
	eggdev_df <- fb_tbl("eggdev") |>
		filter(SpecCode %in% spec_codes) |>
		select(SpecCode, Temperature,
					 EggDevTime) |>
		group_by(SpecCode) |>
  	summarise(across(where(is.numeric), mean, na.rm = TRUE)) |>
		mutate(species_code = SpecCode)
	
	# salinity from same df
	eggdev_df_sal <- fb_tbl("eggdev") |>
		filter(SpecCode %in% spec_codes) |>
		select(SpecCode, Salinity, Salinity2)	|>
		rename(sal_max = Salinity,
					 sal_min = Salinity2) |>
		mutate(df = "eggdev",
					 species_code = SpecCode)

	# eggnurserysystem df
	eggns_df <- fb_tbl("eggnurserysystem") |>
		filter(Speccode %in% spec_codes) |>
		select(EggMortalityMin, EggMortalityMax, 
					 Speccode) |>
  	group_by(Speccode) |>
  	summarise(across(where(is.numeric), mean, na.rm = TRUE)) |>
		mutate(species_code = Speccode)
	
	# temp from same df
	eggns_df_temp <- fb_tbl("eggnurserysystem") |>
		filter(Speccode %in% spec_codes) |>
		select(Speccode, TempMin, TempMax)|>
		rename(temp_max = TempMax,
					 temp_min = TempMin) |>
		mutate(df = "egg_ns",
					 species_code = Speccode)
	
	# salinity from same df
	eggns_df_sal <- fb_tbl("eggnurserysystem") |>
		filter(Speccode %in% spec_codes) |>
		select(Speccode, SalinMax, SalinMin) |>
		rename(sal_max = SalinMax,
					 sal_min = SalinMin) |>
		mutate(df = "egg_ns",
					 species_code = Speccode)
					 
	# larval dynamics df
	larv_df <- fb_tbl("larvdyn") |>
		filter(SpecCode %in% spec_codes) |>
		select(SpecCode, #Temperature, 
					 Duration, Mortality) |>
  	group_by(SpecCode) |>
  	summarise(across(where(is.numeric), mean, na.rm = TRUE)) |>
		mutate(species_code = SpecCode)


	# joindfs
	
	# all but temp
	df_list <- list(spawn_df, eggs_df, eggdev_df, eggns_df, larv_df)
	
	fishbase_dfs <- df_list |>
		reduce(full_join, by = "species_code") |>
		select(-SpecCode.x, -Speccode.x, 
					 -SpecCode.y, -SpecCode,
					 -Speccode.y)

	fishbase_dfs <- left_join(fishbase_dfs, sp_df)	
	
	fishbase_dfs <- fishbase_dfs |>
		rename(Scientific_name = Species,
					 Species = ComName)
	
	# temp dfs
	spawning_temp_df <- bind_rows(
		spawn_df_temp, eggs_df_temp, eggns_df_temp) |>
		select(-Speccode, -SpecCode) |>
		drop_na(temp_max) |>
		distinct() 
	
	spawning_temp_df_sum <- spawning_temp_df |>
		group_by(species_code) |>
  	summarise(temp_min = min(temp_min),
  						temp_max = max(temp_max))
	
	spawning_temp_df_sum <- left_join(
		spawning_temp_df_sum, sp_df,
		by = "species_code") |>
		select(-species_code) |>
		rename(SpawnMinTemp = temp_min,
					 SpawnMaxTemp = temp_max,
					 Scientific_name = Species,
					 Species = ComName)
	
	# salinity - not incl eggdev_df_sal because no sal data
	spawning_sal_df <- bind_rows(
		eggs_df_sal, eggns_df_sal) |>
		select(-Speccode) |>
		drop_na(sal_max) |>
		distinct() 
	
	spawning_sal_df_sum <- spawning_sal_df |>
		group_by(species_code) |>
  	summarise(sal_min = min(sal_min),
  						sal_max = max(sal_max))
	
	spawning_sal_df_sum <- left_join(
		spawning_sal_df_sum, sp_df,
		by = "species_code") |>
		select(-species_code) |>
		rename(SpawnMinSal = sal_min,
					 SpawnMaxSal = sal_max,
					 Scientific_name = Species,
					 Species = ComName)
	
	# bind cols
	temp_sal_df <- left_join(spawning_temp_df_sum, spawning_sal_df_sum)
	
	# join with fishbase
	fishbase_dfs <- left_join(fishbase_dfs, temp_sal_df)

	# add lwr relationships
	lwr <- fb_tbl("poplw") |>
		filter(SpecCode %in% spec_codes) |>
		select(a, b, SpecCode) |>
		group_by(SpecCode) |>
		summarise(a = mean(a),
							b = mean(b)) |>
		rename(species_code = SpecCode)
	
	fishbase_dfs <- left_join(fishbase_dfs, lwr)
	
	fishbase_dfs <- fishbase_dfs |>
		rename(LengthWeightRelationshipA = a,
					 LengthWeightRelationshipB = b)

	write_csv(fishbase_dfs, file = "FishBase_traits.csv")
