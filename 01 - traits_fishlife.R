###### CODE SUMMARY #####
# Description: downloads trait values from FishLife for 6 species currently used in Valkyrie

# Output: .csv file of trait values from FishLife
#
# Programmer: Jennifer Bigman (NOAA/NMFS/OST/ Marine Ecosystems Division) 
# 
# Date: February 7, 2025
#
# Modified: 
############################################################################################

	#devtools::install_github("james-thorson/FishLife", dep=TRUE)

	library(FishLife)
	library(rfishbase)
	library(tidyverse)
  library(here)
	
#	options("duckdbfs_use_nightly" = FALSE) # for rfishbase pkg compatibility
	
	# get species names and tip order from FishLife
	species = FishBase_and_Morphometrics$tree$tip.label 
   
	# convert common names to scientific
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

	# look up tip order in FishLife
	val_sp <- sp_df$Species
	
	sp_list <- match(val_sp, species)

	# get traits and wrangle cols
	traits <- FishBase_and_Morphometrics$beta_gv[sp_list,]
	species <- rownames(traits)
	
	traits <- bind_cols(species, traits) |> 
		rename(species = '...1') 
	
	vars <- names(traits)
	
	patterns <- c("age", "fecundity", "temp", "length",
							 "mortality", "weight", "growth")
	
	vars_keep <- grep(paste0(patterns, collapse = "|"), vars, value = TRUE)[-11]
	
	vars_keep <- append("species", vars_keep)
	
	FL_traits <- traits[, vars_keep]
	
	FL_traits_trim <- FL_traits |>
		dplyr::select(species, "log(age_maturity)", "temperature", 
					 "log(age_max)", "log(fecundity)", "log(natural_mortality)",
					 "log(growth_coefficient)", "log(length_infinity)",
					 "log(weight_infinity)") |>
		rename(Species = species,
					 MaxAgeJvnl =   "log(age_maturity)",
					 MinAgeAdlt =   "log(age_maturity)",
					 MaxAgeAdlt =   "log(age_max)",
					 EggsPerSpawn = "log(fecundity)",
					 VonBertK =     "log(growth_coefficient)",
					 LengthAsymptotic = "log(length_infinity)",
					 AdltNaturalMort = "log(natural_mortality)",
					 WeightAsymptotic = "log(weight_infinity)") |>
		mutate(across(where(is.numeric), ~ exp(.x)))
	
	sp_df_trim <- sp_df |>
		dplyr::select(-species_code)

	FL_traits_trim <- left_join(FL_traits_trim, sp_df_trim) |>
		rename(Scientific_name = Species,
					 Species = ComName)
	

	write_csv(FL_traits_trim, file = here("./data/FishLife_traits.csv"))
	