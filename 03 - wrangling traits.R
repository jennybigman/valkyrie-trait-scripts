###### CODE SUMMARY #####
# Description: join data from FishLife and FishBase together, assign constants; 
# values preferentially selected from FishLife, FishBase, Ron's original database 
# (will need to change this to figure out how to make it applicable to any species)

# Output: .csv file of trait values 

#
# Programmer: Jennifer Bigman (NOAA/NMFS/OST/ Marine Ecosystems Division) 
# 
# Date: February 10, 2025
#
# Modified: 
############################################################################################

	library(FishLife)
	library(rfishbase)
	library(tidyverse)
  library(here)

	# read in dfs from FishLife and FishBase
	FL_traits_trim <- read_csv(file = "FishLife_traits.csv")
	FB_traits_trim <- read_csv(file = "FishBase_traits.csv")

	# join with FishLife
	all_traits <- left_join(FL_traits_trim, FB_traits_trim) 
	
	# add constants
	n <- nrow(all_traits)
	
	constant_df <- tibble(
		"FunctionalGroup" = 1,
		"TempShape" = 11,
		"rArea" = 1,
		"MinAgeJvnl" = 0,
		"PctPopJvnl" = 0.5,
		"PctPopAdlt" = 0.5, 
		"JvnlFishingMort" = 0.05,
		"JvnlNaturalDailyMort" = 0,
		"AssimilationEfficiency" = 0.95,
		"MaxIngestionRate" = 0.69,
		"PctTotalMassThatIsStructuralMass" = 0.76
	)

	constant_df <- replicate(n, constant_df, simplify = FALSE) |>
		bind_rows()
	
	all_traits <- bind_cols(all_traits, constant_df)

	# fix names
	all_traits$Species <- gsub(" ", "_", all_traits$Species)
	
	all_traits$Species[all_traits$Species == "Atlantic_cod"] <- "Atlantic_Cod"
	all_traits$Species[all_traits$Species == "Atlantic_mackerel"] <- "Atlantic_Mackerel"
	all_traits$Species[all_traits$Species == "Black_sea_bass"] <- "Black_Sea_Bass"
	all_traits$Species[all_traits$Species == "Yellowtail_flounder"] <- "Yellowtail_Flounder"

	# fix functional group
	all_traits$FunctionalGroup[all_traits$Species == "Atlantic_Mackerel"] <- 0
	
	# specify max age juvenile = min age adult
	all_traits$MaxAgeJvnl <- all_traits$MinAgeAdlt
	
	# rename cols
	all_traits <- all_traits |>
		rename(SpawnMinDepth = Waterdepthmin,
					 SpawnMaxDepth = Waterdepthmax,
					 DaysToHatch = EggDevTime,
					 DaysAsLarvae = Duration) |>
		select(-FecundityMin, 
					 -FecundityMax, -EggMortalityMin,
					 -EggMortalityMax, -Mortality, 
					 -GestationMin, -GestationMax)
	

	# fill in data/values from existing df
	
	# load Ron's dataset
	d_ron <- read_csv("species_data.csv") |>
		select(-contains("..."), -contains("speed")) |>
		filter(Species != "NA") |> 
		filter(Species != "notes")
	
	# vector of columns in Ron's df
	rd_names <- names(d_ron)
	
	# vector of columns in FishLife/FishBase df
	md_names <- names(all_traits)
	
	# columns that need to be added to FishLife/FishBase df
	col_to_add_md <- setdiff(rd_names, md_names)
	
	# columns to remove from FishLife/FishBase df
	col_to_drop_md <- setdiff(md_names, rd_names)
	
	# remove
	all_traits <- all_traits |>
		select(-all_of(col_to_drop_md))
	
	# add columns to match Ron's df but with NAs
	all_traits[col_to_add_md] <- NA
	
	# preferentially use trait values from FishLife/FishBase df and then if NA, from Ron's df
	
	all_traits <- all_traits %>%
    mutate(across(-Species, as.numeric))
	
	d_ron <- d_ron |>
		mutate(across(-Species, as.numeric))
	
	all_traits <- dplyr::rows_patch(all_traits, d_ron)
	
	# assign avg size based on 1/3 of asymptotic length for juveniles and 2/3 for adults
	wt_asym <- FL_traits_trim |>
		select(Species, WeightAsymptotic)
	
	# fix names
	wt_asym$Species <- gsub(" ", "_", wt_asym$Species)
	
	wt_asym$Species[wt_asym$Species == "Atlantic_cod"] <- "Atlantic_Cod"
	wt_asym$Species[wt_asym$Species == "Atlantic_mackerel"] <- "Atlantic_Mackerel"
	wt_asym$Species[wt_asym$Species == "Black_sea_bass"] <- "Black_Sea_Bass"
	wt_asym$Species[wt_asym$Species == "Yellowtail_flounder"] <- "Yellowtail_Flounder"

	
	all_traits <- all_traits %>%
		left_join(., wt_asym) |>
		mutate(AveMass_g = ((2/3) * WeightAsymptotic),
					 ad_avg_length = ((2/3) * LengthAsymptotic),
					 jv_avg_length = ((1/3) * LengthAsymptotic))
				
	# add salinity
	all_traits$MinOptimalSal = 30
	all_traits$MaxOptimalSal = 999
	
	write_csv(all_traits, "all_traits.csv")
