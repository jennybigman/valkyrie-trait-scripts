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

	# load trait data
	all_traits <- read_csv(here("./data/all_traits.csv"))
	
	# add constants
	n <- nrow(all_traits)
	
	constant_df <- tibble(
		"FunctionalGroup" = 1,
		"TempShape" = 11,
		"PctPopJvnl" = 0.5,
		"PctPopAdlt" = 0.5, 
		"JvnlFishingMort" = 0.05,
		"LarvalMort" = 0.95,
		"AssimilationEfficiency" = 0.95,
		"MaxIngestionRate" = 0.69,
		"PctTotalMassThatIsStructuralMass" = 0.76
	)

	constant_df <- replicate(n, constant_df, simplify = FALSE) |>
		bind_rows()
	
	all_traits <- bind_cols(all_traits, constant_df)


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
	
	# use Fishbase growth for those species that have it
	
	all_traits <- all_traits %>%
    mutate(TLinfinity = coalesce(TLinfinity, LengthAsymptotic),
           K = coalesce(K, VonBertK)) |>
	  select(-LengthAsymptotic, -VonBertK, -Winfinity,
	         -temperature, ) |>
	  rename(VonBertK = K,
	         LengthAsymptotic = TLinfinity,
	         t_zero = to)
	

	# fill in data/values from existing df
	
	# load Ron's dataset
	d_ron <- read_csv(here("./data/species_data.csv")) |>
		select(-contains("..."), -contains("speed")) |>
		filter(Species != "NA") |> 
		filter(Species != "notes") |>
	
	# vector of columns in Ron's df
	rd_names <- names(d_ron)
	
	# vector of columns in FishLife/FishBase df
	md_names <- names(all_traits)
	
	# columns that need to be added to FishLife/FishBase df
	col_to_add_md <- setdiff(rd_names, md_names)
	
	# columns to remove from FishLife/FishBase df
	col_to_drop_md <- setdiff(md_names, rd_names)
	
	col_to_drop_md <- col_to_drop_md[-3] # keep WeightAsymptotic
	col_to_drop_md <- col_to_drop_md[-9] # keep t_zero

	# remove
	
	# add columns to match Ron's df but with NAs
	all_traits[col_to_add_md] <- NA
	
	# preferentially use trait values from FishLife/FishBase df and then if NA, from Ron's df
	
	all_traits <- all_traits %>%
    mutate(across(-Species, as.numeric))
	
	d_ron <- d_ron |>
		mutate(across(-Species, as.numeric))
	
	all_traits <- dplyr::rows_patch(all_traits, d_ron)
	
	# remove uneeded rows
	all_traits <- all_traits |>
	  select(-TotalBiomass_mt, rArea, MinAgeJvnl)
	
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
	#	left_join(., wt_asym) |>
		mutate(AveMass_g = ((2/3) * WeightAsymptotic),
					 ad_avg_length = ((2/3) * LengthAsymptotic),
					 jv_avg_length = ((1/3) * LengthAsymptotic))
				
	# add salinity
	all_traits$MinOptimalSal = 30
	all_traits$MaxOptimalSal = 999
	
	# change intraspawn time for Scup
	all_traits$IntraSpawnTime[all_traits$Species == "Scup"] <- 365
	

	
	write_csv(all_traits, here("./data/all_traits.csv"))

	          