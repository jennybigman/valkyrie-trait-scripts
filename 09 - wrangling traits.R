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


	# add constants
	n <- nrow(all_traits)
	
	constant_df <- tibble(
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


	# functional group 
	fun_grp <- tibble(
	  Species = unique(all_traits$Species),
	  FunctionalGroup = c("Medium_Demersal", "Small_Pelagic", "Medium_Demersal",
	                      "Medium_Demersal", "Medium_Demersal", "Medium_Demersal"),
	  FeedingGuild = c("Piscivore", "Planktivore", "Piscivore",
	                   "Piscivore", "Piscivore", "Piscivore"))
	
	
	
	all_traits <- left_join(all_traits, fun_grp)
	
	# specify max age juvenile = min age adult
	all_traits$MaxAgeJvnl <- all_traits$MinAgeAdlt
	

	# fill in data/values from other columns 
	
	# which columns have missing values in the main dataframe?
	which(is.na(all_traits), arr.ind = TRUE) |>
  as.data.frame() |>
  transform(column = colnames(all_traits)[col])
	
	# if no spawning depth/temp/sal fill in with optimal
	all_traits <- all_traits |>
	  mutate(SpawnMinTemp = coalesce(SpawnMinTemp, MinOptimalTemp),
	         SpawnMaxTemp = coalesce(SpawnMaxTemp, MaxOptimalTemp),
	         SpawnMinSal = coalesce(SpawnMinSal, MinOptimalSal),
	         SpawnMaxSal = coalesce(SpawnMaxSal, MaxOptimalSal),
	         SpawnMinDepth = coalesce(SpawnMinDepth, MinDepth),
	         SpawnMaxDepth = coalesce(SpawnMaxDepth, MaxDepth))
	  
	# need to add intraspawn time, adult and juv fishing mortality, egg survival pct
	all_traits$AdltFishingMort <- c(0.39, 0.4, 0.39, 0.4, 0.41, 0.4)
	all_traits$JvnlNaturalMort <- c(0.8, 0.5, 0.8, 0.5, 0.8, 0.5)
	all_traits$EggSurvival_pct <- c(6.05, 6.05, 2, 25, 2, 50)
	all_traits$IntraSpawnTime <- c(3, 3, 7, 10, 365, 10)
	
	# still missing DaysAsLarvae for BSB, Scup, Yellowtail and DaysToHatch for Yellowtail
	
	## days as larvae
	# bsb
	# Drohan, Amy F.;Manderson, John P.;Packer, David B. 2007. Essential fish habitat source document. Black sea bass, Centropristis striata, life history and habitat characteristics. Northeast Fisheries Science Center (U.S.); NOAA technical memorandum NMFS-NE ; URL : https://repository.library.noaa.gov/view/noaa/4038
	all_traits$DaysAsLarvae[all_traits$Species == "Black_Sea_Bass"] <- 22.5

	# scup 
	all_traits$DaysAsLarvae[all_traits$Species == "Scup"] <- 60
	#Steimle FW, Zetlin CA, Berrien PL, Johnson DL, Chang S. 1999. Essential fish habitat source document:
  #Scup, Stenotomus chrysops, life history and habitat characteristics. NOAA Tech Memo NMFS NE 149; 39
  #p. Accessed online (February 2015): http://www.nefsc.noaa.gov/nefsc/publications/tm/tm149/
  
	# yellowtail flounder
	all_traits$DaysAsLarvae[all_traits$Species == "Yellowtail_Flounder"] <- 60
	# Johnson, Donna (Donna L.), 1999. Essential fish habitat source document. Yellowtail flounder, Limanda ferruginea, life history and habitat characteristics; Northeast Fisheries Science Center (U.S.); NOAA technical memorandum NMFS-NE ; 140; URL : https://repository.library.noaa.gov/view/noaa/3137
  # calculated as first egg to first juvenile time frame
	
	# days to hatch yellowtail flounder
	all_traits$DaysToHatch[all_traits$Species == "Yellowtail_Flounder"] <- 5
  # Johnson, Donna (Donna L.), 1999. Essential fish habitat source document. Yellowtail flounder, Limanda ferruginea, life history and habitat characteristics; Northeast Fisheries Science Center (U.S.); NOAA technical memorandum NMFS-NE ; 140; URL : https://repository.library.noaa.gov/view/noaa/3137

	# specify functional groups
	all_traits$FunctionalGroup[all_traits$FunctionalGroup == 1] <- "Medium_Demersals"
	all_traits$FunctionalGroup[all_traits$FunctionalGroup == 0] <- "Small_Pelagics"
	
	# add NumAgents and IndsPerAgent
	all_traits$NumAgents <- 200/6
	all_traits$IndsPerAgent <- 1000

	# max age juvenile remove
	all_traits <- all_traits |>
	  select(-MaxAgeJvnl)

	write_csv(all_traits, here("./data/all_traits.csv"))

