# number of batches calculations 

	# goal is to calculate the average number of batches for a species that is representative of
	# each functional group in Valkyrie; initial list of species with data on spawning frequency
	# came from Table 1 in Marshall, D. J., Barneche, D. R., & White, C. R. (2022). 
	# How does spawning frequency scale with body size in marine fishes?. 
	# Fish and Fisheries, 23(2), 316-323. We picked representatives from Table 1 for each functional
	# group and went back to the cited paper for that species to get the number of batches. We then
	# took the average as a placeholder for number of batches for that group. 

	library(here)
	library(tidyverse)	

	# 1. Medium demersal (demersal piscivore) - representative: Atlantic cod 
	
	# citation: 
	# Kjesbu, O. S., Solemdal, P., Bratland, P., & Fonn, M. 
	# (1996). 
	# Variation in annual egg production in individual captive Atlantic cod (Gadus morhua). 
	# Canadian Journal of Fisheries and Aquatic Sciences, 53(3), 610-620.
	
	n_batch_dp <- c(16, 17, 18, 20, 21, 14, 4, 12, 21, 13, 19, 15, 11, 20, 16, 15, 18, 14, 19, 10, 15, 21)
	# data above are from Table 1 and do not include the observations that were documented
	# as 'an incomplete data set'
	
	avg_n_batch_dp <- sum(n_batch_dp)/length(n_batch_dp)

	# 2. Small demersal - representative: Pacific sanddab 
	
	# citation:
	# Lefebvre, L. S., Payne, A. M., & Field, J. C. 
	# (2016). 
	# Reproductive dynamics of Pacific sanddab, Citharichthys sordidus, 
	# off the central coast of California. 
	# Journal of Sea Research, 107, 100-111.
	
	n_batch_sd <- c(32, 30, 32, 29, 30)
	
	avg_n_batch_sd <- sum(n_batch_sd)/length(n_batch_sd)

	# 3. Small pelagic - representative: Californian anchovy
	
	# citation: Parrish, R. H., Mallicoate, D. L., & Klingbeil, R. A. 
	# (1986). 
	# Age-dependent fecundity, number of spawnings per year, sex-ratio, and maturation stages
	# in northern anchovy, Engraulis mordax. 
	# Fishery Bulletin, 84, 503–517.
	
	# Table 3 reports average number of spawnings per season across size (and spawning seasons)
	
	
	# 4. Medium pelagic - representative: Queen croaker (Seriphus politus)
	
	# these data came from Dustin Marshall himself 
	# (plus added additional observation apparently missed from a table)
	# will have to verify it!!
	
	n_batch_mp <- c(14, 13.6, 11, 14.7, 17.9, 13.9)
	
	avg_n_batch_mp <- sum(n_batch_mp)/length(n_batch_mp)

	
	################################################
	
	
	# match species based on total length
	
	species_M2021 <- c("Hemiramphus brasiliensis",
										 "Cynoscion nebulosus",
										 "Limanda aspera",
										 "Gadus morhua",
										 "Lachnolaimus maximus",
										 "Seriphus politus",
										 "Lutjanus campechanus",
										 "Merluccius hubbsi",
										 "Micromesistius australis",
										 "Thunnus maccoyii",
										 "Sebastes melanostictus", # not in FishLife
										 "Engraulis ringens",
										 "Tautoga onitis",
										 "Citharichthys sordidus",
										 "Sebastes goodei",
										 "Sprattus sprattus",
										 "Sebastes aleutianus",
										 "Engraulis anchoita",
										 "Engraulis mordax",
										 "Thunnus albacares",
										 "Hemiramphus balao",
										 "Cyprinella leedsi",
										 "Sebastes borealis",
										 "Sardina pilchardus",
										 "Engraulis capensis",
										 "Sardinops sagax")
	
	# get species names and tip order from FishLife
	species = FishBase_and_Morphometrics$tree$tip.label 
	
	sp_list <- match(species_M2021, species)

	# get traits and wrangle cols
	traits <- FishBase_and_Morphometrics$beta_gv[sp_list,]
	species <- rownames(traits)
	
	traits <- bind_cols(species, traits) |> 
		rename(species = '...1') 
	
 
	# wrangle df
	traits <- traits |>
		dplyr::select(species, "log(length_infinity)", "log(weight_infinity)") |>
		rename(Species = species,
					 WeightAsymptotic = "log(weight_infinity)") |>
		mutate(across(where(is.numeric), ~ exp(.x))) |>
		drop_na()
	
	# find closest match of WeightAsymptotic
	valkyrie_sp <- read_csv(file = "FishLife_traits.csv") |>
		select(Scientific_name, WeightAsymptotic) 
	
	# find closest match
	
	t <- valkyrie_sp %>%
  	rowwise() %>%
  	mutate(closest = traits$Species[which.min(abs(traits$WeightAsymptotic - WeightAsymptotic))])
	
	
	