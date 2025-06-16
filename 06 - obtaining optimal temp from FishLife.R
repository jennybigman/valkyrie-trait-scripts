# temperature predictions

	#devtools::install_github("james-thorson/FishLife", dep=TRUE)

	library(FishLife)
	library(rfishbase)
	library(tidyverse)
	library(here)
	
	# load trait data
	all_traits <- read_csv("all_traits.csv")

	
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
	mean_traits <- FishBase_and_Morphometrics$beta_gv[sp_list,]
	
	species <- rownames(mean_traits)

	mean_traits_long <- mean_traits |>
		as_tibble() |>
		mutate(species = species) |>
		pivot_longer(
		cols = where(is.numeric)) |>
		rename(mean = value)
	
	se_traits <- purrr::map_df(sp_list, \(spp) {
		
		se_val <- sqrt(diag(FishBase_and_Morphometrics$Cov_gvv[spp, , ]))
		
		se_val 
		
	})
	
	se_traits_long <- se_traits |>
		as_tibble() |>
		mutate(species = species) |>
		pivot_longer(
		cols = where(is.numeric)) |>
		rename(se = value)
	
	traits <- left_join(mean_traits_long, se_traits_long)
	
	opt_temp_FL <- traits |>
		filter(name == "temperature")

	temp_range <- opt_temp_FL |>
		mutate(MinTemp = mean - (2*se),
					 MaxTemp = mean + (2*se))
	
	temp_range <- temp_range |>
		mutate(common_name = case_when(
			species == "Gadus morhua" ~ "Atlantic_Cod",
			species == "Scomber scombrus" ~ "Atlantic_Mackerel",
			species == "Centropristis striata" ~ "Black_Sea_Bass",  
			species == "Melanogrammus aeglefinus" ~ "Haddock",
			species == "Stenotomus chrysops" ~ "Scup",
			species == "Limanda ferruginea" ~ "Yellowtail_Flounder"     
		))
	
	temp_range_trim <- temp_range |>
		
	
	# add depth to all traits 
	all_traits <- all_traits |>
		mutate(MinDepth = )
	
	
	
	
	
	
	####################### DISTRIBUTION ##########################
  
	# generate 1000 temperature values based on mean and se
	temp_dists <- opt_temp_FL |>
		select(-name) |>
  	pmap(function(species, mean, se) {
  		
    	p <- rnorm(n = 1000, mean = mean, sd = se)
    
    	tibble(
      	p = p,
      	species = species
    	)
  	}) |>
  	bind_rows()  
	
	# plot
	
	temp_dists <- temp_dists |>
		mutate(common_name = case_when(
			species == "Gadus morhua" ~ "Atlantic_Cod",
			species == "Scomber scombrus" ~ "Atlantic_Mackerel",
			species == "Centropristis striata" ~ "Black_Sea_Bass",  
			species == "Melanogrammus aeglefinus" ~ "Haddock",
			species == "Stenotomus chrysops" ~ "Scup",
			species == "Limanda ferruginea" ~ "Yellowtail_Flounder"     
		))
	
	ggplot(temp_dists) +
		geom_density(aes(p), fill = "grey") +
		facet_wrap(~ common_name)
	
	ggplot(temp_dists) +
		geom_density(aes(p, fill = species, color = species))
	
	# predicting max/min optimal temperatures - weight temps towards median? 
	
	# try predicting spawning temp? (narrower range of SD - 0.5 SD)
	
	# weighting temp values - 
	
	# add from fishprior? 
	
	#### one sd either side ####
	
	temp_dists_sum <- temp_dists |>
		group_by(species, common_name) |>
		summarise(mean_t = mean(p),
							sd_t = sd(p))