
	library(FishLife)
	library(rfishbase)
	library(tidyverse)
  library(here)

	# load trait data
	all_traits <- read_csv(here("./data/all_traits.csv"))

	# fishbase species codes
	common_names_list <- c("Atlantic Cod", "Atlantic Mackerel", 
											   "Black Sea Bass", "Haddock",
											   "Scup", "Yellowtail Flounder")

	sp_df <- common_to_sci(common_names_list)
	
	rows_keep <- c(1, 3, 4, 9, 16, 17)
	
	sp_df <- sp_df |> 
		slice(rows_keep) |>
		select(-Language) |>
		rename(species_code = SpecCode)
	
	# edit taxonomy 
	sp_df$Species[sp_df$Species == "Myzopsetta ferruginea"] <- "Limanda ferruginea"

	spec_codes <- unique(sp_df$species_code)

	# download depth data from Fishbase 
	depth <- fb_tbl("species") |>
		filter(SpecCode %in% spec_codes) |>
		select(Genus, Species, FBname, contains("Depth")) |>
		rename(MaxDepth = DepthRangeComDeep,
					 MinDepth = DepthRangeComShallow) |>
		mutate(common_name = case_when(
			FBname == "Atlantic cod" ~ "Atlantic_Cod",
			FBname == "Atlantic mackerel" ~ "Atlantic_Mackerel",
			FBname == "Black seabass" ~ "Black_Sea_Bass",  
			FBname == "Haddock" ~ "Haddock",
			FBname == "Scup" ~ "Scup",
			FBname == "Yellowtail flounder" ~ "Yellowtail_Flounder"     
		)) |>
		select(common_name, MaxDepth, MinDepth) |>
		rename(Species = common_name)
	
	# add to data frame
	all_traits <- left_join(all_traits, depth)
	
	# Black Sea bass and Scup do not have common depth, use spawning depth?
	depth_fill <- all_traits |>
		select(Species, MaxDepth, MinDepth, SpawnMaxDepth, SpawnMinDepth) |>
		mutate(MaxDepth = case_when(
			is.na(MaxDepth) ~ SpawnMaxDepth)) |>
		mutate(MinDepth = case_when(
			is.na(MinDepth) ~ SpawnMinDepth
		))
	
	all_traits <- all_traits |>
		mutate(MaxDepth = ifelse(is.na(MaxDepth), depth_fill$MaxDepth, MaxDepth),
					 MinDepth = ifelse(is.na(MinDepth), depth_fill$MinDepth, MinDepth))
	

	
	write_csv(all_traits, "./data/all_traits.csv")


	