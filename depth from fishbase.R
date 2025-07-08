
	# download depth data from Fishbase 
	depth1 <- fb_tbl("species") |>
		filter(SpecCode %in% spec_codes) |>
		select(SpecCode, Genus, Species, FBname, contains("Depth")) |>
		rename(MaxDepth = DepthRangeDeep,
					 MinDepth = DepthRangeShallow) |>
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

		d <- fb_tbl("ecology") |>
		  filter(SpecCode %in% spec_codes)
		
		
		library(robis)
data <- occurrence("Gadus morhua")
depth_data <- data %>% select(depth)