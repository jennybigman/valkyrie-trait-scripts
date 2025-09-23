# 09 daily ration

# this has to come after calculating ave_mass_g

	# daily ration
	ration_df <- fb_tbl("ration") |>
		filter(SpecCode %in% spec_codes) |>
	  select(SpecCode, Rd1, TBW) |> # total body weight
	  rename(species_code = SpecCode,
	         body_weight = TBW,
	         daily_ration = Rd1)
	
	ration_df <- left_join(ration_df, sp_df) 
	
	ration_df <- ration_df |>
	  select(-Species) |>
	  rename(Species = ComName)
	
	# change names
  ration_df$Species[ration_df$Species == "Atlantic cod"] <- "Atlantic_Cod"
	ration_df$Species[ration_df$Species == "Atlantic mackerel"] <- "Atlantic_Mackerel"
	ration_df$Species[ration_df$Species == "Black sea bass"] <- "Black_Sea_Bass"
	ration_df$Species[ration_df$Species == "Yellowtail flounder"] <- "Yellowtail_Flounder"

	# get weight at ave_size
	ave_size <- all_traits |>
	  select(Species, AveMass_g)
	
	tmp <- left_join(ration_df, ave_size) |>
	  mutate(stage = if_else(body_weight < AveMass_g,
                         "juvenile", "adult")) %>%       
  group_by(Species, stage) %>%
  summarise(mean_ration = mean(daily_ration, na.rm = TRUE),
            n = n())
	
	
