# combine fishlife and fishbase trait data

  # read in dfs from FishLife and FishBase
	fishlife_df <- read_csv(file = here("./data/FishLife_traits.csv"))
	fishbase_df <- read_csv(file = here("./data/FishBase_traits.csv"))

	# if missing traits from Fishbase, use Fishlife
	fishbase_df <- fishbase_df |>
	  rename(WeightAsymptotic = Winfinity,
	         LengthAsymptotic = TLinfinity,
	         VonBertK = K,
	         t_zero = to,
	         DaysToHatch = EggDevTime,
	         DaysAsLarvae = Duration,
		       SpawnMinDepth = Waterdepthmin,
					 SpawnMaxDepth = Waterdepthmax) |>
	  select(WeightAsymptotic,
	         LengthAsymptotic,
	         VonBertK,
	         SpawnMinTemp,
	         SpawnMaxTemp,
	         SpawnMinSal,
	         SpawnMaxSal,
	         t_zero,
	         DaysToHatch,
	         LengthWeightRelationshipA,
	         LengthWeightRelationshipB,
	         DaysAsLarvae,
	         Species,
	         SpawnMinDepth,
	         SpawnMaxDepth)
	
	fishlife_df <- fishlife_df |>
	  select(-temperature, 
	         -VonBertK,
	         -Scientific_name) # fishbase has this and we're using 3 parameter VBGF
	

	overlap <- intersect(names(fishbase_df), names(fishlife_df))
	
	# set your join keys here
  keys <- "Species"

  # identify overlap (excluding keys)
  overlap <- setdiff(intersect(names(fishbase_df), names(fishlife_df)), keys)

  # join, keeping Y's overlapping columns with .y suffix
  Z <- fishbase_df %>%
    left_join(fishlife_df, by = keys, suffix = c("", ".y"))
  
  # for each overlapping column, replace NAs in X with Y's value
  for (nm in overlap) {
    Z[[nm]] <- ifelse(is.na(Z[[nm]]), Z[[paste0(nm, ".y")]], Z[[nm]])
  }
  
  # drop the temporary .y columns, keep X's original order
  Z <- Z %>% select(all_of(names(fishbase_df)))
  
  # remove the overlap cols from fishlife and add the rest back to main dataset
  fishlife_df <- fishlife_df |>
    select(-all_of(overlap))
  
  # add the columns 
  all_traits <- left_join(Z, fishlife_df)
  
  # change names
  all_traits$Species[all_traits$Species == "Atlantic cod"] <- "Atlantic_Cod"
	all_traits$Species[all_traits$Species == "Atlantic mackerel"] <- "Atlantic_Mackerel"
	all_traits$Species[all_traits$Species == "Black sea bass"] <- "Black_Sea_Bass"
	all_traits$Species[all_traits$Species == "Yellowtail flounder"] <- "Yellowtail_Flounder"

	write_csv(all_traits, file = here("./data/all_traits_Sep2025.csv"))
