# 08 - QB predictions

# from Palomares and Pauly 

# set up daily ration predictions -  need ln(Winf), ln(caudal fin aspect ratio), feeding type, and ln(temp (habitat))
	
	
  # get caudal fin aspect ratio
	common_names_list <- c("Atlantic Cod", "Atlantic Mackerel", 
											   "Black Sea Bass", "Haddock",
											   "Scup", "Yellowtail Flounder")

	sp_df <- common_to_sci(common_names_list)
	
	rows_keep <- c(1, 3, 4, 9, 16, 17)
	
	sp_df <- sp_df |> 
		slice(rows_keep) |>
		dplyr::select(-Language) |>
		rename(species_code = SpecCode)
	
	sp <- sp_df$Species

  mm <- morphometrics(sp)  # pull morphometrics table rows for your species
  
  grep("aspect", names(mm), ignore.case = TRUE, value = TRUE)

  mm_ar <- mm |>
    select(Species, any_of(c("AspectRatio", "Aspect.ratio", "aspect_ratio"))) |>
    rename(caudal_AR = any_of(c("AspectRatio", "Aspect.ratio", "aspect_ratio"))) |>
    filter(!is.na(caudal_AR)) |>
    group_by(Species) |>
    summarise(AR = mean(caudal_AR, na.rm = TRUE), .groups = "drop") 
    
  
  A <- left_join(mm_ar, sp_df) |>
    mutate(
      Species = stringr::str_squish(ComName),
      Species = stringr::str_to_title(ComName),  
      Species = stringr::str_replace_all(ComName, " ", "_")) |>
    select(Species, AR)
  
  
  A$Species[A$Species == "Atlantic_cod"] <- "Atlantic_Cod"
  A$Species[A$Species == "Yellowtail_flounder"] <- "Yellowtail_Flounder"
  A$Species[A$Species == "Atlantic_mackerel"] <- "Atlantic_Mackerel"
  A$Species[A$Species == "Black_sea_bass"] <- "Black_Sea_Bass"
  
  all_traits <- left_join(all_traits, A)
  
  
  all_traits <- all_traits |>
    mutate(ln_Winf = log(WeightAsymptotic),
           ln_A = log(AR),
           FT = 0)
  
	
##### 
  
 # 	QB_fun <- function(ln_Winf, ln_T, ln_A, FT){
#
 #   ln_QB = -0.1775 - 0.2018 * ln_Winf + 0.6121 * ln_T + 0.5156 * ln_A + 1.26 * (FT)
 #   QB = exp(ln_QB)
 #   QB
#	}
	