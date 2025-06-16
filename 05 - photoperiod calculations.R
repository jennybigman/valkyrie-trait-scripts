###### CODE SUMMARY #####
# Description: calculate photoperiod for each species to trigger spawning, based on the mid-latitude of
# the distribution 

# Output: 
#
# Programmer: Jennifer Bigman (NOAA/NMFS/OST/ Marine Ecosystems Division) 
# 
# Date: February 10, 2025
#
# Modified: 
############################################################################################	

	library(lubridate)
	library(meteor)
	library(FishLife)
	library(rfishbase)
	library(tidyverse)
  library(here)

	# load trait data
	all_traits <- read_csv("all_traits.csv")
	
	# unique species 
	common_names_list <- c("Atlantic Cod", "Atlantic Mackerel", 
											   "Black Sea Bass", "Haddock",
											   "Scup", "Yellowtail Flounder")
	
	sp_df <- common_to_sci(common_names_list) 
	
	rows_keep <- c(1, 3, 4, 9, 16, 17)
	
	sp_df <- sp_df |> 
		slice(rows_keep) |>
		select(-Language) |>
		rename(species_code = SpecCode)
	
	spec_codes <- sp_df$species_code

	
	# spawning
	spawn_season_df <- fb_tbl("spawning") |>
		filter(SpecCode %in% spec_codes) |>
		select(SpecCode, C_Code, StockCode, Spawningarea,
					 Jan, Feb, Mar, Apr, May, Jun, Jul,
					 Aug, Sep, Oct, Nov, Dec) |>
		rename(species_code = SpecCode,
					 stock_code = StockCode)
	
	# retain observations in US, Country Code = 840 -- use this instead of selecting places 
	spawn_season_df <- spawn_season_df |>
		filter(C_Code == 840)
	
	spawn_season_df <- left_join(spawn_season_df, sp_df) |>
		rename(Species = ComName,
					 sci_name = Species)
	
	spawn_season_df <- spawn_season_df |>
		group_by(Species) |>
		summarise(Jan = mean(Jan, na.rm = TRUE),
							Feb = mean(Feb, na.rm = TRUE),
							Mar = mean(Mar, na.rm = TRUE),
							Apr = mean(Apr, na.rm = TRUE),
							May = mean(May, na.rm = TRUE),
							Jun = mean(Jun, na.rm = TRUE),
							Jul = mean(Jul, na.rm = TRUE),
							Aug = mean(Aug, na.rm = TRUE),
							Sep = mean(Sep, na.rm = TRUE),
							Oct = mean(Oct, na.rm = TRUE),
							Nov = mean(Nov, na.rm = TRUE),
							Dec = mean(Dec, na.rm = TRUE))
	
	
	# replace NaN with NA to make it easier to work with
	spawn_season_df <- spawn_season_df %>%
  	mutate(across(-Species, ~ replace(., is.nan(.), NA)))


	find_spawning_period <- function(row) {
		
		sp <- row[1]
  	
		months <- names(row)
  	
  	# return months where spawning occurs (i.e., the value is 111)
  	spawning_months <- months[!is.na(row)][-1]  # These are the months where spawning happens
  	
  	## need to rearrange months to spawning season ##

		# define the full list of months for comparison
		all_months <- c("Jan", "Feb", "Mar", 
										"Apr", "May", "Jun", 
										"Jul", "Aug", "Sep", 
										"Oct", "Nov", "Dec")

		# find the position of the months in the `all_months` vector
		mo_positions <- match(spawning_months, all_months)

		# check where the break occurs (i.e., where the months are not consecutive)
	  break_index <- which(diff(mo_positions) > 1)

		# if there is a break, rearrange the vector by moving months after the break to the front
		if (length(break_index) > 0) {
		  
			# split the months at the first break and rearrange
		  mo_rearranged <- c(spawning_months[(break_index[1] + 1):length(spawning_months)], spawning_months[1:break_index[1]])
		
		 } else {
		 	
		  # rearrange 
		  mo_rearranged <- spawning_months
		  
		 }
	  
		  mo_rearranged_positions <- match(mo_rearranged, all_months)

		# get the first and last spawning months month
		start_month <- mo_rearranged_positions[1]
    end_month <- mo_rearranged_positions[length(mo_rearranged_positions)]
    
    df <- tibble(sp, start_month, end_month)
    
    df
    
	}
	
	rows <- spawn_season_df |>
		group_nest(row_number())
	
	rows <- rows$data
	
	spn_df <- purrr::map_dfr(rows, find_spawning_period)

	spn_df <- spn_df |>
		mutate(Species = case_when(
			Species == "Atlantic cod" ~ "Atlantic_Cod",
			Species == "Atlantic mackerel" ~ "Atlantic_Mackerel",
			Species == "Black sea bass" ~ "Black_Sea_Bass",  
			Species == "Haddock" ~ "Haddock",
			Species == "Scup" ~ "Scup",
			Species == "Yellowtail flounder" ~ "Yellowtail_Flounder"     
		))
	
	#### find day of year for the middle of each month ####
	
	get_day_of_year <- function(month) {
  	
		date <- mdy(paste(month, 15, 2025))  # 15th of each month, year doesn't matter but needed
    day_of_year <- yday(date) 
  	day_of_year
}

	spn_df$start_doy <- sapply(spn_df$start_month, get_day_of_year)
	spn_df$end_doy <- sapply(spn_df$end_month, get_day_of_year)
	
	# find photoperiod on those days
	
	# mid-latitude of East Coast is 39.73 (~ Barnegat Bay),
	# midpoint between New Bew Brunswick, ME and Morehead City, NC

	get_photoperiod <- function(doy, lat = 39.73){
		
		p <- round(photoperiod(doy, lat), 2)
		p
	}
	
	spn_df$start_pp <- sapply(spn_df$start_doy, get_photoperiod)
	spn_df$end_pp <- sapply(spn_df$end_doy, get_photoperiod)
	
	spn_df$mid_pt <- (spn_df$start_pp + spn_df$end_pp)/2
	
	write_csv(spn_df, file = "photoperiod_df.csv")
	
	# add to all_traits
	
	spn_mo <- spn_df |>
		select(Species, start_month, end_month)

	all_traits <- left_join(all_traits, spn_mo) |>
		rename(SpawnStartMonth = start_month,
					 SpawnEndMonth = end_month)
	
	write.csv(all_traits, file = "all_traits.csv")

