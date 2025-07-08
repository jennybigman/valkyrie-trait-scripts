
  library(tidyverse)
  library(rfishbase)
  
 # species list
  path <- "/Users/jenniferbigman/Library/CloudStorage/Dropbox/Students/Indivero/species_list.rds"
  sp_list <- readRDS(path)
  
  # fix some names
  sp_list <- gsub("sebastes ruberrimus", "Sebastes ruberrimus" , sp_list)
  sp_list <- gsub("Raja binoculata", "Beringraja binoculata" , sp_list)
  sp_list <- gsub("Raja rhina", "Beringraja rhina" , sp_list)
  sp_list <- gsub("sebastes aleutianus", "Sebastes aleutianus" , sp_list)

	sp_codes <- fb_tbl("species") |>
	  select(SpecCode, Genus, Species)
	
	sp_codes <- sp_codes |>
	  mutate(genus_species = paste0(Genus, " ", Species))
	
	sp_codes <- sp_codes |>
	  filter(genus_species %in% sp_list)

	# filter by FAO area
	fao_areas <- faoareas(sp_codes$genus_species) |>
	      select(AreaCode, StockCode, SpecCode)

	# location
	# area <- left_join(XXX, fao_areas) #### XXXX will be database of interest
	
	

    