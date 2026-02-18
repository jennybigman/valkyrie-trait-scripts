# QB exploration

# download QB, L_infinity, k, t0, length-weight regression parameters, and natural mortality (M)

  library(rfishbase)
	library(tidyverse)
	library(conflicted)
  library(here)
  
  #### species database ####
  sp_df <- fb_tbl("species") |>
    select(SpecCode, Genus, Species) |>
    mutate(sciname = paste0(Genus, "_", Species))
  

  #### growth and Linfinity ####
  growth_size_df <- fb_tbl("popgrowth") |>
	  select(StockCode, SpecCode, TLinfinity, K, to) |>
    drop_na(TLinfinity, K, to)
  
  growth_size_df <- left_join(growth_size_df, sp_df)

  	
  #### length weight regression parameters ####
  lwr <- fb_tbl("poplw") |>
    select(StockCode, SpecCode, a, b)
  
  lwr <- left_join(lwr, sp_df)
  
  #### QB #### - has everything we need but Winf instead of Linf
  
  qb_df <- fb_tbl("popqb") |>
    select(StockCode, SpecCode, PopQB, Winf, K, t0, Mortality) |>
    drop_na()
  
  qb_df <- left_join(qb_df, sp_df) # 65 unique species
  
  write_csv(qb_df, file = here("QB exploration", "FishBase_QB_table.csv"))
    
  # ration table
  ration_df <- fb_tbl("ration") |>
    select(StockCode, SpecCode, Rd1) |>
    drop_na(Rd1)
  
  ration_df <- left_join(ration_df, sp_df)
 
  
 

