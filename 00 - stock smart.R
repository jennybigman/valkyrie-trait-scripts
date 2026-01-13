# stock smart R package

  # load package
  # pak::pak("NOAA-EDAB/stocksmart")
  library(stocksmart)
  library(tidyverse)

  # get species codes
  species <- c("Atlantic cod", 
               "Atlantic mackerel", 
               "Black sea bass", 
               "Haddock", 
               "Scup", 
               "Yellowtail flounder")

  
  codes <- purrr::map_dfr(species, \(sp) {
		
		get_species_itis(stock = sp)
    
	})
  
  codes <- codes |>
    distinct(ITIS, .keep_all = TRUE)
  
  codes <- codes$ITIS
  
  code <- codes[1]
  
  d <- get_latest_full_assessment(itis = code)
  
  d <- d$data
  
  # abundance data
  abun <- purrr::map_dfr(codes, \(code) {
    
    df <- get_latest_metrics(itis = code, metrics = "Catch")
    df <- df$data
    df 
    
  })
  
  abun_2013 <- abun |>
    filter(Year == 2013)
  
  
    