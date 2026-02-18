# stock smart R package

  # load package
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
  
  d <- get_latest_metrics(itis = code)
  
  d <- d$data
  
  # abundance data
  abun <- purrr::map_dfr(codes, \(code) {
    
    df <- get_latest_metrics(itis = code, metrics = "Abundance")
    df <- df$data
    df 
    
  })
  
  abun <- abun |>
    dplyr::filter(Description == "Spawning Stock Biomass")
  
  # sum across stocks
  abun_sum <- abun |>
    group_by(CommonName, Year) |>
    summarise(value = sum(Value))
  
  ggplot(abun_sum, aes(x = as.factor(Year), y = value)) +
    geom_point() +
   # geom_point(aes(color = Description)) +
    facet_wrap(~CommonName, scales = "free")
  
  # predict abundance based on the idea that abundance = SSB/mean weight
  
  all_traits <- read_csv(file = here("./data/ave_mass.csv"))
  
  df <- all_traits |>
    select(Species, AveMass_g)
  
 abun_sum <- abun_sum %>%
  mutate(Species = str_to_title(CommonName),
         Species = str_replace_all(Species, " ", "_"))
 
 abun_sum <- left_join(abun_sum, df)

 abun_sum <- abun_sum |>
   mutate(SSB_g = value * 1e6,
          EstAbun = SSB_g/AveMass_g)
 
 abun_sum <- abun_sum |>
   ungroup() |>
   select(Species, Year, value, AveMass_g, SSB_g, EstAbun) |>
   rename(SSS_mt = value)
 
 write_csv(abun_sum, file = here("./data/estimated_abundance.csv"))
  
  
  
  
    