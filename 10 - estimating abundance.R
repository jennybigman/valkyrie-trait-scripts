  biomass_dat <- purrr::map_dfr(codes, \(code) {
    
    df <- get_latest_full_assessment(itis = code) # think about not using
    df <- df$data
    df 
    
  })
  

  biomass_dat <- biomass_dat |>
    filter(str_detect(Description, 
    regex("biomass|recruit|abundance", 
    ignore_case = TRUE)))
 
  # find specific values 
  biomass_dat |>
    group_by(CommonName) |>
    distinct(Description)
  
  keep_patterns <- tibble::tribble(
    ~CommonName,            ~pattern,
    "Atlantic cod",         "Abundance - Age 1|Mature Biomass \\(Mean\\)",
    "Atlantic mackerel",    "Spawning Stock Biomass|Abundance",
    "Black sea bass",       "Age 1|Biomass",
    "Haddock",              "Spawning Stock Biomass|Recruits",
    "Scup",                 "Spawning Stock Biomass|Recruits",
    "Yellowtail flounder",  "Spawning Stock Biomass|Recruits"
  )

 biomass_dat <- biomass_dat |>
  inner_join(keep_patterns, by = "CommonName") |>
  filter(str_detect(Description, regex(pattern, ignore_case = TRUE))) |>
  select(-pattern)
 
 # check units of recruitment
 tmp <- biomass_dat |>
   distinct(Description, .keep_all = TRUE)

  # sum across stocks
  biomass_dat_sum <- biomass_dat |>
    group_by(CommonName, Year, Description) |>
    summarise(value = sum(Value))
 
 all_traits <- read_csv(file = here("./data/ave_mass.csv"))
  
  df <- all_traits |>
    select(Species, AveMass_g)
  
 biomass_dat_sum <- biomass_dat_sum %>%
  mutate(Species = str_to_title(CommonName),
         Species = str_replace_all(Species, " ", "_"))
 
 biomass_dat_sum <- left_join(biomass_dat_sum, df)
 
 spp <- unique(biomass_dat_sum$Species)
 
 sp_dfs <- purrr::map_dfr(spp, \(sp){
   
   nd <- biomass_dat_sum |>
     filter(Species == sp)
   
   nd <- nd |>
   select(CommonName, Species, Year, Description, value) |>
    pivot_wider(
      names_from = Description,
      values_from = value
    )
   
   tmp <- df |>
     filter(Species == sp)
   
   nd <- left_join(nd, tmp)
 
   nd <- nd |>
    mutate(
      SSB_g = pick(contains("Biomass"))[[1]] * 1e6,
      EstAbun = SSB_g / AveMass_g)
   
   nd <- drop_na(nd)
   
   nd <- nd |>
    select(
      CommonName, Species, Year,
      matches("Abundance|Number|Recruits"),
      SSB_g, EstAbun)
   
   nd

 })
   
 sp_dfs <- sp_dfs |>
  mutate(
    recruitment = 
      coalesce(`Abundance - Age 1`, 
               `Number of Recruits - Age 0`,
               `Recruits - Age 1`)) 
 
 sp_dfs <- sp_dfs |>
   mutate(recruitment = recruitment * 1000) |>
   select(CommonName, Species, Year, EstAbun, recruitment)
 
 abun_dat <- sp_dfs |>
   mutate(total_abun = EstAbun + recruitment) |>
   ungroup() |>
   select(Species, Year, total_abun)
   
  write_csv(file = here("abun_dat.csv"), abun_dat)
