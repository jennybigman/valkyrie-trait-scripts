
  t <- faoareas("Gadus morhua")
  unique(t$FAO)
  
  t2 <- faoareas("Gadus morhua") |> slice (4)
  
  
growth <- popgrowth(species_list = "Gadus morhua")

# fill in missing codes
g <- dplyr::group_by(growth, StockCode) |>
  dplyr::summarise(E_CODE = unique(E_CODE)[1])
growth <- dplyr::left_join(dplyr::select(growth, -E_CODE), g, by = "StockCode")
  
# get ecosystem info
ecosystem_ref <- fb_tbl("ecosystemref") |>
  dplyr::filter(E_CODE %in% growth$E_CODE)

#joined <- dplyr::left_join(growth, ecosystem_ref)


  s <- fb_tbl("stocks")
  
  
  # match to FAO area
  growth <- popgrowth(species_list = "Gadus morhua")

  fao_d <- faoareas("Gadus morhua") |>
    select(AreaCode, StockCode, SpecCode)
  
  growth <- left_join(growth, fao_d) |>
    filter(AreaCode == 21) |>
    summarise(mean_k = mean(K))

    