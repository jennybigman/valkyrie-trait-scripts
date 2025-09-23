# adding aquamaps data - from fishbase though, so not sure how helpful this is...

  # Atlantic cod
  a_cod <- data.frame(
    Species = "Atlantic_Cod",
    MinDepth = 0,
    MaxDepth = 600,
    #MinComDepth = 20,
    #MaxComDepth = 200,
    MinOptimalTemp = -0.3,
    MaxOptimalTemp = 16.1,
    #MinComTemp = 4.15,
    #MaxComTemp = 12.15,
    MinOptimalSal = 5.5, 
    MaxOptimalSal = 39.76
    #MinComSal = 7,
    #MaxComSal = 34.8
  )
  
  # Atlantic mackerel
   a_mac <- data.frame(
    Species = "Atlantic_Mackerel",
    MinDepth = 0,
    MaxDepth = 1000,
    #MinComDepth = 0,
    #MaxComDepth = 200,
    MinOptimalTemp = 5,
    MaxOptimalTemp = 23.37,
    #MinComTemp = 8.71,
    #MaxComTemp = 20.5,
    MinOptimalSal = 6.78, 
    MaxOptimalSal = 40
    #MinComSal = 7.5,
    #MaxComSal = 38
  )
   
  # Black sea bass
   B_sb <- data.frame(
    Species = "Black_Sea_Bass",
    MinDepth = 1,
    MaxDepth = 50,
    #MinComDepth = 6,
    #MaxComDepth = 26,
    MinOptimalTemp = 0.27,
    MaxOptimalTemp = 29.45,
    #MinComTemp = 11.66,
    #MaxComTemp = 25.25,
    MinOptimalSal = 19.98, 
    MaxOptimalSal = 36.68
    #MinComSal = 29.83,
    #MaxComSal = 36.18
  )
   
  # Haddock
  H_dd <- data.frame(
    Species = "Haddock",
    MinDepth = 10,
    MaxDepth = 450,
    #MinComDepth = 10,
    #MaxComDepth = 200,
    MinOptimalTemp = 0.3,
    MaxOptimalTemp = 18.98,
    #MinComTemp = 6.57,
    #MaxComTemp = 12.74,
    MinOptimalSal = 5.2, 
    MaxOptimalSal = 39.61
    #MinComSal = 9,
    #MaxComSal = 35.28
  )
  
  # Scup
  scup <- data.frame(
    Species = "Scup",
    MinDepth = 15,
    MaxDepth = 50,
    #MinComDepth = 18,
    #MaxComDepth = 33,
    MinOptimalTemp = 8.25,
    MaxOptimalTemp = 29.24,
    #MinComTemp = 10.25,
    #MaxComTemp = 25.04,
    MinOptimalSal = 19.98, 
    MaxOptimalSal = 36.67
    #MinComSal = 29.5,
    #MaxComSal = 36.17
  )
  
  # Yellowtail flounder
  Y_fl <- data.frame(
    Species = "Yellowtail_Flounder",
    MinDepth = 27,
    MaxDepth = 364,
    #MinComDepth = 36,
    #MaxComDepth = 91,
    MinOptimalTemp = 1.55,
    MaxOptimalTemp = 18.98,
    #MinComTemp = 5.83,
    #MaxComTemp = 14.13,
    MinOptimalSal = 20.83, 
    MaxOptimalSal = 35.42
    #MinComSal = 27.99,
    #MaxComSal = 32.87
  )
  
  # combine
  aqmps_d <- bind_rows(
    a_cod, a_mac, B_sb, H_dd, scup, Y_fl  
  )

  
  write.csv(aqmps_d, file = here("./data/aquamaps_envr_depth_data.csv"))  
  
  # add to all_traits
  
  all_traits$MaxDepth <- 1
  all_traits$MinDepth <- 1
  all_traits$MaxOptimalTemp <- 1
  all_traits$MinOptimalTemp <-1
  all_traits$MaxOptimalSal <- 1
  all_traits$MinOptimalSal <-1
 

  all_traits <- all_traits %>%
    rows_update(aqmps_d, by = "Species")
  

  