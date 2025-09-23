# length at age 1

  length_at_age_fun <- function(LengthAsymptotic, VonBertK, t_zero, age = 1){
    
    L_age <- LengthAsymptotic * (1 - exp(-VonBertK * (age - t_zero)))
    L_age
    
  }
  
  age <- 1
  
  all_traits <- all_traits |>
    mutate(LengthAge1 = pmap_dbl(list(LengthAsymptotic, VonBertK, t_zero),
                                     ~ length_at_age_fun(..1, ..2, ..3, age)))
  
   
 # write_csv(all_traits, file = here("./data/all_traits.csv"))


#  d <- all_traits |>
#    select(Species, LengthAge1) |>
#    mutate(LengthAge1_in = LengthAge1 * 0.393701)