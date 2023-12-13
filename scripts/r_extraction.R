library(tidyverse)
library(FishLife)

data = read_csv(here::here("data", "raw_data", "species_of_interest.csv"))

missing_data = data %>% 
  filter(is.na(r_fin))

have_data = data %>% 
  filter(!is.na(r_fin))

fish_data = FishLife::FishBase_and_RAM$beta_gv %>% 
  as.data.frame()

data_filled = data.frame()

for(i in 1:nrow(missing_data)) {
  species = Match_species(genus_species = missing_data$species[i])
  
  data_filled = rbind(fish_data[species[[1]],], data_filled)
}

missing_data$r_fin = data_filled$r

all_data = rbind(have_data, missing_data)

write_csv(all_data, here::here("data", "raw_data", "species_of_interest.csv"))
