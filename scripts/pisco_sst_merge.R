library(tidyverse)

SST = read_csv(here::here("data", "raw_data", "SST.csv")) %>% 
  as.data.frame()
pisco = read_csv(here::here("data", "raw_data", "pisco_rf.csv")) %>% 
  mutate(lat_c = as.character(round(latitude,2)),
         lon_c = as.character(round(longitude,2)))

# SST_lon = sort(unique(SST$longitude))
# SST_lat = sort(unique(SST$latitude))
# 
# p_lon = sort(unique(pisco$longitude))
# p_lat = sort(unique(pisco$latitude))

SST_filter = SST %>% 
  mutate(lat_c = as.character(round(latitude,2)),
         lon_c = as.character(round(longitude,2)))

SST_filter = SST_filter %>% 
  filter((lat_c %in% pisco$lat_c) & (lon_c %in% pisco$lon_c)) %>% 
  mutate(date = as.Date(stringr::str_remove(t, "T00:00:00Z"))) %>% 
  select(-t, -latitude, -longitude)

rm(SST)

pisco_SST = left_join(pisco, SST_filter)

write_csv(pisco_SST, here::here("data", "processed_data", "pisco_SST.csv"))
