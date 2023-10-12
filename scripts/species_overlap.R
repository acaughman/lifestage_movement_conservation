library(here)
library(lubridate)
library(tidyverse)

rf_data = read_csv(here::here("data", "raw_data", "rf_data.csv"))
pisco = read_csv(here::here("data", "raw_data", "full_pisco.csv")) %>% 
  distinct()
pisco$date <- as.Date(with(pisco, paste(year, month, day, sep = "-")), "%Y-%m-%d")

rf_in_pisco = rf_data %>% 
  filter(sciname %in% pisco$sciname) 
rf_out_pisco = rf_data %>% 
  filter(!sciname %in% pisco$sciname)

rf_species = pisco %>% 
  filter(sciname %in% rf_in_pisco$sciname)
rf_no_species = pisco %>% 
  filter(!sciname %in% rf_in_pisco$sciname)

rf_no = unique(rf_no_species$sciname)

ggplot(rf_in_pisco, aes(magnitude_homerange, month_pld)) +
  geom_jitter(aes(color = as.factor(magnitude_homerange), shape = as.factor(month_pld))) +
  theme_bw()

pisco_rf = inner_join(pisco, rf_data) 

write_csv(pisco_rf, here::here("data", "raw_data", "pisco_rf.csv"))

pisco_post_hw = pisco_rf %>% 
  filter(year > 2016)

ggplot(pisco_post_hw, aes(magnitude_homerange, month_pld)) +
  geom_jitter(aes(color = as.factor(magnitude_homerange), shape = as.factor(month_pld))) +
  theme_bw()

ggplot(pisco_post_hw, aes(date, count)) +
  geom_point(aes(color = as.factor(magnitude_homerange), shape = as.factor(month_pld))) +
  theme_bw() +
  facet_wrap(~affiliated_mpa, scales = "free_y")

ggplot(pisco_post_hw, aes(date, log(count))) +
  geom_jitter(alpha = .5, aes(color = as.factor(magnitude_homerange), shape = as.factor(month_pld))) +
  theme_bw() +
  facet_wrap(~affiliated_mpa, scales = "free_y") +
  geom_smooth(method = "glm", se = FALSE, aes(group = sciname, color = as.factor(magnitude_homerange)))
