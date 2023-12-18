library(here)
library(lubridate)
library(tidyverse)
library(rfishbase)
library(FishLife)

pisco = read_csv(here::here("data", "raw_data", "full_pisco.csv")) %>% 
  distinct()

species_num = pisco %>% 
  group_by(sciname) %>% 
  summarize(count = n())

species_mpa_num = pisco %>% 
  group_by(sciname, affiliated_mpa) %>% 
  summarize(count = n()) %>% 
  filter(count > 10) %>% 
  group_by(sciname) %>% 
  mutate(mpa_count = n()) %>% 
  filter(mpa_count > 1) %>% 
  left_join(pisco)

scinames = species_mpa_num %>% 
  dplyr::select(sciname) %>% 
  arrange(sciname) %>% 
  distinct() %>% 
  filter(!is.na(sciname)) %>% 
  filter(!grepl("spp", sciname))


# load in other data ------------------------------------------------------

life_history <- readRDS(here("data", "raw_data", "MegaData_Ray.rds")) %>%
  as.data.frame() %>% # convert to dataframe
  janitor::clean_names() %>%
  filter(sci_name %in% scinames$sciname) %>% 
  dplyr::select(sci_name, r_fin, kfin) # select relevant columns

names(life_history)[names(life_history) == "sci_name"] <- "species" # rename column so that it matches empirical_homeranges.csv

life_history_means <- life_history %>%
  group_by(species) %>%
  summarise(
    kfin = mean(kfin, na.rm = TRUE), # get mean of carrying capacity for each species
    r_fin = mean(r_fin, na.rm = TRUE)
  ) # get mean of growth rate for each species

geog <- read_csv(here("data", "raw_data", "geog_range.csv")) %>%
  rename(species = SciName) %>% # rename column
  select(species, geog_range) # select desired columns

species_list <- geog %>%
  pull(species) # get list of 811 species

fish <- load_taxa(server = "fishbase") %>% 
  as.data.frame() %>% 
  filter(Species %in% scinames$sciname) %>% 
  dplyr::select(Species, SpecCode, Genus)

datLength <- species(fish$Species, fields = c("Species", "Length", "LTypeMaxM", "DemersPelag", "FBname")) # get wanted information from fishbase

datLength <- datLength[datLength$LTypeMaxM %in% c("TL", "FL", "SL"), ] # subset to TL FL and SL lengths

dfdat <- ecology(fish$Species, fields = c("Species", "DietTroph", "DietSeTroph", "FoodTroph", "FoodSeTroph")) # get fish ecology information from fishbase

lm1 <- lm(DietTroph ~ FoodTroph, dfdat, na.action = na.omit) # create linear model to fill in missing trophic data

predDat2 <- merge(datLength, dfdat, by = "Species", all.x = TRUE) # merge length and tropic data

predDat2$DietTroph2 <- with(predDat2, ifelse(is.na(DietTroph), predict(lm1, newdata = data.frame(FoodTroph = FoodTroph)), DietTroph)) # replace NA trophic data with predictions from linear model

predDat2 <- predDat2[, c("Species", "DietTroph2")]

predDat2 <- merge(predDat2, fish, by = "Species", all.x = TRUE) # merge data with taxa information

## now predicting missing genuses for dietTroph
missingGen <- unique(predDat2$Genus[is.na(predDat2$DietTroph2)]) # get genuses with NA values

miss_diet <- fish %>%
  filter(Genus %in% missingGen) %>% # get missing genuses
  mutate(SpecCode = as.character(SpecCode)) # change SpecCode to a character

grptroph <- ecology(miss_diet$gensp, fields = c("DietTroph", "SpecCode")) %>% # get ecology data of species
  mutate(SpecCode = as.character(SpecCode)) # change SpecCode to a character

d2 <- dplyr::left_join(miss_diet, grptroph) %>% # joining by SpecCode
  group_by(Genus) %>%
  summarize(AvgDietTroph = mean(DietTroph, na.rm = TRUE)) # get mean trophic level by genus

predDat2 <- merge(predDat2, d2, by = "Genus", all.x = T) # merge all data

predDat2$DietTroph3 <- with(predDat2, ifelse(is.na(DietTroph2), AvgDietTroph, DietTroph2)) # replace missing trophic data with genus averages
predDat2$genus <- predDat2$Genus # rename column

fish_df <- fish %>%
  rename(spec_code = SpecCode) %>% # rename column
  dplyr::select(spec_code, Species) %>% # select relevant columns
  rename(species = Species) # rename column

fish_data <- merge(datLength, predDat2) %>%
  janitor::clean_names() %>% # clean the names
  dplyr::select(-genus_2, -diet_troph2, -avg_diet_troph)# merge all data and remove columns that are not relevant

fish_data_o <- left_join(fish_df, fish_data) %>%
  dplyr::select(species,demers_pelag) # merge all data

fish_data_means <- left_join(fish_df, fish_data) %>%
  group_by(species) %>%
  summarise(
    length = mean(length, na.rm = TRUE), # get mean length by species
    diet_troph3 = mean(diet_troph3, na.rm = TRUE)
  ) # get mean trophic level for species

fish_data <- left_join(fish_data_means, fish_data_o, by = "species") %>%
  distinct() # get all the data for each fish species with length and trophic level data

geog = read_csv(here::here("01_data", "01_raw_data", "all_hcaf_species_native.csv"))
geog_species = read_csv(here::here("01_data", "01_raw_data", "all_speciesid.csv")) %>% 
  rename(SpeciesID = SPECIESID) %>% 
  mutate(species = paste(Genus, Species)) %>% 
  filter(species %in% data$species)

geog = left_join(geog_species, geog, multiple = "all") %>% 
  filter(Probability >= .5) %>% 
  select(species, CsquareCode, CenterLat, CenterLong) 

fishlife = list(fish_data, life_history_means, geog) %>% 
  reduce(full_join) %>% 
  filter(!is.na(diet_troph3)) %>% 
  filter(!is.na(diet_troph3))

write_csv(fishlife, here::here("data", "raw_data", "species_of_interest_update.csv"))


# get missing r values ------------------------------------------------------

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

