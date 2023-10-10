library(tidyverse)
library(lme4)
library(lmtest)
library(emmeans)
library(effects)

# Data Loading ------------------------------------------------------------


pisco_hr <- read_csv(here::here("data", "processed_data", "pisco_rf_hr_predict.csv")) %>%
  select(observed_homerange, predicted_homerange, species)
pisco_pld <- read_csv(here::here("data", "processed_data", "pisco_rf_pld_predict.csv")) %>%
  select(observed_PLD, predicted_PLD, species) %>%
  mutate(predicted_PLD = case_when(
    species %in% c("Prionace glauca", "Alopias vulpinus") ~ 0,
    TRUE ~ predicted_PLD
  ))
pisco <- read_csv(here::here("data", "raw_data", "PISCO_kelpforest_fish.1.3.csv"))
pisco_site <- read_csv(here::here("data", "raw_data", "PISCO_kelpforest_site_table.1.2.csv"))
pisco_taxa <- read_csv(here::here("data", "raw_data", "PISCO_kelpforest_taxon_table.1.2.csv"))


# Data Manipulation -------------------------------------------------------

pisco <- list(pisco, pisco_site, pisco_taxa) %>%
  reduce(full_join) %>%
  rename(species = species_definition)

pisco_rf <- full_join(pisco_hr, pisco_pld, relationship = "many-to-many") %>%
  group_by(species) %>%
  reframe(
    predicted_homerange = mean(predicted_homerange, na.rm = TRUE),
    predicted_PLD = mean(predicted_PLD, na.rm = TRUE),
    observed_homerange = observed_homerange,
    observed_PLD = observed_PLD
  ) %>%
  mutate(home_range = case_when(
    !is.na(observed_homerange) ~ observed_homerange,
    is.na(observed_homerange) ~ predicted_homerange
  )) %>%
  rename(pld = predicted_PLD) %>%
  filter(pld < 400) %>%
  mutate(magnitude_homerange = floor(log10(home_range))) %>%
  mutate(month_pld = ceiling(pld / 30)) %>%
  mutate(class = case_when(
    month_pld <= 1 & magnitude_homerange < 2 ~ 1,
    magnitude_homerange < -1 & month_pld == 2 ~ 2,
    month_pld == 3 ~ 3,
    month_pld > 3 ~ 4,
    magnitude_homerange %in% c(-1, 0, 1) & month_pld == 2 ~ 5,
    magnitude_homerange >= 2 ~ 6
  )) %>%
  select(species, class, magnitude_homerange, month_pld) %>%
  mutate(class = as.factor(class)) %>%
  distinct() %>%
  mutate(class2 = class) %>%
  select(-class) %>%
  mutate(class = case_when(
    class2 == 1 ~ "1: mid/low",
    class2 == 2 ~ "2: low/mid",
    class2 == 3 ~ "3: mid/high",
    class2 == 4 ~ "4: mid/veryhigh",
    class2 == 5 ~ "5: mid/mid",
    class2 == 6 ~ "6: high/mid"
  ))


sebastes_1 <- c("Sebastes atrovirens/carnatus/chrysomelas/caurinus", -2, 2, 2, "2: low/mid")
sebastes_2 <- c("Sebastes carnatus/caurinus", -2, 2, 2, "2: low/mid")
sebastes_3 <- c("Sebastes chrysomelas/carnatus", -2, 2, 2, "2: low/mid")

pisco_rf <- rbind(pisco_rf, sebastes_1)
pisco_rf <- rbind(pisco_rf, sebastes_2)
pisco_rf <- rbind(pisco_rf, sebastes_3)

pisco <- full_join(pisco, pisco_rf, multiple = "all") %>%
  filter(!is.na(count)) %>%
  filter(!is.na(species)) %>%
  mutate(magnitude_homerange = as.numeric(magnitude_homerange))

pisco_species <- pisco %>%
  select(species, magnitude_homerange, month_pld, class) %>%
  distinct() %>%
  na.omit()

species_list <- pisco_species %>%
  pull(species)

pisco$date <- as.Date(with(pisco, paste(year, month, day, sep = "-")), "%Y-%m-%d")

dates = unique(pisco$date) %>% as.data.frame()

pisco <- pisco %>%
  filter(species %in% species_list) %>%
  filter(site_status == "MPA")

pisco <- full_join(pisco, pisco_species) %>% 
  filter(!is.na(class2)) %>% 
  filter(MPA_Name %in% c("Point Conception SMR", "Naples SMCA", "Campus Point SMCA",
                         "Anacapa Island SMCA", "Anacapa Island SMR", "Scorpion SMR",
                         "Gull Island SMR", "Painted Cave SMCA", "Carrington Point SMR",
                         "South Point SMR", "Harris Point SMR"))


# initial exploratory plots -----------------------------------------------

ggplot(pisco, aes(magnitude_homerange, month_pld)) +
  geom_jitter(aes(color = class)) +
  theme_bw()

pisco_post_hw = pisco %>% 
  filter(year > 2016)

ggplot(pisco_post_hw, aes(magnitude_homerange, month_pld)) +
  geom_jitter(aes(color = class)) +
  theme_bw()

ggplot(pisco_post_hw, aes(date, count, color = class)) +
  geom_point() +
  theme_bw() +
  facet_wrap(~MPA_Name, scales = "free_y")

ggplot(pisco_post_hw, aes(date, log(count), color = class)) +
  geom_jitter(alpha = .5) +
  theme_bw() +
  facet_wrap(~MPA_Name, scales = "free_y") +
  geom_smooth(method = "glm", se = FALSE, aes(group = species))

ggplot(pisco, aes(date, log(count))) +
  geom_line(aes(color = species)) +
  theme_bw() +
  facet_grid(MPA_Name ~ class) +
  theme(legend.position = "none")


# GLMs --------------------------------------------------------------------

m1 = glm(count ~ class, data = pisco_post_hw, family = poisson())
summary(m1)
sjPlot::plot_model(m1)

m2 = glmer(count ~ class + (1|species), data = pisco_post_hw, family = poisson())
summary(m2)
sjPlot::plot_model(m2)

m3 = glmer(count ~ MPA_Name + (1|class/species), data = pisco_post_hw, family = poisson())
summary(m3)
sjPlot::plot_model(m3)
