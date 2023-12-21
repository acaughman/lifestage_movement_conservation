library(tidyverse)
library(patchwork)
library(lme4)
library(lubridate)
library(jtools)
library(marginaleffects)
library(interactions)
library(emmeans)
library(MASS)

data <- read_csv(here::here("data", "raw_data", "pisco_rf.csv")) %>%
  filter(!is.na(homerange)) %>%
  filter(level != "CAN") %>%
  group_by(sciname, length_to_use, year, transect, date, affiliated_mpa, level, zone, site) %>%
  mutate(
    total_count = sum(count),
    biomass = sum(weight_kg),
  ) %>%
  ungroup() %>% 
  dplyr::select(-transect, -level, -zone, -count, -weight_kg, -weight_g, -lat_c, -lon_c, -site, -longitude, -latitude) %>%
  distinct() %>%
  group_by(sciname, date, affiliated_mpa) %>%
  mutate(
    total_count = sum(total_count),
    total_biomass = sum(biomass, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  distinct() %>%
  mutate(move_combo = paste(magnitude_homerange, month_pld)) %>%
  filter(site_status == "MPA") %>% 
  filter(mpa_defacto_designation != "ref")
 
scinames <- data %>%
  dplyr::select(sciname, common_name, month_pld, magnitude_homerange) %>%
  distinct()

species_mpa_num <- data %>%
  group_by(sciname, affiliated_mpa) %>%
  summarize(count = n()) %>%
  filter(count > 10) %>%
  group_by(sciname) %>%
  mutate(mpa_count = n()) %>%
  filter(mpa_count > 1) %>%
  left_join(scinames)

sub_data <- data %>%
  filter(sciname %in% species_mpa_num$sciname) %>% 
  dplyr::select(-length_to_use, -biomass) %>%
  distinct() %>% 
  mutate(move_combo = fct_relevel(as.factor(move_combo), c("-2 0", "-1 1", "-3 2", "-2 2", "-1 2"))) %>% 
  group_by(sciname, date, affiliated_mpa) %>% 
  mutate(temp = mean(temp, na.rm=TRUE)) %>% 
  distinct()

# Count Model -------------------------------------------------------------

count_model = glmer(total_count ~ scale(size)*move_combo + 
                      scale(min_dist)*move_combo + 
                      scale(max_dist)*move_combo + 
                      scale(temp)*move_combo +
                      scale(habitat_diversity) +
                      (1|sciname), 
                    data = sub_data, 
                    family = poisson(link = "log"),
                    control=glmerControl(optimizer="bobyqa",
                                         optCtrl=list(maxfun=2e5)))
summary(count_model)

car::Anova(count_model, type = 3)

sizecoefs = data.frame(PLD = c(0, 1, 2, 2, 2),
                       hr = c(-2, -1, -3, -2, -1),
                       coef = c(0,-0.10220,   0.36371 , 0.22509 , 0.06817 ))

ggplot(sizecoefs) +
  geom_point(aes(PLD, coef, color=as.factor(hr))) +
  geom_smooth(aes(PLD, coef), method = "lm", se = FALSE)

ggplot(sizecoefs) +
  geom_point(aes(hr, coef, color=as.factor(PLD))) +
  geom_smooth(aes(hr, coef), method = "lm", se = FALSE)

mindistcoefs = data.frame(PLD = c(0, 1, 2, 2, 2),
                          hr = c(-2, -1, -3, -2, -1),
                          coef = c(0, 0.86974, -0.16222, 0.38617, -0.13356))

ggplot(mindistcoefs) +
  geom_point(aes(PLD, coef, color=as.factor(hr))) +
  geom_smooth(aes(PLD, coef), method = "lm", se = FALSE)

ggplot(mindistcoefs) +
  geom_point(aes(hr, coef, color=as.factor(PLD))) +
  geom_smooth(aes(hr, coef), method = "lm", se = FALSE)

maxdistcoefs = data.frame(PLD = c(0, 1, 2, 2, 2),
                          hr = c(-2, -1, -3, -2, -1),
                          coef = c(0,  0.59091, 0.48508  , 0.37683, -0.08340))

ggplot(maxdistcoefs) +
  geom_point(aes(PLD, coef, color=as.factor(hr))) +
  geom_smooth(aes(PLD, coef), method = "lm", se = FALSE)

ggplot(maxdistcoefs) +
  geom_point(aes(hr, coef, color=as.factor(PLD))) +
  geom_smooth(aes(hr, coef), method = "lm", se = FALSE)

e1 = emmip(count_model, move_combo ~ size, cov.reduce = range) +
  theme_bw() +
  labs(x = "MPA Size",
       color = "Movement (hr/pld)") +
  scale_color_viridis_d()
em = emtrends(count_model, ~ move_combo, var = c("size"))
em
contrast(em, method = "pairwise")

e2 = emmip(count_model, move_combo ~ min_dist, cov.reduce = range) +
  theme_bw()  +
  labs(x = "Minimum Distance",
       color = "Movement (hr/pld)") +
  scale_color_viridis_d()
em = emtrends(count_model, ~ move_combo, var = "min_dist")
em
contrast(em, method = "pairwise")

e3 = emmip(count_model, move_combo ~ max_dist, cov.reduce = range) +
  theme_bw()  +
  labs(x = "Maximum Distance",
       color = "Movement (hr/pld)") +
  scale_color_viridis_d()
em = emtrends(count_model, ~ move_combo, var = "max_dist")
em
contrast(em, method = "pairwise")

e4 = emmip(count_model, move_combo ~ temp, cov.reduce = range)+
  theme_bw()  +
  labs(x = "Temperature",
       color = "Movement (hr/pld)") +
  scale_color_viridis_d()
em = emtrends(count_model, ~ move_combo, var = "temp")
em
contrast(em, method = "pairwise")

# e5 = emmip(count_model, move_combo ~ habitat_diversity, cov.reduce = range)+
#   theme_bw()  +
#   labs(x = "Habitat Diversity",
#        color = "Movement (hr/pld)") +
#   scale_color_viridis_d()
# em = emtrends(count_model, ~ move_combo, var = "habitat_diversity")
# em
# contrast(em, method = "pairwise")

plot <- (e1 + e4) / (e2 + e3) + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect")
plot

ggsave(plot, file = paste0("count_model.pdf"), path = here::here("figs"), height = 10, width = 12)


# Biomass Model -------------------------------------------------------------

biomass_model = lmer(total_biomass ~ scale(size)*move_combo + 
                      scale(min_dist)*move_combo + 
                      scale(max_dist)*move_combo + 
                      scale(temp)*move_combo + 
                      scale(habitat_diversity) + 
                       (1|sciname), 
                    data = sub_data)
summary(biomass_model)

car::Anova(biomass_model, type = 3)

sizecoefs = data.frame(PLD = c(0, 1, 2, 2, 2),
                       hr = c(-2, -1, -3, -2, -1),
                   coef = c(0, 3.0818, 1.1167, 4.0932 , -2.6211))

ggplot(sizecoefs) +
  geom_point(aes(PLD, coef, color = as.factor(hr))) +
  geom_smooth(aes(PLD, coef), method = "lm", se = FALSE)

ggplot(sizecoefs) +
  geom_point(aes(hr, coef, color = as.factor(PLD))) +
  geom_smooth(aes(hr, coef), method = "lm", se = FALSE)

mindistcoefs = data.frame(PLD = c(0, 1, 2, 2, 2),
                          hr = c(-2, -1, -3, -2, -1),
                       coef = c(0,  -8.6464, -3.2115,  -0.9482,   0.1291))

ggplot(mindistcoefs) +
  geom_point(aes(PLD, coef, color = as.factor(hr))) +
  geom_smooth(aes(PLD, coef), method = "lm", se = FALSE)

ggplot(mindistcoefs) +
  geom_point(aes(hr, coef, color = as.factor(PLD))) +
  geom_smooth(aes(hr, coef), method = "lm", se = FALSE)

maxdistcoefs = data.frame(PLD = c(0, 1, 2, 2, 2),
                          hr = c(-2, -1, -3, -2, -1),
                          coef = c(0,  4.8116, 4.4490, 6.1217,-3.3143 ))

ggplot(maxdistcoefs) +
  geom_point(aes(PLD, coef, color = as.factor(hr))) +
  geom_smooth(aes(PLD, coef), method = "lm", se = FALSE)

ggplot(maxdistcoefs) +
  geom_point(aes(hr, coef, color = as.factor(PLD))) +
  geom_smooth(aes(hr, coef), method = "lm", se = FALSE)

e1 = emmip(biomass_model, move_combo ~ size, cov.reduce = range) +
  theme_bw()   +
  labs(x = "MPA Size",
       color = "Movement (hr/pld)") +
  scale_color_viridis_d()
em = emtrends(biomass_model, ~ move_combo, var = c("size"))
em
contrast(em, method = "pairwise")

# emmip(biomass_model, move_combo ~ min_dist, cov.reduce = range)
# em = emtrends(biomass_model, ~ move_combo, var = "min_dist")
# em
# contrast(em, method = "pairwise")

e3 = emmip(biomass_model, move_combo ~ max_dist, cov.reduce = range) +
  theme_bw()   +
  labs(x = "Max Distance",
       color = "Movement (hr/pld)") +
  scale_color_viridis_d()
em = emtrends(biomass_model, ~ move_combo, var = "max_dist")
em
contrast(em, method = "pairwise")

# emmip(biomass_model, move_combo ~ temp, cov.reduce = range)
# em = emtrends(biomass_model, ~ move_combo, var = "temp")
# em
# contrast(em, method = "pairwise")

# e5 = emmip(biomass_model, move_combo ~ habitat_diversity, cov.reduce = range)+
#   theme_bw()   +
#   labs(x = "Habitat Diversity",
#        color = "Movement (hr/pld)") +
#   scale_color_viridis_d()
# em = emtrends(biomass_model, ~ move_combo, var = "habitat_diversity")
# em
# contrast(em, method = "pairwise")

plot <- (e3) + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect")
plot

ggsave(plot, file = paste0("biomass_model.pdf"), path = here::here("figs"), height = 10, width = 8)

