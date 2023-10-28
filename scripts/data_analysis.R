library(tidyverse)
library(lme4)
library(jtools)
library(marginaleffects)

data = read_csv(here::here("data", "processed_data", "pisco_SST.csv")) %>% 
  select(-lat_c, -lon_c) %>% 
  filter(site_status == "MPA") %>% 
  filter(level != "CAN") %>% 
  group_by(sciname, length_to_use, year, transect, date, affiliated_mpa) %>% 
  mutate(total_count = sum(count)) %>% 
  select(-zone, -level, -count) %>% 
  distinct() %>% 
  mutate(move_combo = paste(magnitude_homerange, month_pld)) %>% 
  mutate()

ggplot(data, aes(magnitude_homerange, month_pld)) +
  geom_jitter(aes(color = as.factor(magnitude_homerange), shape = as.factor(month_pld))) +
  theme_bw() 

ggplot(data, aes(date, total_count)) +
  geom_point(aes(color = as.factor(magnitude_homerange), shape = as.factor(month_pld))) +
  theme_bw() +
  facet_wrap(~affiliated_mpa, scales = "free_y")

ggplot(data, aes(date, log(total_count))) +
  geom_jitter(alpha = .5) +
  theme_bw() +
  facet_wrap(~affiliated_mpa, scales = "free_y") +
  geom_smooth(method = "glm", se = FALSE, aes(group = sciname, color = as.factor(move_combo)))

data12 = data %>% 
  filter(move_combo == "-1 2")
data20 = data %>% 
  filter(move_combo == "-2 0")
data22 = data %>% 
  filter(move_combo == "-2 2")
data32 = data %>% 
  filter(move_combo == "-3 2")

glm12 = glm(total_count ~ scale(size) * scale(settlement_mpa_total) + temp + habitat_richness, data = data12, family = poisson)
summary(glm1)
glm20 = glm(total_count ~ scale(size) * scale(settlement_mpa_total) + temp + habitat_richness, data = data20, family = poisson)
summary(glm2)
glm22 = glm(total_count ~ scale(size) * scale(settlement_mpa_total) + temp + habitat_richness, data = data22, family = poisson)
summary(glm3)
glm32 = glm(total_count ~ scale(size) * scale(settlement_mpa_total) + temp + habitat_richness, data = data32, family = poisson)
summary(glm4)

plot_summs(glm32, glm20, glm22, glm12)

data = data %>% 
  mutate(movement = magnitude_homerange)

glm1 = glm(total_count ~ movement * scale(size) * scale(settlement_mpa_total) + temp + habitat_richness, data = data, family = poisson)
summary(glm1)

data = data %>% 
  mutate(movement = month_pld)

glm2 =  glm(total_count ~ movement * scale(size) * scale(settlement_mpa_total) + temp + habitat_richness, data = data, family = poisson)
summary(glm2)
plot_coefs(glm1,glm2)

plot_comparisons(glm1, variables = "movement", condition = c("size", "settlement_mpa_total")) +
  labs(
    x = "MPA Size",
    y = "Count Difference with Change in Home Range",
    color = "Settlement Total",
    fill = "Settlement Total"
  ) +
  theme_bw() +
  scale_color_viridis_d() +
  scale_fill_viridis_d()

plot_comparisons(glm1, variables = "movement", condition = c("settlement_mpa_total", "size")) +
  labs(
    x = "Settlement Total",
    y = "Count Difference with Change in Home Range",
    color = "MPA Size",
    fill = "MPA Size"
  ) +
  theme_bw() +
  scale_color_viridis_d() +
  scale_fill_viridis_d()

plot_comparisons(glm2, variables = "movement", condition = c("size", "settlement_mpa_total")) +
  labs(
    x = "MPA Size",
    y = "Count Difference with Change in PLD",
    color = "Settlement Total",
    fill = "Settlement Total"
  ) +
  theme_bw() +
  scale_color_viridis_d() +
  scale_fill_viridis_d()

plot_comparisons(glm2, variables = "movement", condition = c("settlement_mpa_total", "size")) +
  labs(
    x = "Settlement Total",
    y = "Count Difference with Change in PLD",
    color = "MPA Size",
    fill = "MPA Size"
  ) +
  theme_bw() +
  scale_color_viridis_d() +
  scale_fill_viridis_d()
