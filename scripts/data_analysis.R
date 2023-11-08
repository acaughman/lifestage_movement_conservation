library(tidyverse)
library(lme4)
library(jtools)
library(marginaleffects)
library(MASS)

data = read_csv(here::here("data", "processed_data", "pisco_SST.csv")) %>% 
  filter(site_status == "MPA") %>% 
  filter(level != "CAN") %>% 
  group_by(sciname, length_to_use, year, transect, date, affiliated_mpa) %>% 
  mutate(total_count = sum(count)) %>% 
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


data = data %>% 
  mutate(movement = magnitude_homerange)


glm1.4 = glm(total_count ~ movement * scale(size) * scale(settlement_mpa_total) + temp + habitat_richness, data = data, family = poisson)
summary(glm1.4)

glm1.7 = glmer(total_count ~ movement * scale(size) * scale(settlement_mpa_total) + (1|sciname), data = data, family = poisson)
summary(glm1.7)
glm1.8 = glmer(total_count ~ movement * scale(size) * scale(settlement_mpa_total) + (1 + total_count|sciname), data = data, family = poisson)
summary(glm1.8)

plot_coefs(glm1.4, glm1.7, glm1.8)

glm1.1 = glm(log(weight_kg) ~ movement * scale(size) * scale(settlement_mpa_total) + temp + habitat_richness, data = data, family = gaussian())
summary(glm1.1)

# |t| > 1.96
glm1.2 = lmer(log(weight_kg) ~ movement * scale(size) * scale(settlement_mpa_total) + (1|sciname), data = data)
summary(glm1.2)
glm1.3 = lmer(log(weight_kg) ~ movement * scale(size) * scale(settlement_mpa_total) + (1 + total_count|sciname), data = data)
summary(glm1.3)

plot_coefs(glm1.1, glm1.2, glm1.3)

data = data %>% 
  mutate(movement = month_pld)

glm2.4 = glm(total_count ~ movement * scale(size) * scale(settlement_mpa_total) + temp + habitat_richness, data = data, family = poisson)
summary(glm2.4)

glm2.7 = glmer(total_count ~ movement * scale(size) * scale(settlement_mpa_total) + (1|sciname), data = data, family = poisson(link = "log"))
summary(glm2.7)
glm2.8 = glmer(total_count ~ movement * scale(size) * scale(settlement_mpa_total) + (1 + total_count |sciname), data = data, family = poisson(link = "log"))
summary(glm2.8)

plot_coefs(glm2.4, glm2.7, glm2.8)

glm2.1 = glm(log(weight_kg) ~ movement * scale(size) * scale(settlement_mpa_total) + temp + habitat_richness, data = data, family = gaussian())
summary(glm2.1)

# |t| > 1.96 
glm2.2 = lmer(log(weight_kg) ~ movement * scale(size) * scale(settlement_mpa_total) + (1|sciname), data = data)
summary(glm2.2)
glm2.3 = lmer(log(weight_kg) ~ movement * scale(size) * scale(settlement_mpa_total) + (1 + total_count|sciname), data = data)
summary(glm2.3)

plot_coefs(glm2.1, glm2.2, glm2.3)

plot_coefs(glm1.4,glm2.4)
plot_coefs(glm1.8,glm2.8)

sjPlot::plot_model(glm1.4,
                   show.values=TRUE, show.p=TRUE)
sjPlot::plot_model(glm1.7,
                   show.values=TRUE, show.p=TRUE)
sjPlot::plot_model(glm1.8,
                   show.values=TRUE, show.p=TRUE)
sjPlot::plot_model(glm2.4,
                   show.values=TRUE, show.p=TRUE)
sjPlot::plot_model(glm2.7,
                   show.values=TRUE, show.p=TRUE)
sjPlot::plot_model(glm2.8,
                   show.values=TRUE, show.p=TRUE)


sjPlot::plot_model(glm1.1,
                   show.values=TRUE, show.p=TRUE)
sjPlot::plot_model(glm1.2,
                   show.values=TRUE, show.p=TRUE)
sjPlot::plot_model(glm1.3,
                   show.values=TRUE, show.p=TRUE)
sjPlot::plot_model(glm2.1,
                   show.values=TRUE, show.p=TRUE)
sjPlot::plot_model(glm2.2,
                   show.values=TRUE, show.p=TRUE)
sjPlot::plot_model(glm2.3,
                   show.values=TRUE, show.p=TRUE)

plot_comparisons(glm1.1, variables = "movement", condition = c("size", "settlement_mpa_total")) +
  labs(
    x = "MPA Size",
    y = "Count Difference with Change in Home Range",
    color = "Settlement Total",
    fill = "Settlement Total"
  ) +
  theme_bw() +
  scale_color_viridis_d() +
  scale_fill_viridis_d()

plot_comparisons(glm2.1, variables = "movement", condition = c("size", "settlement_mpa_total")) +
  labs(
    x = "MPA Size",
    y = "Count Difference with Change in Home Range",
    color = "Settlement Total",
    fill = "Settlement Total"
  ) +
  theme_bw() +
  scale_color_viridis_d() +
  scale_fill_viridis_d()

plot_comparisons(glm1.7, variables = "movement", condition = c("size", "settlement_mpa_total")) +
  labs(
    x = "MPA Size",
    y = "Count Difference with Change in Home Range",
    color = "Settlement Total",
    fill = "Settlement Total"
  ) +
  theme_bw() +
  scale_color_viridis_d() +
  scale_fill_viridis_d()

plot_comparisons(glm1.7, variables = "movement", condition = c("settlement_mpa_total", "size")) +
  labs(
    x = "Settlement Total",
    y = "Count Difference with Change in Home Range",
    color = "MPA Size",
    fill = "MPA Size"
  ) +
  theme_bw() +
  scale_color_viridis_d() +
  scale_fill_viridis_d()

plot_comparisons(glm2.7, variables = "movement", condition = c("size", "settlement_mpa_total")) +
  labs(
    x = "MPA Size",
    y = "Count Difference with Change in PLD",
    color = "Settlement Total",
    fill = "Settlement Total"
  ) +
  theme_bw() +
  scale_color_viridis_d() +
  scale_fill_viridis_d()

plot_comparisons(glm2.7, variables = "movement", condition = c("settlement_mpa_total", "size")) +
  labs(
    x = "Settlement Total",
    y = "Count Difference with Change in PLD",
    color = "MPA Size",
    fill = "MPA Size"
  ) +
  theme_bw() +
  scale_color_viridis_d() +
  scale_fill_viridis_d()


data11 = data %>% 
  filter(move_combo == "-1 1")
data12 = data %>% 
  filter(move_combo == "-1 2")
data20 = data %>% 
  filter(move_combo == "-2 0")
data22 = data %>% 
  filter(move_combo == "-2 2")
data30 = data %>% 
  filter(move_combo == "-3 0")
data32 = data %>% 
  filter(move_combo == "-3 2")
data02 = data %>% 
  filter(move_combo == "0 2")


glm11 = glmer(total_count ~ scale(size) * scale(settlement_mpa_total) + (1|sciname), data = data11, family = poisson)
summary(glm11)
glm12 = glmer(total_count ~ scale(size) * scale(settlement_mpa_total) + (1 + total_count|sciname), data = data12, family = poisson)
summary(glm12)
glm20 = glmer(total_count ~ scale(size) * scale(settlement_mpa_total) + (0 + total_count|sciname), data = data20, family = poisson)
summary(glm20)
glm22 = glmer(total_count ~ scale(size) * scale(settlement_mpa_total) + (1 + total_count|sciname), data = data22, family = poisson)
summary(glm22)
glm30 = glm(total_count ~ scale(size) * scale(settlement_mpa_total), data = data30, family = poisson)
summary(glm30)
glm32 = glmer(total_count ~ scale(size) * scale(settlement_mpa_total) + (1|sciname), data = data32, family = poisson)
summary(glm32)
glm02 = glm(total_count ~ scale(size) * scale(settlement_mpa_total), data = data02, family = poisson)
summary(glm02)

plot_summs(glm30, glm32, glm20, glm22, glm11, glm12)
plot_summs(glm32, glm22, glm12, glm02)

sjPlot::plot_model(glm32,
                   show.values=TRUE, show.p=TRUE)
sjPlot::plot_model(glm20,
                   show.values=TRUE, show.p=TRUE)
sjPlot::plot_model(glm22,
                   show.values=TRUE, show.p=TRUE)
sjPlot::plot_model(glm11,
                   show.values=TRUE, show.p=TRUE)
sjPlot::plot_model(glm12,
                   show.values=TRUE, show.p=TRUE)


glm11 = lm(log(weight_kg) ~ scale(size) * scale(settlement_mpa_total), data = data11)
summary(glm11)
glm12 = lmer(log(weight_kg) ~ scale(size) * scale(settlement_mpa_total) + (1|sciname), data = data12)
summary(glm12)
glm20 = lmer(log(weight_kg) ~ scale(size) * scale(settlement_mpa_total) + (1|sciname), data = data20)
summary(glm20)
glm22 = lmer(log(weight_kg) ~ scale(size) * scale(settlement_mpa_total) + (1|sciname), data = data22)
summary(glm22)
glm30 = lm(log(weight_kg) ~ scale(size) * scale(settlement_mpa_total), data = data30)
summary(glm30)
glm32 = lmer(log(weight_kg) ~ scale(size) * scale(settlement_mpa_total) + (1|sciname), data = data32)
summary(glm32)
glm02 = lm(log(weight_kg) ~ scale(size) * scale(settlement_mpa_total), data = data02)
summary(glm02)

plot_summs(glm30, glm32, glm20, glm22, glm11, glm12)
plot_summs(glm32, glm22, glm12, glm02)

sjPlot::plot_model(glm32,
                   show.values=TRUE, show.p=TRUE)
sjPlot::plot_model(glm20,
                   show.values=TRUE, show.p=TRUE)
sjPlot::plot_model(glm22,
                   show.values=TRUE, show.p=TRUE)
sjPlot::plot_model(glm11,
                   show.values=TRUE, show.p=TRUE)
sjPlot::plot_model(glm12,
                   show.values=TRUE, show.p=TRUE)
