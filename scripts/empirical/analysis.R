library(tidyverse)
library(patchwork)
library(mgcv)
library(lme4)
library(sjPlot)
library(sjmisc)
library(emmeans)
library(MASS)

data <- read_csv(here::here("data", "raw_data", "pisco_rf.csv")) %>%
  filter(!is.na(homerange)) %>%
  filter(level != "CAN") %>%
  filter(site_status == "MPA")%>% 
  filter(mpa_defacto_designation != "ref") %>%
  dplyr::select(-lat_c, -lon_c, -latitude, -longitude) %>% 
  distinct() %>% 
  group_by(date, affiliated_mpa) %>% 
  mutate(temp = mean(temp, na.rm=TRUE)) %>% 
  ungroup() %>% 
  distinct()%>%
  mutate(magnitude_homerange = magnitude_homerange + 3) %>% 
  mutate(move_combo = paste(magnitude_homerange, "/" , month_pld)) %>% 
  group_by(date, site, affiliated_mpa, mpa_defacto_designation, zone, transect) %>% 
  mutate(total_count = sum(count),
         total_biomass = sum(weight_kg))%>% 
  tidyr::replace_na(list(total_biomass = 0, count = 0)) %>% 
  ungroup() %>% 
  dplyr::select(-level, -weight_kg, -length_to_use, -weight_g) %>% 
  distinct() %>% 
  group_by(date, affiliated_mpa, mpa_defacto_designation) %>% 
  mutate(biomass = mean(total_biomass, na.rm=TRUE),
         count = mean(total_count, na.rm = TRUE),
         n_rep = n(),
         sd = sd(total_biomass, na.rm=TRUE),
         se = sd/sqrt(n_rep)) %>% 
  ungroup() %>% 
  dplyr::select(-site, -zone,-transect, -total_biomass, -total_count) %>% 
  distinct() %>% 
  mutate(imp_lag = year - year(as.Date(implementation_date, tryFormats = c("%m/%d/%Y"))))

scinames <- data %>%
  dplyr::select(sciname, common_name, month_pld, magnitude_homerange) %>%
  distinct()

species_mpa_num <- data %>%
  group_by(sciname, affiliated_mpa) %>%
  summarize(count = n()) %>%
  filter(count > 8) %>%
  group_by(sciname) %>%
  mutate(mpa_count = n()) %>%
  left_join(scinames)

sub_data <- data %>%
  filter(sciname %in% species_mpa_num$sciname) %>% 
  distinct() %>% 
  mutate(move_combo = fct_relevel(as.factor(move_combo), c("2 / 1", "2 / 2", "1 / 0", "1 / 2", "0 / 2"))) %>% 
  distinct() %>% 
  mutate(sciname = fct_relevel(as.factor(sciname), c("Trachurus symmetricus", "Semicossyphus pulcher", 
                                                     "Sebastes melanops", "Caulolatilus princeps",
                                                     "Sebastes mystinus", "Sebastes paucispinis",
                                                     "Sebastes carnatus", "Ophiodon elongatus"))) %>% 
  filter(imp_lag >= 0)


# Count Model -------------------------------------------------------------

count_model1 = gam(biomass ~ s(size, by = move_combo, k = 4, bs = "tp") +
                    s(min_dist, by = move_combo, k = 4, bs = "tp") +
                    move_combo +
                    s(imp_lag, min_dist) + 
                    s(temp, bs = "tp") + prop_rock + mpa_defacto_designation +
                    s(sciname, bs = "re"), 
                  data = sub_data, method = "REML")

summary(count_model1)
# anova(count_model1)


#plot(count_model)
gratia::draw(count_model1, parametric = FALSE) +
  theme_bw()

coef(count_model1)
gratia::variance_comp(count_model1)
gratia::edf(count_model1)

count_model2 = lmer(biomass ~ size:move_combo +
                     min_dist:move_combo +
                     move_combo +
                     imp_lag:min_dist + 
                     temp + prop_rock + mpa_defacto_designation +
                     (1|sciname), 
                   data = sub_data)
summary(count_model2)

AIC(count_model1)
AIC(count_model2)
anova(count_model2, count_model1, test = "Chisq")

# OLD ---------------------------------------------------------------------



# count_model = glmer(floor(count) ~ scale(size)*move_combo*scale(min_dist) + 
#                       scale(max_dist)*move_combo + 
#                       scale(settlement_mpa_total)*move_combo + 
#                       scale(temp) + 
#                       scale(habitat_diversity) + 
#                       (1|mpa_defacto_designation) +
#                       scale(date) + 
#                       (1|sciname), 
#                     data = sub_data, 
#                     family = poisson(link = "log"),
#                     control=glmerControl(optimizer="bobyqa",
#                                          optCtrl=list(maxfun=2e5)))
# 
# summary(count_model)
# 
# car::Anova(count_model, type = 3)
# 
# # count_model2 = glmer(total_count ~ scale(size)*move_combo + 
# #                       scale(min_dist)*move_combo + 
# #                       scale(max_dist)*move_combo + 
# #                       scale(temp)*move_combo + 
# #                       scale(habitat_diversity) + 
# #                       (1|sciname)+
# #                        (0 + scale(habitat_diversity)|sciname)+
# #                        (0 + scale(min_dist)|sciname) +
# #                        (0 + scale(temp)|sciname), 
# #                     data = sub_data, 
# #                     family = poisson(link = "log"),
# #                     control=glmerControl(optimizer="bobyqa",
# #                                          optCtrl=list(maxfun=2e5)))
# # anova(count_model, count_model2)
# 
# # coef(count_model)
# 
# # sizecoefs = data.frame(PLD = c(0, 1, 2, 2, 2),
# #                        hr = c(-2, -1, -3, -2, -1),
# #                        coef = c(0, 0.44430, 0.16043, -0.01842, -0.02858))
# # 
# # ggplot(sizecoefs) +
# #   geom_point(aes(PLD, coef, color=as.factor(hr))) +
# #   geom_smooth(aes(PLD, coef), method = "lm", se = FALSE)
# # 
# # ggplot(sizecoefs) +
# #   geom_point(aes(hr, coef, color=as.factor(PLD))) +
# #   geom_smooth(aes(hr, coef), method = "lm", se = FALSE)
# # 
# # mindistcoefs = data.frame(PLD = c(0, 1, 2, 2, 2),
# #                           hr = c(-2, -1, -3, -2, -1),
# #                           coef = c(0, -1.10346,-0.04528, 0.62926, 0.1461))
# # 
# # ggplot(mindistcoefs) +
# #   geom_point(aes(PLD, coef, color=as.factor(hr))) +
# #   geom_smooth(aes(PLD, coef), method = "lm", se = FALSE)
# # 
# # ggplot(mindistcoefs) +
# #   geom_point(aes(hr, coef, color=as.factor(PLD))) +
# #   geom_smooth(aes(hr, coef), method = "lm", se = FALSE)
# 
# e1 = emmip(count_model, move_combo ~ size|min_dist, cov.reduce = range) +
#   theme_bw() +
#   labs(x = "MPA Size",
#        color = "Movement (hr/pld)") +
#   scale_color_viridis_d()
# em = emtrends(count_model, ~ move_combo, var = c("size"))
# em
# 
# contrast(em, method = "pairwise")
# 
# e2 = emmip(count_model, move_combo ~ min_dist, cov.reduce = range) +
#   theme_bw()  +
#   labs(x = "Minimum Distance",
#        color = "Movement (hr/pld)") +
#   scale_color_viridis_d()
# em = emtrends(count_model, ~ move_combo, var = "min_dist")
# em
# contrast(em, method = "pairwise")
# 
# e3 = emmip(count_model, move_combo ~ max_dist, cov.reduce = range) +
#   theme_bw()  +
#   labs(x = "Maximum Distance",
#        color = "Movement (hr/pld)") +
#   scale_color_viridis_d()
# em = emtrends(count_model, ~ move_combo, var = "max_dist")
# em
# contrast(em, method = "pairwise")
# 
# 
# plot <- (e1) / (e2 + e3) + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect")
# plot
# 
# ggsave(plot, file = paste0("count_model.pdf"), path = here::here("figs"), height = 10, width = 12)
# 
# plot(count_model)
#            
# plot_model(count_model, vline.color="red", show.values = TRUE)
# plot_model(count_model, vline.color="red", type = "emm", terms = c("size", "move_combo"))
# 
# # Biomass Model -------------------------------------------------------------
# 
# biomass_model = lmer(log(biomass) ~ scale(size)*move_combo*scale(min_dist) + 
#                        scale(max_dist)*move_combo + 
#                        scale(settlement_mpa_total)*move_combo + 
#                        scale(temp) + 
#                        scale(habitat_diversity) + 
#                        (1|mpa_defacto_designation) +
#                        scale(date) + 
#                        (1|sciname), 
#                     data = sub_data)
# summary(biomass_model)
# 
# coef(biomass_model)
# 
# car::Anova(biomass_model, type = 3)
# 
# # sizecoefs = data.frame(PLD = c(0, 1, 2, 2, 2),
# #                        hr = c(-2, -1, -3, -2, -1),
# #                    coef = c(0, 3.0818, 1.1167, 4.0932 , -2.6211))
# # 
# # ggplot(sizecoefs) +
# #   geom_point(aes(PLD, coef, color = as.factor(hr))) +
# #   geom_smooth(aes(PLD, coef), method = "lm", se = FALSE)
# # 
# # ggplot(sizecoefs) +
# #   geom_point(aes(hr, coef, color = as.factor(PLD))) +
# #   geom_smooth(aes(hr, coef), method = "lm", se = FALSE)
# 
# # mindistcoefs = data.frame(PLD = c(0, 1, 2, 2, 2),
# #                           hr = c(-2, -1, -3, -2, -1),
# #                        coef = c(0,  -8.6464, -3.2115,  -0.9482,   0.1291))
# # 
# # ggplot(mindistcoefs) +
# #   geom_point(aes(PLD, coef, color = as.factor(hr))) +
# #   geom_smooth(aes(PLD, coef), method = "lm", se = FALSE)
# # 
# # ggplot(mindistcoefs) +
# #   geom_point(aes(hr, coef, color = as.factor(PLD))) +
# #   geom_smooth(aes(hr, coef), method = "lm", se = FALSE)
# 
# e1 = emmip(biomass_model, move_combo ~ size|min_dist, cov.reduce = range) +
#   theme_bw()   +
#   labs(x = "MPA Size",
#        color = "Movement (hr/pld)") +
#   scale_color_viridis_d()
# em = emtrends(biomass_model, ~ move_combo, var = c("size"))
# em
# contrast(em, method = "pairwise")
# 
# # emmip(biomass_model, move_combo ~ min_dist, cov.reduce = range)
# # em = emtrends(biomass_model, ~ move_combo, var = "min_dist")
# # em
# # contrast(em, method = "pairwise")
# 
# e3 = emmip(biomass_model, move_combo ~ max_dist, cov.reduce = range) +
#   theme_bw()   +
#   labs(x = "Max Distance",
#        color = "Movement (hr/pld)") +
#   scale_color_viridis_d()
# em = emtrends(biomass_model, ~ move_combo, var = "max_dist")
# em
# contrast(em, method = "pairwise")
# 
# # emmip(biomass_model, move_combo ~ temp, cov.reduce = range)
# # em = emtrends(biomass_model, ~ move_combo, var = "temp")
# # em
# # contrast(em, method = "pairwise")
# 
# # e5 = emmip(biomass_model, move_combo ~ habitat_diversity, cov.reduce = range)+
# #   theme_bw()   +
# #   labs(x = "Habitat Diversity",
# #        color = "Movement (hr/pld)") +
# #   scale_color_viridis_d()
# # em = emtrends(biomass_model, ~ move_combo, var = "habitat_diversity")
# # em
# # contrast(em, method = "pairwise")
# 
# plot <- (e3) + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect")
# plot
# 
# ggsave(plot, file = paste0("biomass_model.pdf"), path = here::here("figs"), height = 10, width = 8)
# 
