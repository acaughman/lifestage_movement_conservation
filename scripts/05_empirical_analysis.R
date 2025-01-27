library(tidyverse)
library(patchwork)


# RF data ---------------------------------------------------------

rf = read_csv(here::here("data", "raw_data", "rf_data.csv")) %>% 
  mutate(pld_dist = ((pi * (1.33 * (pld)^1.3)^2)) / 100 / 2) %>% 
  filter(!is.na(homerange)) %>% 
  filter(!is.na(pld)) %>% 
  mutate(dominate = case_when(
    round(homerange, 2) < round(pld_dist, 2) ~ "PLD",
    round(homerange, 2) >= round(pld_dist, 2) ~ "Home range"
  ))

dominate_sum = rf %>% 
  group_by(dominate) %>% 
  summarize(count = n())

# Data Load and Initial Manipulation --------------------------------------

full_pisco <- read_csv(here::here("data", "raw_data", "full_pisco.csv")) %>%
  distinct() %>% 
  filter(level != "CAN") %>%
  filter(site_status == "MPA")%>% 
  filter(mpa_defacto_designation != "ref") %>%
  dplyr::select(-lat_c, -lon_c, -latitude, -longitude) %>%   #remove lat, long for site maps below
  distinct() %>% 
  group_by(date, site, affiliated_mpa, mpa_defacto_designation, zone, transect) %>% 
  mutate(total_count = sum(count),
         total_biomass = sum(weight_kg)) %>% 
  replace_na(list(total_biomass = 0, count = 0)) %>% 
  ungroup() %>% 
  dplyr::select(-level, -weight_kg, -length_to_use, -weight_g) %>% 
  distinct() %>% 
  group_by(date, affiliated_mpa, mpa_defacto_designation) %>% 
  mutate(biomass = mean(total_biomass, na.rm=TRUE),
         count = mean(total_count, na.rm = TRUE),
         n_rep = n(),
         sd_bio = sd(total_biomass, na.rm=TRUE),
         se_bio = sd_bio/sqrt(n_rep)) %>% 
  ungroup() %>% 
  dplyr::select(-site, -zone,-transect, -total_biomass, -total_count) %>% 
  distinct() %>% 
  filter(!is.na(sciname)) %>% 
  filter(!grepl("spp", sciname))

sub_pisco = full_pisco %>% 
  filter(sciname %in% c("Paralabrax clathratus", 
                        # "Medialuna californiensis", 
                        "Ophiodon elongatus",
                        # "Acanthoclinus fuscus",
                        "Phanerodon furcatus",
                        # "Sebastes auriculatus",
                        # "Sebastes melanops", 
                        # "Sebastes melanostomus", 
                        "Sebastes mystinus", 
                        # "Trachurus symmetricus",
                        # "Hypsypops rubicundus",
                        # "Sebastes serriceps",
                        "Embiotoca jacksoni")) %>% 
  mutate(movement = case_when(
    sciname == "Paralabrax clathratus" ~ "low HR, high PLD", 
    # sciname == "Medialuna californiensis" ~ "med HR, high PLD", 
    sciname == "Ophiodon elongatus" ~ "med HR, high PLD",
    # sciname == "Acanthoclinus fuscus"~ "low HR, high PLD",
    sciname == "Phanerodon furcatus" ~ "med HR, low PLD",
    # sciname == "Sebastes auriculatus" ~ "med HR, high PLD",
    # sciname == "Sebastes melanops" ~ "med HR, high PLD", 
    # sciname == "Sebastes melanostomus"~ "med HR, high PLD",
    sciname == "Sebastes mystinus" ~ "high HR, high PLD",
    # sciname == "Trachurus symmetricus" ~ "high HR, high PLD",
    # sciname == "Hypsypops rubicundus" ~ "low HR, high PLD",
    # sciname == "Sebastes serriceps"~ "low HR, high PLD",
    sciname == "Embiotoca jacksoni" ~ "low HR, low PLD"
  )) %>% 
  mutate(affiliated_mpa = fct_reorder(affiliated_mpa, min_dist, .desc = FALSE)) %>% 
  mutate(sciname = fct_relevel(sciname, c("Embiotoca jacksoni", "Paralabrax clathratus",
                                            "Phanerodon furcatus", "Ophiodon elongatus",
                                            "Sebastes mystinus"))) 

mpa_summary = full_pisco %>% 
  select(affiliated_mpa, implementation_date, settlement_mpa_total, size, min_dist, max_dist) %>% 
  distinct()

p1 <- ggplot(sub_pisco, aes(sciname, count, color = movement)) +
  geom_boxplot() +
  theme_bw() +
  scale_y_log10() +
  theme(strip.background = element_rect(fill = "transparent")) +
  scale_color_viridis_d() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  labs(y = "Count/Transect", x = "", color = "MPA")

p2 <- ggplot(sub_pisco, aes(sciname, biomass, color = movement)) +
  geom_boxplot() +
  theme_bw() +
  scale_y_log10() +
  theme(strip.background = element_rect(fill = "transparent")) +
  scale_color_viridis_d() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(y = "Biomass (kg/transect)", x = "", color = "MPA")

p1 / p2 + plot_layout(guides = "collect")

tsp <- ggplot(sub_pisco, aes(date, count, color = movement)) +
  geom_vline(aes(xintercept = ymd(2014, truncated = 2L)), color = "red", linetype = "dashed", alpha = 0.5) +
  geom_vline(aes(xintercept = ymd(2016, truncated = 2L)), color = "red", linetype = "dashed", alpha = 0.5) +
  geom_line() +
  theme_bw(base_size = 20) +
  facet_grid(affiliated_mpa ~ sciname) +
  scale_y_log10() +
  theme(strip.background = element_rect(fill = "transparent")) +
  scale_color_viridis_d(guide = "none") +
  labs(x = "", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
tsp

kelp_bass = sub_pisco %>% 
  filter(sciname == "Paralabrax clathratus") %>% 
  filter(!(affiliated_mpa %in% c("santa barbara island smr", "carrington point smr")))

size <- ggplot(kelp_bass, aes(date, count, color = as.factor(size)), linewidth = 1) +
  geom_vline(aes(xintercept = ymd(2014, truncated = 2L)), color = "red", linetype = "dashed", alpha = 0.5) +
  geom_vline(aes(xintercept = ymd(2016, truncated = 2L)), color = "red", linetype = "dashed", alpha = 0.5) +
  geom_line() +
  theme_bw(base_size = 20) +
  facet_wrap(~affiliated_mpa, scales = "free_y") +
  scale_y_log10() +
  theme(strip.background = element_rect(fill = "transparent")) +
  scale_color_viridis_d() +
  labs(x = "", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
size

average_pisco = sub_pisco %>% 
  group_by(sciname, affiliated_mpa) %>% 
  mutate(avg_count = mean(count, na.rm = TRUE))

ggplot() +
  geom_point(data = average_pisco, aes(x = sciname, y = avg_count, color = min_dist)) +
  theme_bw() +
  scale_color_viridis_c() +
  facet_wrap(~size) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# OLD ---------------------------------------------------------------------

# 
# species_num <- full_pisco %>%
#   group_by(sciname) %>%
#   summarize(count = n())
# 
# species_mpa_num <- full_pisco %>%
#   group_by(sciname, affiliated_mpa) %>%
#   summarize(count = n()) %>% 
#   filter(count > 1) %>% 
#   group_by(sciname) %>%
#   mutate(mpa_count = n()) %>% 
#   filter(mpa_count > 1)
# 
# mpa_num <- species_mpa_num %>%
#   group_by(affiliated_mpa) %>%
#   summarize(
#     count_ind = sum(count),
#     count_species = n()
#   )
# 
# sub_pisco <- full_pisco %>%
#   filter(sciname %in% species_mpa_num$sciname)
# 
# 
# # Subset to species to analyze --------------------------------------------
# 
# # Paralabrax clathratus low PLD, grow fast
# # rock fish variable recruiters, range of PLD
# # sheephead short PLD, consistent recruitement
# # red abalone? not this dataset
# 
# sci_sub = c("Paralabrax clathratus", "Semicossyphus pulcher", "Sebastes mystinus",
#             "Sebastes carnatus", "Sebastes melanostomus", "Sebastes paucispinis", 
#             "Sebastes melanops")
# 
# # "Paralabrax clathratus" HR: 0.003349, PLD 25 - 36 not in data set, found online
# # "Semicossyphus pulcher" HR:0.111954514 PLD:47.46666667
# # "Sebastes mystinus" HR: 0.017957634	PLD: 52.7057187
# # "Sebastes carnatus" HR: 0.001092107	PLD: 47.74623829
# # "Sebastes melanostomus" HR: 0.001809314	PLD: 56.80472276
# # "Sebastes paucispinis" HR: 0.047456191 PLD: 45.75724411
# # "Sebastes diploproa" HR: 0.097733981 PLD: 65.85248449 removed 
# # "Sebastes melanops" HR: 0.25	PLD: 45.04911932
# # "Sebastes pinniger" HR: 0.163563979	PLD: 55.25874663 removed
# 
# pisco = full_pisco %>% 
#   filter(sciname %in% sci_sub) %>% 
#   mutate(move_combo = case_when(
#     sciname == "Paralabrax clathratus" ~ "low / low", 
#     sciname == "Semicossyphus pulcher" ~ "high / med", 
#     sciname == "Sebastes mystinus" ~ "med / med",
#     sciname == "Sebastes carnatus" ~ "low / med", 
#     sciname == "Sebastes melanostomus" ~ "low / high", 
#     sciname == "Sebastes paucispinis" ~ "med / med", 
#     sciname == "Sebastes melanops" ~ "high / med", 
#     sciname == "Sebastes pinniger" ~ "high / high"
#   )) %>% 
#   group_by(year, sciname, affiliated_mpa) %>% 
#   mutate(count_avg = mean(count, na.rm=TRUE),
#          biomass_avg = mean(biomass, na.rm=TRUE)) %>% 
#   select(-count, -biomass, -date, -month, -day, -sd_bio, -se_bio, -n_rep) %>% 
#   ungroup() %>% 
#   distinct()
# 
# p1 <- ggplot(pisco, aes(sciname, count_avg, color = affiliated_mpa)) +
#   geom_boxplot() +
#   theme_bw() +
#   scale_y_log10() +
#   theme(strip.background = element_rect(fill = "transparent")) +
#   scale_color_viridis_d() +
#   theme(
#     axis.text.x = element_blank(),
#     axis.ticks.x = element_blank()
#   ) +
#   labs(y = "Count/Transect", x = "", color = "MPA")
# 
# p2 <- ggplot(pisco, aes(sciname, biomass_avg, color = affiliated_mpa)) +
#   geom_boxplot() +
#   theme_bw() +
#   scale_y_log10() +
#   theme(strip.background = element_rect(fill = "transparent")) +
#   scale_color_viridis_d() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
#   labs(y = "Biomass (kg/transect)", x = "", color = "MPA")
# 
# plot <- p1 / p2 + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect")
# plot
# 
# ggsave(plot, file = paste0("biomass_count_summary.pdf"), path = here::here("figs"), height = 15, width = 15)
# 
# tsp <- ggplot(pisco, aes(as.numeric(year), count_avg)) +
#   geom_vline(aes(xintercept = 2014), color = "red", linetype = "dashed", alpha = 0.5) +
#   geom_vline(aes(xintercept = 2016), color = "red", linetype = "dashed", alpha = 0.5) +
#   geom_line() +
#   theme_bw(base_size = 20) +
#   facet_grid(sciname ~ affiliated_mpa) +
#   scale_y_log10() +
#   theme(strip.background = element_rect(fill = "transparent")) +
#   scale_color_viridis_d() +
#   labs(x = "", y = "Count") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
# tsp
# 
# ggsave(tsp, file = paste0("species_mpa_timeseries.pdf"), path = here::here("figs"), height = 30, width = 30, limitsize = FALSE)
# 
# data_sum = pisco %>% 
#   group_by(sciname, year) %>% 
#   summarise(count = n()) %>% 
#   filter(!(year %in% c(2015, 2016, 2017))) %>% 
#   rename(max_year = year) %>% 
#   slice_max(n = 1, order_by = count) %>% 
#   full_join(pisco) %>% 
#   filter(year == max_year) %>% 
#   select(-max_year, -count) %>% 
#   group_by(sciname, affiliated_mpa) %>% 
#   slice_max(n = 1, order_by = year) %>% 
#   ungroup()
# 
# p1 <- ggplot(data_sum, aes(sciname, count_avg, color = affiliated_mpa)) +
#   geom_jitter(width = .1) +
#   theme_bw() +
#   scale_y_log10() +
#   theme(strip.background = element_rect(fill = "transparent")) +
#   scale_color_viridis_d() +
#   theme(
#     axis.text.x = element_blank(),
#     axis.ticks.x = element_blank()
#   ) +
#   labs(y = "Count/Transect", x = "", color = "MPA")
# 
# p2 <- ggplot(data_sum, aes(sciname, biomass_avg, color = affiliated_mpa)) +
#   geom_jitter(width = .1) +
#   theme_bw() +
#   scale_y_log10() +
#   theme(strip.background = element_rect(fill = "transparent")) +
#   scale_color_viridis_d() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
#   labs(y = "Biomass (kg/transect)", x = "", color = "MPA")
# 
# plot <- p1 / p2 + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect")
# plot
# 
# # predictors summary ------------------------------------------------------
# 
# p1 <- ggplot(data_sum, aes(size, count_avg, color = move_combo, group = sciname)) +
#   geom_jitter(alpha = 0.5) +
#   theme_bw() +
#   theme(strip.background = element_rect(fill = "transparent")) +
#   geom_smooth(method = "glm", se = FALSE, method.args = list(family = "poisson")) +
#   scale_color_viridis_d() +
#   scale_y_log10() +
#   labs(y = "count_avg", x = "MPA Size", color = "Movement (hr/pld)")
# 
# p2 <- ggplot(data_sum, aes(min_dist, count_avg, color = move_combo, group = sciname)) +
#   geom_jitter(alpha = 0.5) +
#   theme_bw() +
#   theme(strip.background = element_rect(fill = "transparent")) +
#   geom_smooth(method = "glm", se = FALSE, method.args = list(family = "poisson")) +
#   scale_color_viridis_d() +
#   scale_y_log10() +
#   labs(y = "count_avg", x = "Minimum Distance", color = "Movement (hr/pld)")
# 
# p3 <- ggplot(data_sum, aes(max_dist, count_avg, color = move_combo, group = sciname)) +
#   geom_jitter(alpha = 0.5) +
#   theme_bw() +
#   theme(strip.background = element_rect(fill = "transparent")) +
#   geom_smooth(method = "glm", se = FALSE, method.args = list(family = "poisson")) +
#   scale_color_viridis_d() +
#   scale_y_log10() +
#   labs(y = "count_avg", x = "Maximum Distance", color = "Movement (hr/pld)")
# 
# ggplot(data_sum, aes(as.factor(implementation_date), count_avg, color = move_combo)) +
#   geom_boxplot(alpha = 0.5) +
#   theme_bw() +
#   facet_wrap(~sciname, scales = "free") +
#   theme(strip.background = element_rect(fill = "transparent")) +
#   scale_color_viridis_d()
# 
# ggplot(data_sum, aes(habitat_richness, count_avg, color = move_combo)) +
#   geom_jitter(alpha = 0.5) +
#   theme_bw() +
#   facet_wrap(~sciname, scales = "free") +
#   theme(strip.background = element_rect(fill = "transparent")) +
#   geom_smooth(method = "glm", se = FALSE, method.args = list(family = "poisson")) +
#   scale_color_viridis_d()
# 
# p4 <- ggplot(data_sum, aes(habitat_diversity, count_avg, color = move_combo, group = sciname)) +
#   geom_jitter(alpha = 0.5) +
#   theme_bw() +
#   theme(strip.background = element_rect(fill = "transparent")) +
#   geom_smooth(method = "glm", se = FALSE, method.args = list(family = "poisson")) +
#   scale_color_viridis_d() +
#   scale_y_log10() +
#   labs(y = "count_avg", x = "Habitat Diversity", color = "Movement (hr/pld)")
# 
# p8 = ggplot(data_sum, aes(prop_rock, count_avg, color = move_combo, group = sciname)) +
#   geom_jitter(alpha = 0.5) +
#   theme_bw() +
#   theme(strip.background = element_rect(fill = "transparent")) +
#   geom_smooth(method = "glm", se = FALSE, method.args = list(family = "poisson")) +
#   scale_color_viridis_d() +
#   scale_y_log10() +
#   labs(y = "count_avg", x = "Proportion Rock", color = "Movement (hr/pld)")
# 
# ggplot(data_sum, aes(fishing_pressure, count_avg, color = move_combo, group = sciname)) +
#   geom_jitter(alpha = 0.5) +
#   theme_bw() +
#   theme(strip.background = element_rect(fill = "transparent")) +
#   geom_smooth(method = "glm", se = FALSE, method.args = list(family = "poisson")) +
#   scale_color_viridis_d() +
#   scale_y_log10()
# 
# ggplot(data_sum, aes(as.factor(mpa_defacto_designation), count_avg, color = move_combo)) +
#   geom_boxplot(alpha = 0.5) +
#   theme_bw() +
#   facet_wrap(~sciname, scales = "free") +
#   theme(strip.background = element_rect(fill = "transparent")) +
#   scale_color_viridis_d()
# 
# # p5 <- ggplot(data_sum, aes(temp, count_avg, color = move_combo, group = sciname)) +
# #   geom_jitter(alpha = 0.5) +
# #   theme_bw() +
# #   theme(strip.background = element_rect(fill = "transparent")) +
# #   geom_smooth(method = "glm", se = FALSE, method.args = list(family = "poisson")) +
# #   scale_color_viridis_d() +
# #   scale_y_log10() +
# #   labs(y = "count_avg", x = "Temperature", color = "Movement (hr/pld)")
# 
# # p6 <- ggplot(data_sum, aes(year, count_avg, color = move_combo, group = sciname)) +
# #   geom_jitter(alpha = 0.5) +
# #   theme_bw() +
# #   theme(strip.background = element_rect(fill = "transparent")) +
# #   geom_smooth(se = FALSE) +
# #   scale_color_viridis_d() +
# #   scale_y_log10() +
# #   labs(y = "count_avg", x = "Date", color = "Movement (hr/pld)")
# 
# p7 <- ggplot(data_sum, aes(settlement_mpa_total, count_avg, color = move_combo, group = sciname)) +
#   geom_jitter(alpha = 0.5) +
#   theme_bw() +
#   theme(strip.background = element_rect(fill = "transparent")) +
#   geom_smooth(method = "glm", se = FALSE, method.args = list(family = "poisson")) +
#   scale_color_viridis_d() +
#   scale_y_log10() +
#   labs(y = "count_avg", x = "Settlement Total", color = "Movement (hr/pld)")
# 
# plot <- (p1 + p7) / (p2 + p3) / (p4 + p8) + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect")
# plot
# 
# ggsave(plot, file = paste0("count_avg_preditors_summary.pdf"), path = here::here("figs"), height = 10, width = 12)
# 
# # predictors biomass ------------------------------------------------------
# 
# p1 <- ggplot(data_sum, aes(size, biomass_avg, color = move_combo, group = sciname)) +
#   geom_jitter(alpha = 0.5) +
#   theme_bw() +
#   theme(strip.background = element_rect(fill = "transparent")) +
#   geom_smooth(method = "lm", se = FALSE) +
#   scale_color_viridis_d() +
#   scale_y_log10() +
#   labs(y = "biomass_avg", x = "MPA Size", color = "Movement (hr/pld)")
# 
# p2 <- ggplot(data_sum, aes(min_dist, biomass_avg, color = move_combo, group = sciname)) +
#   geom_jitter(alpha = 0.5) +
#   theme_bw() +
#   theme(strip.background = element_rect(fill = "transparent")) +
#   geom_smooth(method = "lm", se = FALSE) +
#   scale_color_viridis_d() +
#   scale_y_log10() +
#   labs(y = "biomass_avg", x = "Minimum Distance", color = "Movement (hr/pld)")
# 
# p3 <- ggplot(data_sum, aes(max_dist, biomass_avg, color = move_combo, group = sciname)) +
#   geom_jitter(alpha = 0.5) +
#   theme_bw() +
#   theme(strip.background = element_rect(fill = "transparent")) +
#   geom_smooth(method = "lm", se = FALSE) +
#   scale_color_viridis_d() +
#   scale_y_log10() +
#   labs(y = "biomass_avg", x = "Maximum Distance", color = "Movement (hr/pld)")
# 
# ggplot(data_sum, aes(as.factor(implementation_date), biomass_avg, color = move_combo)) +
#   geom_boxplot(alpha = 0.5) +
#   theme_bw() +
#   facet_wrap(~sciname, scales = "free") +
#   theme(strip.background = element_rect(fill = "transparent")) +
#   # geom_smooth(method = "loess", se = FALSE) +
#   scale_y_log10() +
#   scale_color_viridis_d()
# 
# ggplot(data_sum, aes(habitat_richness, biomass_avg, color = move_combo)) +
#   geom_jitter(alpha = 0.5) +
#   theme_bw() +
#   facet_wrap(~sciname, scales = "free") +
#   theme(strip.background = element_rect(fill = "transparent")) +
#   geom_smooth(method = "lm", se = FALSE) +
#   scale_color_viridis_d()
# 
# p4 <- ggplot(data_sum, aes(habitat_diversity, biomass_avg, color = move_combo, group = sciname)) +
#   geom_jitter(alpha = 0.5) +
#   theme_bw() +
#   theme(strip.background = element_rect(fill = "transparent")) +
#   geom_smooth(method = "lm", se = FALSE) +
#   scale_color_viridis_d() +
#   scale_y_log10() +
#   labs(y = "biomass_avg", x = "Habitat Diversity", color = "Movement (hr/pld)")
# 
# p8 = ggplot(data_sum, aes(prop_rock, biomass_avg, color = move_combo, group = sciname)) +
#   geom_jitter(alpha = 0.5) +
#   theme_bw() +
#   theme(strip.background = element_rect(fill = "transparent")) +
#   geom_smooth(method = "lm", se = FALSE) +
#   scale_color_viridis_d() +
#   scale_y_log10() +
#   labs(y = "biomass_avg", x = "Proportion Rock", color = "Movement (hr/pld)")
# 
# ggplot(data_sum, aes(fishing_pressure, biomass_avg, color = move_combo)) +
#   geom_jitter(alpha = 0.5) +
#   theme_bw() +
#   facet_wrap(~sciname, scales = "free") +
#   theme(strip.background = element_rect(fill = "transparent")) +
#   geom_smooth(method = "lm", se = FALSE) +
#   scale_color_viridis_d()
# 
# ggplot(data_sum, aes(as.factor(mpa_defacto_designation), biomass_avg, color = move_combo)) +
#   geom_boxplot(alpha = 0.5) +
#   theme_bw() +
#   facet_wrap(~sciname) +
#   theme(strip.background = element_rect(fill = "transparent")) +
#   # geom_smooth(method = "loess", se = FALSE) +
#   scale_y_log10() +
#   scale_color_viridis_d()
# 
# # p5 <- ggplot(data_sum, aes(temp, biomass_avg, color = move_combo, group = sciname)) +
# #   geom_jitter(alpha = 0.5) +
# #   theme_bw() +
# #   theme(strip.background = element_rect(fill = "transparent")) +
# #   geom_smooth(method = "lm", se = FALSE) +
# #   scale_color_viridis_d() +
# #   scale_y_log10() +
# #   labs(y = "biomass_avg", x = "Temperature", color = "Movement (hr/pld)")
# # 
# # p6 <- ggplot(data_sum, aes(date, biomass_avg, color = move_combo, group = sciname)) +
# #   geom_jitter(alpha = 0.5) +
# #   theme_bw() +
# #   theme(strip.background = element_rect(fill = "transparent")) +
# #   geom_smooth(method = "lm", se = FALSE) +
# #   scale_color_viridis_d() +
# #   scale_y_log10() +
# #   labs(y = "biomass_avg", x = "Date", color = "Movement (hr/pld)")
# 
# p7 <- ggplot(data_sum, aes(settlement_mpa_total, biomass_avg, color = move_combo, group = sciname)) +
#   geom_jitter(alpha = 0.5) +
#   theme_bw() +
#   theme(strip.background = element_rect(fill = "transparent")) +
#   geom_smooth(method = "lm", se = FALSE) +
#   scale_color_viridis_d() +
#   scale_y_log10() +
#   labs(y = "biomass_avg", x = "Settlement Total", color = "Movement (hr/pld)")
# 
# plot <- (p1 + p7) / (p2 + p3) / (p4 + p8) + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect")
# plot
# 
# ggsave(plot, file = paste0("biomass_avg_preditors_summary.pdf"), path = here::here("figs"), height = 10, width = 12)
# 
# 
# 
# # Species Facet -----------------------------------------------------------
# 
# p1 <- ggplot(data_sum, aes(size, count_avg, color = move_combo, group = sciname)) +
#   geom_jitter(alpha = 0.5) +
#   theme_bw() +
#   theme(strip.background = element_rect(fill = "transparent")) +
#   geom_smooth(method = "glm", se = FALSE, method.args = list(family = "poisson")) +
#   scale_color_viridis_d() +
#   facet_wrap(~sciname, scales = "free_y") +
#   scale_y_log10() +
#   labs(y = "count_avg", x = "MPA Size", color = "Movement (hr/pld)")
# 
# p2 <- ggplot(data_sum, aes(min_dist, count_avg, color = move_combo, group = sciname)) +
#   geom_jitter(alpha = 0.5) +
#   theme_bw() +
#   theme(strip.background = element_rect(fill = "transparent")) +
#   geom_smooth(method = "glm", se = FALSE, method.args = list(family = "poisson")) +
#   scale_color_viridis_d() +
#   facet_wrap(~sciname, scales = "free_y") +
#   scale_y_log10() +
#   labs(y = "count_avg", x = "Minimum Distance", color = "Movement (hr/pld)")
# 
# p3 <- ggplot(data_sum, aes(prop_rock, count_avg, color = move_combo, group = sciname)) +
#   geom_jitter(alpha = 0.5) +
#   theme_bw() +
#   theme(strip.background = element_rect(fill = "transparent")) +
#   geom_smooth(method = "glm", se = FALSE, method.args = list(family = "poisson")) +
#   scale_color_viridis_d() +
#   facet_wrap(~sciname, scales = "free_y") +
#   scale_y_log10() +
#   labs(y = "count_avg", x = "Proportion Rock", color = "Movement (hr/pld)")
# 
# p4 <- ggplot(data_sum, aes(settlement_mpa_total, count_avg, color = move_combo, group = sciname)) +
#   geom_jitter(alpha = 0.5) +
#   theme_bw() +
#   theme(strip.background = element_rect(fill = "transparent")) +
#   geom_smooth(method = "glm", se = FALSE, method.args = list(family = "poisson")) +
#   scale_color_viridis_d() +
#   facet_wrap(~sciname, scales = "free_y") +
#   scale_y_log10() +
#   labs(y = "count_avg", x = "Settlement", color = "Movement (hr/pld)")
# 
# 
# 
# p1 <- ggplot(data_sum, aes(size, biomass_avg, color = move_combo, group = sciname)) +
#   geom_jitter(alpha = 0.5) +
#   theme_bw() +
#   theme(strip.background = element_rect(fill = "transparent")) +
#   geom_smooth(method = "glm", se = FALSE, method.args = list(family = "poisson")) +
#   scale_color_viridis_d() +
#   facet_wrap(~sciname, scales = "free_y") +
#   scale_y_log10() +
#   labs(y = "biomass_avg", x = "MPA Size", color = "Movement (hr/pld)")
# 
# p2 <- ggplot(data_sum, aes(min_dist, biomass_avg, color = move_combo, group = sciname)) +
#   geom_jitter(alpha = 0.5) +
#   theme_bw() +
#   theme(strip.background = element_rect(fill = "transparent")) +
#   geom_smooth(method = "glm", se = FALSE, method.args = list(family = "poisson")) +
#   scale_color_viridis_d() +
#   facet_wrap(~sciname, scales = "free_y") +
#   scale_y_log10() +
#   labs(y = "biomass_avg", x = "Minimum Distance", color = "Movement (hr/pld)")
# 
# p3 <- ggplot(data_sum, aes(prop_rock, biomass_avg, color = move_combo, group = sciname)) +
#   geom_jitter(alpha = 0.5) +
#   theme_bw() +
#   theme(strip.background = element_rect(fill = "transparent")) +
#   geom_smooth(method = "glm", se = FALSE, method.args = list(family = "poisson")) +
#   scale_color_viridis_d() +
#   facet_wrap(~sciname, scales = "free_y") +
#   scale_y_log10() +
#   labs(y = "biomass_avg", x = "Proportion Rock", color = "Movement (hr/pld)")
# 
# p4 <- ggplot(data_sum, aes(settlement_mpa_total, biomass_avg, color = move_combo, group = sciname)) +
#   geom_jitter(alpha = 0.5) +
#   theme_bw() +
#   theme(strip.background = element_rect(fill = "transparent")) +
#   geom_smooth(method = "glm", se = FALSE, method.args = list(family = "poisson")) +
#   scale_color_viridis_d() +
#   facet_wrap(~sciname, scales = "free_y") +
#   scale_y_log10() +
#   labs(y = "biomass_avg", x = "Settlement", color = "Movement (hr/pld)")
# 
