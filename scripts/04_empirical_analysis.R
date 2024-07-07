library(tidyverse)

pisco <- read_csv(here::here("data", "raw_data", "full_pisco.csv")) %>%
  distinct() %>% 
  filter(level != "CAN") %>%
  filter(site_status == "MPA")%>% 
  filter(mpa_defacto_designation != "ref")
  
  
  %>%
  dplyr::select(-lat_c, -lon_c, -latitude, -longitude) %>%   #remove lat, long for site maps below
  distinct() %>% 
  group_by(date, affiliated_mpa) %>% 
  mutate(temp = mean(temp, na.rm=TRUE),
         max_temp = max(temp, na.rm=TRUE)) %>% 
  ungroup() %>% 
  distinct()%>%
  mutate(magnitude_homerange = magnitude_homerange + 3) %>% 
  mutate(move_combo = paste(magnitude_homerange, "/" , month_pld)) %>% 
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
         sd = sd(total_biomass, na.rm=TRUE),
         se = sd/sqrt(n_rep)) %>% 
  ungroup() %>% 
  dplyr::select(-site, -zone,-transect, -total_biomass, -total_count) %>% 
  distinct()


species_num <- data %>%
  group_by(sciname) %>%
  summarize(count = n())

species_mpa_num <- data %>%
  group_by(sciname, affiliated_mpa) %>%
  summarize(count = n()) %>%
  filter(count > 7) %>%
  group_by(sciname) %>%
  mutate(mpa_count = n()) %>%
  left_join(scinames)

mpa_num <- species_mpa_num %>%
  group_by(affiliated_mpa) %>%
  summarize(
    count_ind = sum(count),
    count_species = n()
  )

counts <- species_mpa_num %>%
  dplyr::select(sciname, magnitude_homerange, month_pld) %>%
  distinct()

p1 <- ggplot(counts, aes(magnitude_homerange, month_pld)) +
  geom_jitter(aes(color = as.factor(magnitude_homerange), shape = as.factor(month_pld)), size = 3) +
  theme_bw() +
  labs(
    x = "Home Range Magnitude",
    y = "Month PLD",
    color = "Home Range Magnitude",
    shape = "Month PLD"
  ) +
  scale_color_viridis_d()
p1

ggsave(p1, file = paste0("species_move_summary_largeN.pdf"), path = here::here("figs"), height = 8, width = 10)

sub_data <- data %>%
  filter(sciname %in% species_mpa_num$sciname) %>% 
  distinct() %>% 
  mutate(move_combo = fct_relevel(as.factor(move_combo), c("2 / 1", "2 / 2", "1 / 0", "1 / 2", "0 / 2"))) %>% 
  distinct() %>% 
  mutate(sciname = fct_relevel(as.factor(sciname), c("Trachurus symmetricus", "Semicossyphus pulcher", 
                                                     "Sebastes melanops", "Caulolatilus princeps",
                                                     "Sebastes mystinus", "Sebastes paucispinis",
                                                     "Sebastes carnatus", "Ophiodon elongatus")))

p1 <- ggplot(sub_data, aes(sciname, count, color = move_combo)) +
  geom_boxplot() +
  theme_bw() +
  facet_wrap(~affiliated_mpa) +
  scale_y_log10() +
  theme(strip.background = element_rect(fill = "transparent")) +
  scale_color_viridis_d() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  labs(y = "Count/Transect", x = "", color = "Movement (hr/pld)")

p2 <- ggplot(sub_data, aes(sciname, biomass, color = move_combo)) +
  geom_boxplot() +
  theme_bw() +
  facet_wrap(~affiliated_mpa) +
  scale_y_log10() +
  theme(strip.background = element_rect(fill = "transparent")) +
  scale_color_viridis_d() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(y = "Biomass (kg/transect)", x = "", color = "Movement (hr/pld)")

plot <- p1 / p2 + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect")
plot

ggsave(plot, file = paste0("biomass_count_summary.pdf"), path = here::here("figs"), height = 10, width = 12)

tsp <- ggplot(sub_data, aes(date, count)) +
  geom_vline(aes(xintercept = ymd(2014, truncated = 2L)), color = "red", linetype = "dashed", alpha = 0.5) +
  geom_vline(aes(xintercept = ymd(2016, truncated = 2L)), color = "red", linetype = "dashed", alpha = 0.5) +
  geom_line() +
  theme_bw() +
  facet_grid(sciname ~ affiliated_mpa) +
  scale_y_log10() +
  theme(strip.background = element_rect(fill = "transparent")) +
  scale_color_viridis_d() +
  labs(x = "", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
tsp

ggsave(tsp, file = paste0("species_mpa_timeseries.pdf"), path = here::here("figs"), height = 12, width = 15)

temp <- ggplot(sub_data) +
  geom_vline(aes(xintercept = ymd(2014, truncated = 2L)), color = "red", linetype = "dashed", alpha = 0.5) +
  geom_vline(aes(xintercept = ymd(2016, truncated = 2L)), color = "red", linetype = "dashed", alpha = 0.5) +
  geom_line(aes(date, temp), linewidth = 1) +
  theme_bw() +
  facet_wrap(~affiliated_mpa) +
  scale_y_log10() +
  theme(strip.background = element_rect(fill = "transparent")) +
  scale_color_viridis_d() +
  labs(x = "", y = "Temperature") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  geom_smooth(aes(date, temp), method = "lm", se = FALSE, linetype = "dashed")
temp

ggsave(temp, file = paste0("temp_summary.pdf"), path = here::here("figs"), height = 10, width = 12)

# predictors summary ------------------------------------------------------

ggplot(sub_data) +
  geom_histogram(aes(count), bins = 10) +
  facet_grid(month_pld ~ magnitude_homerange, scales = "free")
group_by(affiliated_mpa) %>% 
  summarize(mean = mean(count, na.rm = TRUE),
            var = var(count, na.rm = TRUE)) %>% 
  mutate(ratio  = mean/var)

p1 <- ggplot(sub_data, aes(size, count, color = move_combo, group = sciname)) +
  geom_jitter(alpha = 0.2) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "transparent")) +
  geom_smooth(se = FALSE) +
  scale_color_viridis_d() +
  scale_y_log10() +
  labs(y = "Count", x = "MPA Size", color = "Movement (hr/pld)")

p2 <- ggplot(sub_data, aes(min_dist, count, color = move_combo, group = sciname)) +
  geom_jitter(alpha = 0.2) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "transparent")) +
  geom_smooth(se = FALSE) +
  scale_color_viridis_d() +
  scale_y_log10() +
  labs(y = "Count", x = "Minimum Distance", color = "Movement (hr/pld)")

p3 <- ggplot(sub_data, aes(max_dist, count, color = move_combo, group = sciname)) +
  geom_jitter(alpha = 0.2) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "transparent")) +
  geom_smooth(se = FALSE) +
  scale_color_viridis_d() +
  scale_y_log10() +
  labs(y = "Count", x = "Maximum Distance", color = "Movement (hr/pld)")

ggplot(sub_data, aes(as.factor(implementation_date), count, color = move_combo)) +
  geom_boxplot(alpha = 0.5) +
  theme_bw() +
  facet_wrap(~sciname, scales = "free") +
  theme(strip.background = element_rect(fill = "transparent")) +
  scale_color_viridis_d()

ggplot(sub_data, aes(habitat_richness, count, color = move_combo)) +
  geom_jitter(alpha = 0.5) +
  theme_bw() +
  facet_wrap(~sciname, scales = "free") +
  theme(strip.background = element_rect(fill = "transparent")) +
  geom_smooth(method = "glm", se = FALSE, method.args = list(family = "poisson")) +
  scale_color_viridis_d()

p4 <- ggplot(sub_data, aes(habitat_diversity, count, color = move_combo, group = sciname)) +
  geom_jitter(alpha = 0.2) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "transparent")) +
  geom_smooth(se = FALSE) +
  scale_color_viridis_d() +
  scale_y_log10() +
  labs(y = "Count", x = "Habitat Diversity", color = "Movement (hr/pld)")

p8 =ggplot(sub_data, aes(prop_rock, count, color = move_combo, group = sciname)) +
  geom_jitter(alpha = 0.2) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "transparent")) +
  geom_smooth(se = FALSE) +
  scale_color_viridis_d() +
  scale_y_log10() +
  labs(y = "Count", x = "Proportion Rock", color = "Movement (hr/pld)")

ggplot(sub_data, aes(fishing_pressure, count, color = move_combo, group = sciname)) +
  geom_jitter(alpha = 0.5) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "transparent")) +
  geom_smooth(method = "glm", se = FALSE, method.args = list(family = "poisson")) +
  scale_color_viridis_d() +
  scale_y_log10()

ggplot(sub_data, aes(as.factor(mpa_defacto_designation), count, color = move_combo)) +
  geom_boxplot(alpha = 0.5) +
  theme_bw() +
  facet_wrap(~sciname, scales = "free") +
  theme(strip.background = element_rect(fill = "transparent")) +
  scale_color_viridis_d()

p5 <- ggplot(sub_data, aes(temp, count, color = move_combo, group = sciname)) +
  geom_jitter(alpha = 0.2) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "transparent")) +
  geom_smooth(se = FALSE) +
  scale_color_viridis_d() +
  scale_y_log10() +
  labs(y = "Count", x = "Temperature", color = "Movement (hr/pld)")

p6 <- ggplot(sub_data, aes(date, count, color = move_combo, group = sciname)) +
  geom_jitter(alpha = 0.2) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "transparent")) +
  geom_smooth(se = FALSE) +
  scale_color_viridis_d() +
  scale_y_log10() +
  labs(y = "Count", x = "Date", color = "Movement (hr/pld)")

p7 <- ggplot(sub_data, aes(settlement_mpa_total, count, color = move_combo, group = sciname)) +
  geom_jitter(alpha = 0.2) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "transparent")) +
  geom_smooth(se = FALSE) +
  scale_color_viridis_d() +
  scale_y_log10() +
  labs(y = "Count", x = "Settlement Total", color = "Movement (hr/pld)")

plot <- (p6 + p5) / (p1 + p7) / (p2 + p3) / (p4 + p8) + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect")
plot

ggsave(plot, file = paste0("count_preditors_summary.pdf"), path = here::here("figs"), height = 10, width = 12)

# predictors biomass ------------------------------------------------------

ggplot(sub_data) +
  geom_histogram(aes(log(biomass)), bins = 10) +
  facet_grid(month_pld ~ magnitude_homerange, scales = "free")

p1 <- ggplot(sub_data, aes(size, biomass, color = move_combo, group = sciname)) +
  geom_jitter(alpha = 0.2) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "transparent")) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_viridis_d() +
  scale_y_log10() +
  labs(y = "Biomass", x = "MPA Size", color = "Movement (hr/pld)")

p2 <- ggplot(sub_data, aes(min_dist, biomass, color = move_combo, group = sciname)) +
  geom_jitter(alpha = 0.2) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "transparent")) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_viridis_d() +
  scale_y_log10() +
  labs(y = "Biomass", x = "Minimum Distance", color = "Movement (hr/pld)")

p3 <- ggplot(sub_data, aes(max_dist, biomass, color = move_combo, group = sciname)) +
  geom_jitter(alpha = 0.2) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "transparent")) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_viridis_d() +
  scale_y_log10() +
  labs(y = "Biomass", x = "Maximum Distance", color = "Movement (hr/pld)")

ggplot(sub_data, aes(as.factor(implementation_date), biomass, color = move_combo)) +
  geom_boxplot(alpha = 0.5) +
  theme_bw() +
  facet_wrap(~sciname, scales = "free") +
  theme(strip.background = element_rect(fill = "transparent")) +
  # geom_smooth(method = "loess", se = FALSE) +
  scale_y_log10() +
  scale_color_viridis_d()

ggplot(sub_data, aes(habitat_richness, biomass, color = move_combo)) +
  geom_jitter(alpha = 0.5) +
  theme_bw() +
  facet_wrap(~sciname, scales = "free") +
  theme(strip.background = element_rect(fill = "transparent")) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_viridis_d()

p4 <- ggplot(sub_data, aes(habitat_diversity, biomass, color = move_combo, group = sciname)) +
  geom_jitter(alpha = 0.2) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "transparent")) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_viridis_d() +
  scale_y_log10() +
  labs(y = "Biomass", x = "Habitat Diversity", color = "Movement (hr/pld)")

p8 = ggplot(sub_data, aes(prop_rock, biomass, color = move_combo, group = sciname)) +
  geom_jitter(alpha = 0.2) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "transparent")) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_viridis_d() +
  scale_y_log10() +
  labs(y = "Biomass", x = "Proportion Rock", color = "Movement (hr/pld)")

ggplot(sub_data, aes(fishing_pressure, biomass, color = move_combo)) +
  geom_jitter(alpha = 0.5) +
  theme_bw() +
  facet_wrap(~sciname, scales = "free") +
  theme(strip.background = element_rect(fill = "transparent")) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_viridis_d()

ggplot(sub_data, aes(as.factor(mpa_defacto_designation), biomass, color = move_combo)) +
  geom_boxplot(alpha = 0.5) +
  theme_bw() +
  facet_wrap(~sciname) +
  theme(strip.background = element_rect(fill = "transparent")) +
  # geom_smooth(method = "loess", se = FALSE) +
  scale_y_log10() +
  scale_color_viridis_d()

p5 <- ggplot(sub_data, aes(temp, biomass, color = move_combo, group = sciname)) +
  geom_jitter(alpha = 0.2) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "transparent")) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_viridis_d() +
  scale_y_log10() +
  labs(y = "Biomass", x = "Temperature", color = "Movement (hr/pld)")

p6 <- ggplot(sub_data, aes(date, biomass, color = move_combo, group = sciname)) +
  geom_jitter(alpha = 0.2) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "transparent")) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_viridis_d() +
  scale_y_log10() +
  labs(y = "Biomass", x = "Date", color = "Movement (hr/pld)")

p7 <- ggplot(sub_data, aes(settlement_mpa_total, biomass, color = move_combo, group = sciname)) +
  geom_jitter(alpha = 0.2) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "transparent")) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_viridis_d() +
  scale_y_log10() +
  labs(y = "Biomass", x = "Settlement Total", color = "Movement (hr/pld)")

plot <- (p6 + p5) / (p1 + p7) / (p2 + p3) / (p4 + p8) + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect")
plot

ggsave(plot, file = paste0("biomass_preditors_summary.pdf"), path = here::here("figs"), height = 10, width = 12)
