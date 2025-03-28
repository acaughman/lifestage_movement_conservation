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

percent_dominate_adult = dominate_sum$count[1] / sum(dominate_sum$count) * 100

# Data Load and Initial Manipulation --------------------------------------

full_pisco <- read_csv(here::here("data", "raw_data", "full_pisco.csv")) %>%
  distinct() %>% 
  filter(level != "CAN") %>% 
  dplyr::select(-lat_c, -lon_c, -latitude, -longitude) %>%   #remove lat, long for site maps below
  distinct() %>% 
  # get average per transect in each MPA and ref
  group_by(year, affiliated_mpa, sciname, site_status) %>% 
  mutate(count = mean(count, na.rm=TRUE),
         biomass = mean(weight_kg, na.rm=TRUE)) %>% 
  replace_na(list(biomass_per = 0, count_per = 0)) %>% 
  ungroup()%>% 
  dplyr::select(-weight_kg, -length_to_use, -weight_g, -date, -day, -month,-zone,-transect, -level, -site , -region4, -temp) %>% 
  distinct() %>% 
  filter(!is.na(sciname))%>% 
  filter(!grepl("spp", sciname)) %>% 
  #remove data from within 2 years of MPA implementation
  mutate(imp_year = year(mdy(implementation_date))) %>% 
  filter(year > imp_year + 2)%>% 
  #remove outliers within years
  group_by(sciname) %>% 
  mutate(q1 = quantile(count, probs = 0.25),
         q3 = quantile(count, probs = 0.75)) %>% 
  filter(count < q3 + (2.5 * (q3 - q1))) %>% 
  filter(count > q1 - (2.5 * (q3 - q1))) %>% 
  mutate(species_count = n()) %>% 
  filter(species_count > 10) %>% 
  select(-species_count, -q1, -q3, -imp_year, -implementation_date) %>% 
  # calcluate year average
  group_by(affiliated_mpa, sciname, site_status) %>% 
  mutate(avg_count = mean(count, na.rm=TRUE),
         avg_biomass = mean(biomass, na.rm=TRUE)) 
  

pisco_rf = left_join(rf, full_pisco) %>% 
  filter(!is.na(affiliated_mpa))

sub_pisco = full_pisco %>% 
  filter(sciname %in% c("Paralabrax clathratus", 
                        "Medialuna californiensis",
                        "Ophiodon elongatus",
                        "Acanthoclinus fuscus",
                        "Phanerodon furcatus",
                        "Sebastes auriculatus",
                        "Sebastes melanops",
                        "Sebastes melanostomus",
                        "Sebastes mystinus", 
                        "Trachurus symmetricus",
                        "Hypsypops rubicundus",
                        "Sebastes serriceps",
                        "Embiotoca jacksoni")) %>% 
  full_join(pisco_rf) %>% 
  filter(target_status == "Targeted") %>% 
  mutate(hr_cat = case_when(
    magnitude_homerange < 0 ~ "low HR",
    magnitude_homerange > 1 ~ "high HR",
    !is.na(magnitude_homerange) ~ "med HR"
  )) %>% 
  mutate(pld_cat = case_when(
    month_pld == 0 ~ "low PLD",
    month_pld > 0 ~ "high PLD"
  )) %>% 
  mutate(movement = case_when(
    sciname == "Paralabrax clathratus" ~ "low HR, high PLD",
    sciname == "Medialuna californiensis" ~ "med HR, high PLD",
    sciname == "Ophiodon elongatus" ~ "med HR, high PLD",
    sciname == "Acanthoclinus fuscus"~ "low HR, high PLD",
    sciname == "Phanerodon furcatus" ~ "med HR, low PLD",
    sciname == "Sebastes auriculatus" ~ "med HR, high PLD",
    sciname == "Sebastes melanops" ~ "med HR, high PLD",
    sciname == "Sebastes melanostomus"~ "med HR, high PLD",
    sciname == "Sebastes mystinus" ~ "med HR, high PLD",
    sciname == "Trachurus symmetricus" ~ "high HR, high PLD",
    sciname == "Hypsypops rubicundus" ~ "low HR, high PLD",
    sciname == "Sebastes serriceps"~ "low HR, high PLD",
    sciname == "Embiotoca jacksoni" ~ "low HR, low PLD",
    !is.na(hr_cat) & !is.na(pld_cat) ~ paste0(hr_cat, ", ", pld_cat)
  )) %>% 
  filter(!is.na(movement)) %>% 
  mutate(affiliated_mpa = fct_reorder(affiliated_mpa, min_dist, .desc = FALSE)) %>% 
  mutate(mpa_designation = case_when(
    mpa_defacto_designation == "ref" ~ "reference",
    TRUE ~ "MPA"
  )) %>% 
  dplyr::select(-mpa_defacto_designation, -mpa_state_designation) %>% 
  filter(!(movement %in% c("high HR, low PLD", "med HR, high PLD"))) %>% 
  mutate(movement = fct_relevel(movement, "low HR, low PLD", "low HR, high PLD", "med HR, low PLD", "med HR, high PLD", "high HR, high PLD"))# %>% 
  # filter(sciname != "Scomber japonicus")

mpa_sites = sub_pisco %>%
  filter(mpa_designation == "MPA") %>%
  distinct() %>%
  ungroup() %>% 
  rename(mpa_count = avg_count) %>%
  dplyr::select(-mpa_designation, -biomass, -year, -count, - avg_biomass, -site_status) %>% 
  distinct()
ref_sites = sub_pisco %>%
  filter(mpa_designation == "reference") %>%
  distinct() %>%
  ungroup() %>% 
  rename(ref_count = avg_count) %>%
  dplyr::select(-mpa_designation, -biomass, -year, -count, -avg_biomass, -site_status) %>%
  distinct()

wider_pisco = full_join(mpa_sites, ref_sites) %>%
  mutate(in_out = log(mpa_count / ref_count)) %>% 
  filter(!is.na(in_out)) %>% 
  select(sciname, affiliated_mpa, in_out)

mpa_summary = full_pisco %>% 
  ungroup() %>% 
  dplyr::select(affiliated_mpa, size, min_dist, max_dist) %>% 
  distinct() %>% 
  mutate(larvae_spacing = case_when(
   min_dist > 192 ~ "not close enough",
   min_dist <= 192 & min_dist >32 ~ "close enough for high PLD",
   min_dist <= 32  & min_dist > 8 ~ "close enough for high/medium PLD",
   min_dist <= 8 ~ "close enough for all"
  )) %>% 
  mutate(adult_spacing = case_when(
    min_dist > 32 ~ "not close enough",
    min_dist <=32 & min_dist > 8 ~ "close enough for high movement",
    min_dist <= 8  & min_dist > 0.5 ~ "close enough for high/medium movement",
    min_dist <= 0.5 ~ "close enough for all"
  )) %>% 
  mutate(larval_size = case_when(
    size < 8 ~ "not large enough",
    size >= 8 & size < 32 ~ "large enough for low PLD",
    size >=32 & size < 192 ~ "large enough for low/medium PLD",
    size >= 192 ~ "large enough for all"
  )) %>% 
  mutate(adult_size = case_when( #using 2 x home range rule
    size < 0.5 * 2 ~ "not large enough",
    size >= 0.5 * 2 & size < 8 * 2 ~ "large enough for low movement",
    size >= 8 * 2 & size < 32 * 2 ~ "large enough for low/medium movement",
    size >= 32 * 2 ~ "large enough for all"
  ))

sub_pisco = list(sub_pisco, mpa_summary, wider_pisco) %>% 
  reduce(full_join)

sub_pisco %>% 
  ungroup() %>% 
  select(sciname, movement) %>% 
  distinct() %>% 
  group_by(movement) %>% 
  summarize(n = n())

ggplot(sub_pisco %>% filter(site_status == "MPA")) +
  geom_line(aes(year, count, color = movement)) +
  theme_bw() +
  facet_wrap(~sciname)


# count figs --------------------------------------------------------------

p1 <- ggplot(sub_pisco %>% filter(site_status == "MPA"), aes(movement, count, color = movement)) +
  geom_boxplot() +
  theme_bw() +
  scale_y_log10() +
  theme(strip.background = element_rect(fill = "transparent")) +
  scale_color_viridis_d() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  labs(y = "Count/Transect", x = "", color = "MPA") +
  facet_wrap(~affiliated_mpa)
p1

p1 <- ggplot(sub_pisco %>% filter(site_status == "MPA"), aes(sciname, count, color = movement)) +
  geom_boxplot() +
  theme_bw() +
  scale_y_log10() +
  theme(strip.background = element_rect(fill = "transparent")) +
  scale_color_viridis_d() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  labs(y = "Count/Transect", x = "", color = "MPA") +
  facet_wrap(~affiliated_mpa)
p1

p2 <- ggplot(sub_pisco %>% filter(site_status == "MPA"), aes(size, count, color = movement)) +
  geom_vline(aes(xintercept = 0.5 * 2), color = "red", linetype = "dashed")+
  geom_vline(aes(xintercept = 8 * 2), color = "red", linetype = "dashed")+
  geom_vline(aes(xintercept = 32 * 2), color = "red", linetype = "dashed")+
  # geom_line() +
  geom_smooth(method= "lm") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "transparent")) +
  scale_color_viridis_d(end = 0.9) +
  facet_wrap(~movement, scales = "free_y") +
  labs(y = "Count/Transect", x = "", color = "MPA")
p2

ggsave(p2, filename = here::here("figs", "empirical_count_size.pdf"), height = 12, width = 12)

p2 <- ggplot(sub_pisco %>% filter(site_status == "MPA"), aes(min_dist, count, color = movement)) +
  geom_vline(aes(xintercept = 0.5), color = "red", linetype = "dashed")+
  geom_vline(aes(xintercept = 8), color = "red", linetype = "dashed")+
  geom_vline(aes(xintercept = 32), color = "red", linetype = "dashed")+
  # geom_line() +
  geom_smooth(method= "lm") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "transparent")) +
  scale_color_viridis_d(end = 0.9) +
  facet_wrap(~movement, scales = "free_y") +
  labs(y = "Count/Transect", x = "", color = "MPA")
p2

ggsave(p2, filename = here::here("figs", "empirical_cout_space.pdf"), height = 12, width = 12)


# in/out figs --------------------------------------------------------------

p1 <- ggplot(sub_pisco %>% filter(site_status == "MPA"), aes(movement, in_out, color = movement)) +
  geom_boxplot() +
  theme_bw() +
  # scale_y_log10() +
  theme(strip.background = element_rect(fill = "transparent")) +
  scale_color_viridis_d() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  labs(y = "Count/Transect", x = "", color = "MPA") +
  facet_wrap(~affiliated_mpa)
p1

p1 <- ggplot(sub_pisco %>% filter(site_status == "MPA"), aes(sciname, in_out, color = movement)) +
  geom_boxplot() +
  theme_bw() +
  # scale_y_log10() +
  theme(strip.background = element_rect(fill = "transparent")) +
  scale_color_viridis_d() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  labs(y = "Count/Transect", x = "", color = "MPA") +
  facet_wrap(~affiliated_mpa)
p1

p2 <- ggplot(sub_pisco %>% filter(site_status == "MPA"), aes(size, in_out, color = movement)) +
  geom_vline(aes(xintercept = 0.5 * 2), color = "red", linetype = "dashed")+
  geom_vline(aes(xintercept = 8 * 2), color = "red", linetype = "dashed")+
  geom_vline(aes(xintercept = 32 * 2), color = "red", linetype = "dashed")+
  # geom_line() +
  geom_smooth(method = "lm") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "transparent")) +
  scale_color_viridis_d(end = 0.9) +
  facet_wrap(~movement, scales = "free_y") +
  labs(y = "Count/Transect", x = "", color = "MPA")
p2

ggsave(p2, filename = here::here("figs", "empirical_inout_size.pdf"), height = 12, width = 12)

p2 <- ggplot(sub_pisco %>% filter(site_status == "MPA"), aes(min_dist, in_out, color = movement)) +
  geom_vline(aes(xintercept = 0.5), color = "red", linetype = "dashed")+
  geom_vline(aes(xintercept = 8), color = "red", linetype = "dashed")+
  geom_vline(aes(xintercept = 32), color = "red", linetype = "dashed")+
  # geom_line() +
  geom_smooth(method= "lm") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "transparent")) +
  scale_color_viridis_d(end = 0.9) +
  facet_wrap(~movement, scales = "free_y") +
  labs(y = "Count/Transect", x = "", color = "MPA")
p2

ggsave(p2, filename = here::here("figs", "empirical_inout_space.pdf"), height = 12, width = 12)
