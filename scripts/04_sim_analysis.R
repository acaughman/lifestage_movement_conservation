library(tidyverse)
library(patchwork)

# Process Data ------------------------------------------------------------

output <- read_csv(here::here("data", "processed_data", "model_results.csv"))
connect <- read_csv(here::here("data", "processed_data", "connectivity_results.csv"))

mpa <- output %>%
  filter(mpa != "Non-MPA") %>%
  group_by(mpa, mpa_size, mpa_spacing, larval, adult, generation, age) %>%
  summarize(mean_pop = mean(pop, na.rm = TRUE)) %>%
  filter(mpa == "MPA 1") %>%
  mutate(adult_cat = case_when(
    adult %in% c(1, 2) ~ "low",
    adult %in% c(4, 8) ~ "medium",
    adult %in% c(16, 32) ~ "high"
  )) %>%
  mutate(larval_cat = case_when(
    larval %in% c(4, 2) ~ "low",
    larval %in% c(16, 8) ~ "medium",
    larval %in% c(190, 32) ~ "high"
  )) %>%
  mutate(
    movement = c(paste0(adult, " / ", larval)),
    move_cat = c(paste0(adult_cat, " / ", larval_cat)),
    move_ratio = adult / larval
  ) %>%
  mutate(ratio_cat = case_when(
    move_ratio < 1 ~ "Larval Greater",
    move_ratio == 1 ~ "Equal",
    move_ratio > 1 ~ "Adult Greater"
  ))

rm(output)

mpa <- mpa %>%
  mutate(movement = fct_relevel(movement, c(
    "1 / 2", "2 / 2", "4 / 2", "8 / 2", "16 / 2", "32 / 2",
    "1 / 4", "1 / 8", "1 / 16", "1 / 32", "1 / 190",
    "2 / 4", "4 / 4", "8 / 4", "16 / 4", "32 / 4",
    "2 / 8", "2 / 16", "2 / 32", "2 / 190",
    "4 / 8", "8 / 8", "16 / 8", "32 / 8",
    "4 / 16", "4 / 32", "4 / 190",
    "8 / 16", "16 / 16", "32 / 16",
    "8 / 32", "8 / 190",
    "16 / 32", "32 / 32",
    "16 / 190", "32 / 190"
  )))

connect <- connect %>%
  mutate(
    adult_fished_c_1 = adult_RS_fished + adult_IS_fished,
    adult_mpa_c_1 = adult_RS_mpa + adult_IS_mpa,
    larvae_fished_c_1 = larvae_RS_fished + larvae_IS_fished,
    larvae_mpa_c_1 = larvae_RS_mpa + larvae_IS_mpa,
    adult_c_2 = adult_SR + adult_II,
    larvae_c_2 = larvae_SR + larvae_II
  ) %>%
  mutate(
    fished_c1 = adult_fished_c_1 + larvae_fished_c_1,
    mpa_c1 = adult_mpa_c_1 + larvae_mpa_c_1,
    c2 = adult_c_2 + larvae_c_2,
    relative_mpa_c1 = adult_mpa_c_1 / larvae_mpa_c_1,
    relative_fished_c1 = adult_fished_c_1 / larvae_fished_c_1,
    relative_c2 = adult_c_2 / larvae_c_2
  ) %>%
  mutate(adult_cat = case_when(
    adult %in% c(1, 2) ~ "low",
    adult %in% c(4, 8) ~ "medium",
    adult %in% c(16, 32) ~ "high"
  )) %>%
  mutate(larval_cat = case_when(
    larval %in% c(4, 2) ~ "low",
    larval %in% c(16, 8) ~ "medium",
    larval %in% c(190, 32) ~ "high"
  )) %>%
  mutate(
    movement = c(paste0(adult, " / ", larval)),
    move_cat = c(paste0(adult_cat, " / ", larval_cat)),
    move_ratio = adult / larval
  ) %>%
  mutate(movement = fct_relevel(movement, c(
    "1 / 2", "2 / 2", "4 / 2", "8 / 2", "16 / 2", "32 / 2",
    "1 / 4", "1 / 8", "1 / 16", "1 / 32", "1 / 190",
    "2 / 4", "4 / 4", "8 / 4", "16 / 4", "32 / 4",
    "2 / 8", "2 / 16", "2 / 32", "2 / 190",
    "4 / 8", "8 / 8", "16 / 8", "32 / 8",
    "4 / 16", "4 / 32", "4 / 190",
    "8 / 16", "16 / 16", "32 / 16",
    "8 / 32", "8 / 190",
    "16 / 32", "32 / 32",
    "16 / 190", "32 / 190"
  ))) %>%
  mutate(ratio_cat = case_when(
    move_ratio < 1 ~ "Larval Greater",
    move_ratio == 1 ~ "Equal",
    move_ratio > 1 ~ "Adult Greater"
  ))

# Figures -----------------------------------------------------------------

p1 <- ggplot(mpa %>% filter(age == "adult") %>% filter(generation == 90)) +
  geom_point(aes(mpa_spacing, mean_pop, color = as.factor(larval), shape = as.factor(adult))) +
  geom_line(aes(mpa_spacing, mean_pop, color = as.factor(larval), linetype = as.factor(adult), group = movement), linewidth = 0.8) +
  theme_bw() +
  facet_wrap(~mpa_size, nrow = 1, scales = "free_x") +
  labs(
    x = "MPA Spacing",
    y = "Average Population Size in MPA",
    color = "Larval Movement",
    shape = "Adult Movement",
    linetype = "Adult Movement"
  ) +
  scale_color_viridis_d()

ggsave(p1, path = here::here("figs"), file = paste0("mpa_spacing.pdf"), height = 8, width = 12, limitsize = FALSE)

p2 <- ggplot(mpa %>% filter(age == "adult") %>% filter(generation == 90)) +
  geom_point(aes(mpa_size, mean_pop, color = as.factor(larval), shape = as.factor(adult))) +
  geom_line(aes(mpa_size, mean_pop, color = as.factor(larval), linetype = as.factor(adult), group = movement), linewidth = 0.8) +
  theme_bw() +
  facet_wrap(~mpa_spacing, nrow = 1, scales = "free_x") +
  labs(
    x = "MPA Size",
    y = "Average Population Size in MPA",
    color = "Larval Movement",
    shape = "Adult Movement",
    linetype = "Adult Movement"
  ) +
  scale_color_viridis_d()

ggsave(p2, path = here::here("figs"), file = paste0("mpa_size.pdf"), height = 8, width = 12, limitsize = FALSE)

p1 <- ggplot(connect) +
  geom_point(aes(adult, adult_RS_fished), color = "black", size = 2, alpha = 0.1) +
  geom_point(aes(adult, adult_RS_mpa, color = as.factor(mpa_spacing), shape = as.factor(mpa_size))) +
  scale_color_viridis_d() +
  theme_bw() +
  labs(
    x = "Adult Movement",
    y = "Retention Strength",
    color = "MPA Spacing",
    shape = "MPA Size"
  )

p2 <- ggplot(connect) +
  geom_point(aes(larval, larvae_RS_fished), color = "black", size = 2, alpha = 0.1) +
  geom_point(aes(larval, larvae_RS_mpa, color = as.factor(mpa_spacing), shape = as.factor(mpa_size))) +
  scale_color_viridis_d() +
  theme_bw() +
  labs(
    x = "Larval Movement",
    y = "Retention Strength",
    color = "MPA Spacing",
    shape = "MPA Size"
  )

p3 <- ggplot(connect) +
  geom_point(aes(adult, adult_IS_fished), color = "black", size = 2, alpha = 0.1) +
  geom_point(aes(adult, adult_IS_mpa, color = as.factor(mpa_spacing), shape = as.factor(mpa_size))) +
  scale_color_viridis_d() +
  theme_bw() +
  labs(
    x = "Adult Movement",
    y = "Import Strength",
    color = "MPA Spacing",
    shape = "MPA Size"
  )

p4 <- ggplot(connect) +
  geom_point(aes(larval, larvae_IS_fished), color = "black", size = 2, alpha = 0.1) +
  geom_point(aes(larval, larvae_IS_mpa, color = as.factor(mpa_spacing), shape = as.factor(mpa_size))) +
  scale_color_viridis_d() +
  theme_bw() +
  labs(
    x = "Larval Movement",
    y = "Import Strength",
    color = "MPA Spacing",
    shape = "MPA Size"
  )

plot <- (p1 + p2) / (p3 + p4) + plot_annotation(tag_level = "A") + plot_layout(guides = "collect")

ggsave(plot, path = here::here("figs"), file = paste0("absolute_connectivity.pdf"), height = 8, width = 12, limitsize = FALSE)

p1 <- ggplot(connect %>% filter(mpa_spacing !=0)) +
  geom_point(aes(larvae_IS_fished, adult_IS_fished, color = ratio_cat)) +
  geom_abline() +
  scale_color_viridis_d() +
  facet_wrap(mpa_size~mpa_spacing, scales = "free", ncol = 4)+
  theme_bw() +
  labs(
    x = "Larval Import Stregnth",
    y = "Adult Import Stregnth",
    color = "Adult / Larval Movement",
    title = "Fished"
  ) +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(face = "bold"))

p2 <- ggplot(connect %>% filter(mpa_spacing !=0)) +
  geom_point(aes(larvae_IS_mpa, adult_IS_mpa, color = ratio_cat)) +
  geom_abline() +
  scale_color_viridis_d() +
  facet_wrap(mpa_size~mpa_spacing, scales = "free", ncol = 4)+
  theme_bw() +
  labs(
    x = "Larval Import Stregnth",
    y = "Adult Import Stregnth",
    color = "Adult / Larval Movement",
    title = "MPA"
  )+
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(face = "bold"))

plot <- (p1 + p2) + plot_annotation(tag_level = "A") + plot_layout(guides = "collect")

ggsave(plot, path = here::here("figs"), file = paste0("import_strength.pdf"), height = 8, width = 20, limitsize = FALSE)

p1 <- ggplot(connect) +
  geom_point(aes(adult, adult_ID_fished), color = "black", size = 2, alpha = 0.1) +
  geom_point(aes(adult, adult_ID_mpa, color = as.factor(mpa_spacing), shape = as.factor(mpa_size))) +
  scale_color_viridis_d() +
  theme_bw() +
  labs(
    x = "Adult Movement",
    y = "Import Diversity",
    color = "MPA Spacing",
    shape = "MPA Size"
  )

p2 <- ggplot(connect) +
  geom_point(aes(larval, larvae_ID_fished), color = "black", size = 2, alpha = 0.1) +
  geom_point(aes(larval, larvae_ID_mpa, color = as.factor(mpa_spacing), shape = as.factor(mpa_size))) +
  scale_color_viridis_d() +
  theme_bw() +
  labs(
    x = "Larval Movement",
    y = "Import Diversity",
    color = "MPA Spacing",
    shape = "MPA Size"
  )

plot <- (p1 + p2) + plot_annotation(tag_level = "A") + plot_layout(guides = "collect")

ggsave(plot, path = here::here("figs"), file = paste0("import_diversity.pdf"), height = 8, width = 12, limitsize = FALSE)


p1 <- ggplot(connect) +
  geom_point(aes(mpa_size, mpa_c1, color = move_cat)) +
  theme_bw() +
  scale_color_viridis_d() +
  geom_smooth(aes(mpa_size, adult_mpa_c_1, linetype = as.factor(adult)), se = FALSE, alpha = 0.3, color = "black", linewidth = 0.4) +
  labs(
    x = "MPA Size",
    y = "Absolute Settlers",
    color = "Movement",
    linetype = "Adult Movement",
    title = "Adults"
  )

p2 <- ggplot(connect) +
  geom_point(aes(mpa_spacing, mpa_c1, color = move_cat)) +
  theme_bw() +
  scale_color_viridis_d() +
  geom_smooth(aes(mpa_spacing, adult_mpa_c_1, linetype = as.factor(adult)), se = FALSE, alpha = 0.3, color = "black", linewidth = 0.4) +
  labs(
    x = "MPA Spacing",
    y = "Absolute Settlers",
    color = "Movement",
    linetype = "Adult Movement"
  )

p3 <- ggplot(connect) +
  geom_point(aes(mpa_size, mpa_c1, color = move_cat)) +
  theme_bw() +
  scale_color_viridis_d() +
  geom_smooth(aes(mpa_size, larvae_mpa_c_1, linetype = as.factor(larval)), se = FALSE, alpha = 0.3, color = "black", linewidth = 0.4) +
  labs(
    x = "MPA Size",
    y = "Absolute Settlers",
    color = "Movement",
    linetype = "Larval Movement",
    title = "Larvae"
  )

p4 <- ggplot(connect) +
  geom_point(aes(mpa_spacing, mpa_c1, color = move_cat)) +
  theme_bw() +
  scale_color_viridis_d() +
  geom_smooth(aes(mpa_spacing, larvae_mpa_c_1, linetype = as.factor(larval)), se = FALSE, alpha = 0.3, color = "black", linewidth = 0.4) +
  labs(
    x = "MPA Spacing",
    y = "Absolute Settlers",
    color = "Movement",
    linetype = "Larval Movement"
  )

plot <- (p1 + p2) / (p3 + p4) + plot_annotation(tag_level = "A") + plot_layout(guides = "collect")

ggsave(plot, path = here::here("figs"), file = paste0("adultvslarval_connectivity.pdf"), height = 8, width = 12, limitsize = FALSE)

p3 <- ggplot(connect) +
  geom_hline(aes(yintercept = 1), color = "red", linetype = "dashed", linewidth = 0.4) +
  geom_point(aes(larval, relative_mpa_c1, color = as.factor(adult))) +
  theme_bw() +
  scale_color_viridis_d() +
  labs(
    x = "Larval Movement",
    y = "Relative Adult / Larval Settlers",
    color = "Adult Movement",
    linetype = "Adult Movement",
    title = "Realized MPA Connectivity"
  ) +
  geom_smooth(aes(larval, relative_mpa_c1, linetype = as.factor(adult)), se = FALSE, alpha = 0.3, color = "black", linewidth = 0.4) +
  scale_y_log10()

p4 <- ggplot(connect) +
  geom_hline(aes(yintercept = 1), color = "red", linetype = "dashed", linewidth = 0.4) +
  geom_point(aes(adult, relative_mpa_c1, color = as.factor(larval))) +
  theme_bw() +
  scale_color_viridis_d() +
  labs(
    x = "Adult Movement",
    y = "Relative Adult / Larval Settlers",
    color = "Larval Movement",
    linetype = "Larval Movement"
  ) +
  geom_smooth(aes(adult, relative_mpa_c1, linetype = as.factor(larval)), se = FALSE, alpha = 0.3, color = "black", linewidth = 0.4) +
  scale_y_log10()

p5 <- ggplot(connect) +
  geom_hline(aes(yintercept = 1), color = "red", linetype = "dashed", linewidth = 0.4) +
  geom_point(aes(larval, relative_fished_c1, color = as.factor(adult))) +
  theme_bw() +
  scale_color_viridis_d() +
  labs(
    x = "Larval Movement",
    y = "Relative Adult / Larval Settlers",
    color = "Adult Movement",
    linetype = "Adult Movement",
    title = "Realized Fished Connectivity"
  ) +
  geom_smooth(aes(larval, relative_fished_c1, linetype = as.factor(adult)), se = FALSE, alpha = 0.3, color = "black", linewidth = 0.4) +
  scale_y_log10()

p6 <- ggplot(connect) +
  geom_hline(aes(yintercept = 1), color = "red", linetype = "dashed", linewidth = 0.4) +
  geom_point(aes(adult, relative_fished_c1, color = as.factor(larval))) +
  theme_bw() +
  scale_color_viridis_d() +
  labs(
    x = "Adult Movement",
    y = "Relative Adult / Larval Settlers",
    color = "Larval Movement",
    linetype = "Larval Movement"
  ) +
  geom_smooth(aes(adult, relative_fished_c1, linetype = as.factor(larval)), se = FALSE, alpha = 0.3, color = "black", linewidth = 0.4) +
  scale_y_log10()

p1 <- ggplot(connect) +
  geom_hline(aes(yintercept = 1), color = "red", linetype = "dashed", linewidth = 0.4) +
  geom_point(aes(larval, relative_c2, color = as.factor(adult))) +
  theme_bw() +
  scale_color_viridis_d() +
  labs(
    x = "Larval Movement",
    y = "Relative Adult / Larval Settlers",
    color = "Adult Movement",
    title = "Theorectical Connectivity"
  ) +
  scale_y_log10() +
  theme(legend.position = "none")

p2 <- ggplot(connect) +
  geom_hline(aes(yintercept = 1), color = "red", linetype = "dashed", linewidth = 0.4) +
  geom_point(aes(adult, relative_c2, color = as.factor(larval))) +
  theme_bw() +
  scale_color_viridis_d() +
  labs(
    x = "Adult Movement",
    y = "Relative Adult / Larval Settlers",
    color = "Larval Movement"
  ) +
  scale_y_log10() +
  theme(legend.position = "none")

plot <- (p1 + p2) / (p5 + p6) / (p3 + p4) + plot_annotation(tag_level = "A") + plot_layout(guides = "collect")

ggsave(plot, path = here::here("figs"), file = paste0("relative_connectivity.pdf"), height = 12, width = 12, limitsize = FALSE)


# ESA ---------------------------------------------------------------------

library(tidyverse)
library(patchwork)

# Process Data ------------------------------------------------------------

output <- read_csv(here::here("data", "processed_data", "model_results.csv"))

mpa <- output %>%
  filter(mpa != "Non-MPA") %>%
  group_by(mpa, mpa_size, mpa_spacing, larval, adult, generation, age) %>%
  summarize(mean_pop = mean(pop, na.rm = TRUE)) %>%
  filter(mpa == "MPA 1") %>%
  mutate(adult_cat = case_when(
    adult %in% c(1, 2) ~ "low",
    adult %in% c(4, 8) ~ "medium",
    adult %in% c(16, 32) ~ "high"
  )) %>%
  mutate(larval_cat = case_when(
    larval %in% c(4, 2) ~ "low",
    larval %in% c(16, 8) ~ "medium",
    larval %in% c(190, 32) ~ "high"
  )) %>%
  mutate(
    movement = c(paste0(adult, " / ", larval)),
    move_cat = c(paste0(adult_cat, " / ", larval_cat)),
    move_ratio = adult / larval
  ) %>%
  mutate(ratio_cat = case_when(
    move_ratio < 1 ~ "Larval Greater",
    move_ratio == 1 ~ "Equal",
    move_ratio > 1 ~ "Adult Greater"
  ))

rm(output)

mpa <- mpa %>%
  mutate(movement = fct_relevel(movement, c(
    "1 / 2", "2 / 2", "4 / 2", "8 / 2", "16 / 2", "32 / 2",
    "1 / 4", "1 / 8", "1 / 16", "1 / 32", "1 / 190",
    "2 / 4", "4 / 4", "8 / 4", "16 / 4", "32 / 4",
    "2 / 8", "2 / 16", "2 / 32", "2 / 190",
    "4 / 8", "8 / 8", "16 / 8", "32 / 8",
    "4 / 16", "4 / 32", "4 / 190",
    "8 / 16", "16 / 16", "32 / 16",
    "8 / 32", "8 / 190",
    "16 / 32", "32 / 32",
    "16 / 190", "32 / 190"
  ))) %>%
  filter(movement %in% c("8 / 8", "8 / 2", "2 / 8", "2 / 2"))

connect_full <- read_csv(here::here("data", "processed_data", "connectivity_results.csv"))

connect <- connect_full %>%
  mutate(
    adult_fished_c_1 = adult_RS_fished + adult_IS_fished,
    adult_mpa_c_1 = adult_RS_mpa + adult_IS_mpa,
    larvae_fished_c_1 = larvae_RS_fished + larvae_IS_fished,
    larvae_mpa_c_1 = larvae_RS_mpa + larvae_IS_mpa,
    adult_c_2 = adult_SR + adult_II,
    larvae_c_2 = larvae_SR + larvae_II
  ) %>%
  mutate(
    fished_c1 = adult_fished_c_1 + larvae_fished_c_1,
    mpa_c1 = adult_mpa_c_1 + larvae_mpa_c_1,
    c2 = adult_c_2 + larvae_c_2,
    relative_mpa_c1 = adult_mpa_c_1 / larvae_mpa_c_1,
    relative_fished_c1 = adult_fished_c_1 / larvae_fished_c_1,
    relative_c2 = adult_c_2 / larvae_c_2,
    relative_mpa_R = adult_RS_mpa / larvae_RS_mpa,
    relative_fished_R = adult_RS_fished / larvae_RS_fished,
    relative_mpa_I = adult_IS_mpa / larvae_IS_mpa,
    relative_fished_I = adult_IS_fished / larvae_IS_fished,
  ) %>%
  mutate(adult_cat = case_when(
    adult %in% c(1, 2) ~ "low",
    adult %in% c(4, 8) ~ "high",
    adult %in% c(16, 32) ~ "high"
  )) %>%
  mutate(larval_cat = case_when(
    larval %in% c(4, 2) ~ "low",
    larval %in% c(16, 8) ~ "high",
    larval %in% c(190, 32) ~ "high"
  )) %>%
  mutate(
    movement = c(paste0(adult, " / ", larval)),
    move_cat = c(paste0(adult_cat, " / ", larval_cat)),
    move_ratio = adult / larval
  ) %>%
  mutate(movement = fct_relevel(movement, c(
    "1 / 2", "2 / 2", "4 / 2", "8 / 2", "16 / 2", "32 / 2",
    "1 / 4", "1 / 8", "1 / 16", "1 / 32", "1 / 190",
    "2 / 4", "4 / 4", "8 / 4", "16 / 4", "32 / 4",
    "2 / 8", "2 / 16", "2 / 32", "2 / 190",
    "4 / 8", "8 / 8", "16 / 8", "32 / 8",
    "4 / 16", "4 / 32", "4 / 190",
    "8 / 16", "16 / 16", "32 / 16",
    "8 / 32", "8 / 190",
    "16 / 32", "32 / 32",
    "16 / 190", "32 / 190"
  ))) %>%
  mutate(ratio_cat = case_when(
    move_ratio < 1 ~ "Larval Greater",
    move_ratio == 1 ~ "Equal",
    move_ratio > 1 ~ "Adult Greater"
  )) %>%
  filter(larval != 190)

single_mpa_connect8 <- connect %>%
  filter(movement %in% c("8 / 8", "8 / 2", "2 / 8")) %>%
  filter(
    mpa_size == 8,
    mpa_spacing == 8
  )

single_mpa_connect4 <- connect %>%
  filter(movement %in% c("8 / 8", "8 / 2", "2 / 8")) %>%
  filter(
    mpa_size == 4,
    mpa_spacing == 4
  )

single_mpa_all_move <- connect %>%
  filter(adult %in% c(2, 4, 8, 16)) %>%
  filter(larval %in% c(2, 4, 8, 16)) %>%
  filter(
    mpa_size == 8,
    mpa_spacing == 8
  )

move_filter <- connect %>%
  filter(movement %in% c("8 / 8", "8 / 2", "2 / 8"))

eq_pop_size4 = mpa %>% 
  filter(age == "adult") %>% filter(generation == 40) %>% filter(mpa_size == 4) %>% 
  pull(mean_pop)

eq_pop_spacing4 = mpa %>% 
  filter(age == "adult") %>% filter(generation == 40) %>% filter(mpa_spacing == 4) %>% 
  pull(mean_pop)


# Figs --------------------------------------------------------------------

p1 <- ggplot(single_mpa_all_move) +
  geom_hline(aes(yintercept = 1), color = "red", linetype = "dashed", linewidth = 1) +
  geom_point(aes(adult, relative_c2, color = as.factor(larval)), size = 5) +
  theme_bw(base_size = 20) +
  scale_color_viridis_d(end = 0.9) +
  labs(
    x = "Adult Movement",
    y = "Relative Adult / Larval Connectivity",
    color = "Larval Movement",
    linetype = "Larval Movement",
    title = "Theoretical Connectivity"
  ) +
  geom_smooth(aes(adult, relative_c2, linetype = as.factor(larval)), se = FALSE, alpha = 0.3, color = "black", linewidth = 1) +
  ylim(c(0.2, 4.5))

ggsave(p1, path = here::here("figs", "pres"), file = paste0("theoretical.pdf"), height = 8, width = 8, limitsize = FALSE)

p2 <- ggplot(single_mpa_all_move) +
  # geom_abline(aes()) +
  geom_hline(aes(yintercept = 1), color = "red", linetype = "dashed", linewidth = 1) +
  geom_point(aes(adult, relative_mpa_c1, color = as.factor(larval)), size = 5) +
  theme_bw(base_size = 20) +
  scale_color_viridis_d(end = 0.9) +
  labs(
    x = "Adult Movement",
    y = "",
    color = "Larval Movement",
    linetype = "Larval Movement",
    title = "Realized Connectivity"
  ) +
  geom_smooth(aes(adult, relative_mpa_c1, linetype = as.factor(larval)), se = FALSE, alpha = 0.3, color = "black", linewidth = 1) +
  ylim(c(0.2, 4.5))

plot <- p1 + p2 + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect")

ggsave(plot, path = here::here("figs", "pres"), file = paste0("theoretical_v_realized.pdf"), height = 8, width = 12, limitsize = FALSE)

p3 <- ggplot(single_mpa_all_move) +
  # geom_abline(aes()) +
  geom_hline(aes(yintercept = 1), color = "red", linetype = "dashed", linewidth = 1) +
  geom_point(aes(adult, relative_fished_c1, color = as.factor(larval)), size = 5) +
  theme_bw(base_size = 20) +
  scale_color_viridis_d(end = 0.9) +
  labs(
    x = "Adult Movement",
    y = "",
    color = "Larval Movement",
    linetype = "Larval Movement",
    title = "Realized Connectivity \n without MPA"
  ) +
  geom_smooth(aes(adult, relative_fished_c1, linetype = as.factor(larval)), se = FALSE, alpha = 0.3, color = "black", linewidth = 1) +
  ylim(c(0.2, 4.5))

plot <- p1 + p3 + p2 + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect")

ggsave(plot, path = here::here("figs", "pres"), file = paste0("theoretical_v_realized2.pdf"), height = 8, width = 16, limitsize = FALSE)

p1 <- ggplot(single_mpa_connect4) +
  geom_hline(aes(yintercept = 1), linetype = "dashed", linewidth = 1) +
  geom_point(aes(ratio_cat, relative_mpa_R, color = "Retention"), size = 5) +
  geom_point(aes(ratio_cat, relative_mpa_I, color = "Import"), size = 5) +
  theme_bw(base_size = 20) +
  labs(
    x = "Relative Movement",
    y = "Relative Contribution Adult vs. Larvae",
    color = "",
    title = "Small MPA"
  ) +
  scale_color_manual(values = c("Retention" = "green", "Import" = "blue"))

p2 <- ggplot(single_mpa_connect8) +
  geom_hline(aes(yintercept = 1), linetype = "dashed", linewidth = 1) +
  geom_point(aes(ratio_cat, relative_mpa_R, color = "Retention"), size = 5) +
  geom_point(aes(ratio_cat, relative_mpa_I, color = "Import"), size = 5) +
  theme_bw(base_size = 20) +
  labs(
    x = "Relative Movement",
    y = "",
    color = "",
    title = "Large MPA"
  ) +
  scale_color_manual(values = c("Retention" = "green", "Import" = "blue")) +
  scale_y_log10()

plot <- p1 + p2 + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect")

ggsave(plot, path = here::here("figs", "pres"), file = paste0("retention_v_import.pdf"), height = 8, width = 12, limitsize = FALSE)

p1 <- ggplot(move_filter %>% filter(mpa_spacing == 4)) +
  geom_hline(aes(yintercept = 1), linetype = "dashed", linewidth = 1) +
  geom_jitter(aes(mpa_size, relative_mpa_R, color = as.factor(ratio_cat)), height = 0, width = 0.7, size = 5) +
  theme_bw(base_size = 20) +
  scale_color_viridis_d(end = 0.9) +
  scale_y_log10() +
  labs(
    x = "MPA Size",
    y = "Relative Retention: Adult / Larval",
    color = ""
  )

p2 <- ggplot(move_filter %>% filter(mpa_size == 4)) +
  geom_hline(aes(yintercept = 1), linetype = "dashed", linewidth = 1) +
  geom_jitter(aes(mpa_spacing, relative_mpa_R, color = as.factor(ratio_cat)), height = 0, width = 0.7, size = 5) +
  theme_bw(base_size = 20) +
  scale_color_viridis_d(end = 0.9) +
  scale_y_log10() +
  labs(
    x = "MPA Spacing",
    y = "Relative Retention: Adult / Larval",
    color = ""
  )

p4 <- ggplot(move_filter %>% filter(mpa_size == 4)) +
  geom_hline(aes(yintercept = 1), linetype = "dashed", linewidth = 1) +
  geom_jitter(aes(mpa_spacing, relative_mpa_I, color = as.factor(ratio_cat)), height = 0, width = 0.7, size = 5) +
  theme_bw(base_size = 20) +
  scale_color_viridis_d(end = 0.9) +
  scale_y_log10() +
  labs(
    x = "MPA Spacing",
    y = "Relative Import: Adult / Larval",
    color = ""
  )

p3 <- ggplot(move_filter %>% filter(mpa_spacing == 4)) +
  geom_hline(aes(yintercept = 1), linetype = "dashed", linewidth = 1) +
  geom_jitter(aes(mpa_size, relative_mpa_I, color = as.factor(ratio_cat)), height = 0, width = 0.7, size = 5) +
  theme_bw(base_size = 20) +
  scale_color_viridis_d(end = 0.9) +
  scale_y_log10() +
  labs(
    x = "MPA Size",
    y = "Relative Import: Adult / Larval",
    color = ""
  )

plot <- (p1 + p2) / (p3 + p4) + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect")

ggsave(plot, path = here::here("figs", "pres"), file = paste0("retention_v_import2.pdf"), height = 15, width = 15, limitsize = FALSE)


p1 <- ggplot(move_filter %>% filter(mpa_spacing == 4)) +
  geom_hline(aes(yintercept = 1), linetype = "dashed", linewidth = 1) +
  geom_jitter(aes(mpa_size, adult_RS_mpa, color = as.factor(ratio_cat), shape = "Adult"), height = 0, width = 0.7, size = 5) +
  geom_jitter(aes(mpa_size, larvae_RS_mpa, color = as.factor(ratio_cat), shape = "Larval"), height = 0, width = 0.7, size = 5) +
  theme_bw(base_size = 20) +
  scale_color_viridis_d(end = 0.9) +
  scale_y_log10() +
  labs(
    x = "MPA Size",
    y = "Absolute Retention",
    color = "",
    shape = ""
  ) +
  scale_shape_manual(values = c("Adult" = 19, "Larval" = 1))

p2 <- ggplot(move_filter %>% filter(mpa_size == 4)) +
  geom_hline(aes(yintercept = 1), linetype = "dashed", linewidth = 1) +
  geom_jitter(aes(mpa_spacing, adult_RS_mpa, color = as.factor(ratio_cat), shape = "Adult"), height = 0, width = 0.7, size = 5) +
  geom_jitter(aes(mpa_spacing, larvae_RS_mpa, color = as.factor(ratio_cat), shape = "Larval"), height = 0, width = 0.7, size = 5) +
  theme_bw(base_size = 20) +
  scale_color_viridis_d(end = 0.9) +
  scale_y_log10() +
  labs(
    x = "MPA Spacing",
    y = "Absolute Retention",
    color = "",
    shape = ""
  ) +
  scale_shape_manual(values = c("Adult" = 19, "Larval" = 1))

p4 <- ggplot(move_filter %>% filter(mpa_size == 4) %>% filter(mpa_spacing != 0)) +
  geom_hline(aes(yintercept = 1), linetype = "dashed", linewidth = 1) +
  geom_jitter(aes(mpa_spacing, adult_IS_mpa, color = as.factor(ratio_cat), shape = "Adult"), height = 0, width = 0.7, size = 5) +
  geom_jitter(aes(mpa_spacing, larvae_IS_mpa, color = as.factor(ratio_cat), shape = "Larval"), height = 0, width = 0.7, size = 5) +
  theme_bw(base_size = 20) +
  scale_color_viridis_d(end = 0.9) +
  scale_y_log10() +
  labs(
    x = "MPA Spacing",
    y = "Absolute Import",
    color = "",
    shape = ""
  ) +
  scale_shape_manual(values = c("Adult" = 19, "Larval" = 1))

p3 <- ggplot(move_filter %>% filter(mpa_spacing == 4)) +
  geom_hline(aes(yintercept = 1), linetype = "dashed", linewidth = 1) +
  geom_jitter(aes(mpa_size, adult_IS_mpa, color = as.factor(ratio_cat), shape = "Adult"), height = 0, width = 0.7, size = 5) +
  geom_jitter(aes(mpa_size, larvae_IS_mpa, color = as.factor(ratio_cat), shape = "Larval"), height = 0, width = 0.7, size = 5) +
  theme_bw(base_size = 20) +
  scale_color_viridis_d(end = 0.9) +
  scale_y_log10() +
  labs(
    x = "MPA Size",
    y = "Absolute Import",
    color = "",
    shape = ""
  ) +
  scale_shape_manual(values = c("Adult" = 19, "Larval" = 1))

plot <- (p1 + p2) / (p3 + p4) + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect")

ggsave(plot, path = here::here("figs", "pres"), file = paste0("absolute_retention_import.pdf"), height = 15, width = 15, limitsize = FALSE)

p1 <- ggplot(mpa %>% filter(age == "adult") %>% filter(generation == 90) %>% filter(mpa_spacing == 4)) +
  geom_hline(aes(yintercept = eq_pop_spacing4), linetype = "dashed", linewidth = 1) +
  geom_point(aes(mpa_size, mean_pop, color = as.factor(move_cat)), size = 5) +
  geom_line(aes(mpa_size, mean_pop, color = as.factor(move_cat)), linewidth = 1) +
  theme_bw(base_size = 20) +
  scale_color_viridis_d(end = 0.9) +
  labs(
    x = "MPA Size",
    y = "Average Population in the MPA",
    color = "Adult / Larval"
  )

p2 <- ggplot(mpa %>% filter(age == "adult") %>% filter(generation == 90) %>% filter(mpa_size == 4)) +
  geom_hline(aes(yintercept = eq_pop_size4), linetype = "dashed", linewidth = 1) +
  geom_point(aes(mpa_spacing, mean_pop, color = as.factor(move_cat)), size = 5) +
  geom_line(aes(mpa_spacing, mean_pop, color = as.factor(move_cat)), linewidth = 1) +
  theme_bw(base_size = 20) +
  scale_color_viridis_d(end = 0.9) +
  labs(
    x = "MPA Spacing",
    y = "Average Population in the MPA",
    color = "Adult / Larval"
  ) 

plot <- (p1 + p2) / (p3 + p4) + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect")

ggsave(plot, path = here::here("figs", "pres"), file = paste0("population_mpa.pdf"), height = 10, width = 15, limitsize = FALSE)
