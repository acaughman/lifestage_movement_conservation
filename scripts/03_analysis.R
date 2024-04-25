library(tidyverse)
library(patchwork)

# Process Data ------------------------------------------------------------

output = read_csv(here::here("data", "processed_data", "model_results.csv"))
connect = read_csv(here::here("data", "processed_data", "connectivity_results.csv"))

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
    larval %in% c(1, 2) ~ "low",
    larval %in% c(4, 8) ~ "medium",
    larval %in% c(16, 32) ~ "high"
  )) %>% 
  mutate(movement = c(paste0(adult, " / ", larval)),
         move_cat = c(paste0(adult_cat, " / ", larval_cat)))

rm(output)

mpa = mpa %>%
  mutate(movement = fct_relevel(movement, c(
    "1 / 1", "2 / 1", "4 / 1", "8 / 1", "16 / 1", "32 / 1",
    "1 / 2", "1 / 4", "1 / 8", "1 / 16", "1 / 32",
    "2 / 2", "4 / 2", "8 / 2", "16 / 2", "32 / 2",
    "2 / 4", "2 / 8", "2 / 16", "2 / 32",
    "4 / 4", "8 / 4", "16 / 4", "32 / 4",
    "4 / 8", "4 / 16", "4 / 32",
    "8 / 8", "16 / 8", "32 / 8",
    "8 / 16", "8 / 32",
    "16 / 16", "32 / 16",
    "16 / 32", "32 / 32"
  )))

connect = connect %>% 
  mutate(adult_fished_c_1 = adult_RS_fished + adult_IS_fished,
         adult_mpa_c_1 = adult_RS_mpa + adult_IS_mpa,
         larvae_fished_c_1 = larvae_RS_fished + larvae_IS_fished,
         larvae_mpa_c_1 = larvae_RS_mpa + larvae_IS_mpa,
         adult_c_2 = adult_SR + adult_II,
         larvae_c_2 = larvae_SR + larvae_II) %>% 
  mutate(fished_c1 = adult_fished_c_1 + larvae_fished_c_1,
         mpa_c1 = adult_mpa_c_1 + larvae_mpa_c_1,
         c2 = adult_c_2 + larvae_c_2,
         relative_mpa_c1 = adult_mpa_c_1 / larvae_mpa_c_1,
         relative_c2 = adult_c_2 / larvae_c_2) %>% 
  mutate(adult_cat = case_when(
    adult %in% c(1, 2) ~ "low",
    adult %in% c(4, 8) ~ "medium",
    adult %in% c(16, 32) ~ "high"
  )) %>% 
  mutate(larval_cat = case_when(
    larval %in% c(1, 2) ~ "low",
    larval %in% c(4, 8) ~ "medium",
    larval %in% c(16, 32) ~ "high"
  )) %>% 
  mutate(movement = c(paste0(adult, " / ", larval)),
         move_cat = c(paste0(adult_cat, " / ", larval_cat))) %>% 
mutate(movement = fct_relevel(movement, c(
  "1 / 1", "2 / 1", "4 / 1", "8 / 1", "16 / 1", "32 / 1",
  "1 / 2", "1 / 4", "1 / 8", "1 / 16", "1 / 32",
  "2 / 2", "4 / 2", "8 / 2", "16 / 2", "32 / 2",
  "2 / 4", "2 / 8", "2 / 16", "2 / 32",
  "4 / 4", "8 / 4", "16 / 4", "32 / 4",
  "4 / 8", "4 / 16", "4 / 32",
  "8 / 8", "16 / 8", "32 / 8",
  "8 / 16", "8 / 32",
  "16 / 16", "32 / 16",
  "16 / 32", "32 / 32"
)))


# Figures -----------------------------------------------------------------

p1 = ggplot(mpa %>% filter(age == "adult") %>% filter(generation > 90)) +
  geom_point(aes(mpa_spacing, mean_pop, color = as.factor(larval), shape = as.factor(adult))) +
  geom_line(aes(mpa_spacing, mean_pop, color = as.factor(larval), linetype = as.factor(adult), group = movement), linewidth = 0.8) +
  theme_bw() +
  facet_wrap(~ mpa_size, nrow = 1, scales = "free_x") +
  labs(
    x = "MPA Spacing",
    y = "Average Population Size in MPA",
    color = "Larval Movement",
    shape = "Adult Movement",
    linetype = "Adult Movement"
  ) +
  scale_color_viridis_d()

ggsave(p1, path = here::here("figs"), file = paste0("mpa_spacing.pdf"), height = 8, width = 12, limitsize = FALSE)

p2 = ggplot(mpa %>% filter(age == "adult") %>% filter(generation == 90)) +
  geom_point(aes(mpa_size, mean_pop, color = as.factor(larval), shape = as.factor(adult))) +
  geom_line(aes(mpa_size, mean_pop, color = as.factor(larval), linetype = as.factor(adult), group = movement), linewidth = 0.8) +
  theme_bw() +
  facet_wrap(~ mpa_spacing, nrow = 1, scales = "free_x") +
  labs(
    x = "MPA Size",
    y = "Average Population Size in MPA",
    color = "Larval Movement",
    shape = "Adult Movement",
    linetype = "Adult Movement"
  ) +
  scale_color_viridis_d()

ggsave(p2, path = here::here("figs"), file = paste0("mpa_size.pdf"), height = 8, width = 12, limitsize = FALSE)

p1 = ggplot(connect) +
  geom_point(aes(adult, adult_RS_mpa, color = as.factor(mpa_spacing), shape = as.factor(mpa_size))) +
  geom_point(aes(adult, adult_RS_fished), color = "black") +
  scale_color_viridis_d() +
  theme_bw() +
  labs(x = "Adult Movement",
       y = "Retention Strength",
       color = "MPA Spacing", 
       shape = "MPA Size")

p2 = ggplot(connect) +
  geom_point(aes(larval, larvae_RS_mpa, color = as.factor(mpa_spacing), shape = as.factor(mpa_size))) +
  geom_point(aes(larval, larvae_RS_fished), color = "black") +
  scale_color_viridis_d() +
  theme_bw() +
  labs(x = "Larval Movement",
       y = "Retention Strength",
       color = "MPA Spacing", 
       shape = "MPA Size")

p3 = ggplot(connect) +
  geom_point(aes(adult, adult_IS_mpa, color = as.factor(mpa_spacing), shape = as.factor(mpa_size))) +
  geom_point(aes(adult, adult_IS_fished), color = "black") +
  scale_color_viridis_d() +
  theme_bw() +
  labs(x = "Adult Movement",
       y = "Import Strength",
       color = "MPA Spacing", 
       shape = "MPA Size")

p4 = ggplot(connect) +
  geom_point(aes(larval, larvae_IS_mpa, color = as.factor(mpa_spacing), shape = as.factor(mpa_size))) +
  geom_point(aes(larval, larvae_IS_fished), color = "black") +
  scale_color_viridis_d() +
  theme_bw() +
  labs(x = "Larval Movement",
       y = "Import Strength",
       color = "MPA Spacing", 
       shape = "MPA Size")

plot = (p1 + p2) / (p3 + p4) + plot_annotation(tag_level = "A") + plot_layout(guides = "collect")

ggsave(plot, path = here::here("figs"), file = paste0("absolute_connectivity.pdf"), height = 8, width = 12, limitsize = FALSE)

p1 = ggplot(connect) +
  geom_point(aes(adult, adult_ID_mpa, color = as.factor(mpa_spacing), shape = as.factor(mpa_size))) +
  geom_point(aes(adult, adult_ID_fished), color = "black") +
  scale_color_viridis_d() +
  theme_bw() +
  labs(x = "Adult Movement",
       y = "Import Diversity",
       color = "MPA Spacing", 
       shape = "MPA Size")

p2 = ggplot(connect) +
  geom_point(aes(larval, larvae_ID_mpa, color = as.factor(mpa_spacing), shape = as.factor(mpa_size))) +
  geom_point(aes(larval, larvae_ID_fished), color = "black") +
  scale_color_viridis_d() +
  theme_bw() +
  labs(x = "Larval Movement",
       y = "Import Diversity",
       color = "MPA Spacing", 
       shape = "MPA Size")

plot = (p1 + p2) + plot_annotation(tag_level = "A") + plot_layout(guides = "collect")

ggsave(plot, path = here::here("figs"), file = paste0("import_diversity.pdf"), height = 8, width = 12, limitsize = FALSE)

# p1 = ggplot(connect) +
#   geom_point(aes(adult, adult_SR, color = as.factor(mpa_spacing), shape = as.factor(mpa_size))) +
#   geom_point(aes(adult, adult_SR), color = "black") +
#   scale_color_viridis_d() +
#   theme_bw() +
#   labs(x = "Adult Movement",
#        y = "Self-Recruitment",
#        color = "MPA Spacing", 
#        shape = "MPA Size")
# 
# p2 = ggplot(connect) +
#   geom_point(aes(larval, larvae_SR, color = as.factor(mpa_spacing), shape = as.factor(mpa_size))) +
#   geom_point(aes(larval, larvae_SR), color = "black") +
#   scale_color_viridis_d() +
#   theme_bw() +
#   labs(x = "Larval Movement",
#        y = "Self-Recruitment",
#        color = "MPA Spacing", 
#        shape = "MPA Size")
# 
# p3 = ggplot(connect) +
#   geom_point(aes(adult, adult_II, color = as.factor(mpa_spacing), shape = as.factor(mpa_size))) +
#   geom_point(aes(adult, adult_II), color = "black") +
#   scale_color_viridis_d() +
#   theme_bw() +
#   labs(x = "Adult Movement",
#        y = "Import Influence",
#        color = "MPA Spacing", 
#        shape = "MPA Size")
# 
# p4 = ggplot(connect) +
#   geom_point(aes(larval, larvae_II, color = as.factor(mpa_spacing), shape = as.factor(mpa_size))) +
#   geom_point(aes(larval, larvae_II), color = "black") +
#   scale_color_viridis_d() +
#   theme_bw() +
#   labs(x = "Larval Movement",
#        y = "Import Influence",
#        color = "MPA Spacing", 
#        shape = "MPA Size")
# 
# (p1 + p2) / (p3 + p4) + plot_annotation(tag_level = "A") + plot_layout(guides = "collect")

# p1 = ggplot(connect) +
#   geom_point(aes(mpa_size, adult_mpa_c_1, color = movement)) +
#   theme_bw() +
#   scale_color_viridis_d() +
#   geom_smooth(aes(mpa_size, adult_mpa_c_1, linetype = as.factor(adult)), se=FALSE, alpha = 0.3, color = "black", linewidth = 0.4) +
#   labs(x = "MPA Size",
#        y = "Absolute Adult Settlers", 
#        color = "Movement",
#        linetype = "Adult Movement")
# p2 = ggplot(connect) +
#   geom_point(aes(mpa_spacing, adult_mpa_c_1, color = movement)) +
#   theme_bw() +
#   scale_color_viridis_d() +
#   geom_smooth(aes(mpa_spacing, adult_mpa_c_1, linetype = as.factor(adult)), se=FALSE, alpha = 0.3, color = "black", linewidth = 0.4) +
#   labs(x = "MPA Spacing",
#        y = "Absolute Adult Settlers", 
#        color = "Movement",
#        linetype = "Adult Movement")
# p3 = ggplot(connect) +
#   geom_point(aes(mpa_size, larvae_mpa_c_1, color = movement)) +
#   theme_bw() +
#   scale_color_viridis_d() +
#   geom_smooth(aes(mpa_size, larvae_mpa_c_1, linetype = as.factor(larval)), se=FALSE, alpha = 0.3, color = "black", linewidth = 0.4) +
#   labs(x = "MPA Size",
#        y = "Absolute Larval Settlers", 
#        color = "Movement",
#        linetype = "Larval Movement")
# p4 = ggplot(connect) +
#   geom_point(aes(mpa_spacing, larvae_mpa_c_1, color = movement)) +
#   theme_bw() +
#   scale_color_viridis_d() +
#   geom_smooth(aes(mpa_spacing, larvae_mpa_c_1, linetype = as.factor(larval)), se=FALSE, alpha = 0.3, color = "black", linewidth = 0.4) +
#   labs(x = "MPA Spacing",
#        y = "Absolute Larval Settlers", 
#        color = "Movement",
#        linetype = "Larval Movement")
# 
# plot = (p1 + p2) / (p3 + p4) + plot_annotation(tag_level = "A") + plot_layout(guides = "collect")

p1 = ggplot(connect) +
  geom_point(aes(mpa_size, mpa_c1, color = move_cat)) +
  theme_bw() +
  scale_color_viridis_d() +
  geom_smooth(aes(mpa_size, adult_mpa_c_1, linetype = as.factor(adult)), se=FALSE, alpha = 0.3, color = "black", linewidth = 0.4) +
  labs(x = "MPA Size",
       y = "Absolute Settlers", 
       color = "Movement",
       linetype = "Adult Movement")
p2 = ggplot(connect) +
  geom_point(aes(mpa_spacing, mpa_c1, color = move_cat)) +
  theme_bw() +
  scale_color_viridis_d() +
  geom_smooth(aes(mpa_spacing, adult_mpa_c_1, linetype = as.factor(adult)), se=FALSE, alpha = 0.3, color = "black", linewidth = 0.4) +
  labs(x = "MPA Spacing",
       y = "Absolute Settlers", 
       color = "Movement",
       linetype = "Adult Movement")
p3 = ggplot(connect) +
  geom_point(aes(mpa_size, mpa_c1, color = move_cat)) +
  theme_bw() +
  scale_color_viridis_d() +
  geom_smooth(aes(mpa_size, larvae_mpa_c_1, linetype = as.factor(larval)), se=FALSE, alpha = 0.3, color = "black", linewidth = 0.4) +
  labs(x = "MPA Size",
       y = "Absolute Settlers", 
       color = "Movement",
       linetype = "Larval Movement")
p4 = ggplot(connect) +
  geom_point(aes(mpa_spacing, mpa_c1, color = move_cat)) +
  theme_bw() +
  scale_color_viridis_d() +
  geom_smooth(aes(mpa_spacing, larvae_mpa_c_1, linetype = as.factor(larval)), se=FALSE, alpha = 0.3, color = "black", linewidth = 0.4) +
  labs(x = "MPA Spacing",
       y = "Absolute Settlers", 
       color = "Movement",
       linetype = "Larval Movement")

plot = (p1 + p2) / (p3 + p4) + plot_annotation(tag_level = "A") + plot_layout(guides = "collect")

ggsave(plot, path = here::here("figs"), file = paste0("adultvslarval_connectivity.pdf"), height = 8, width = 12, limitsize = FALSE)

p3 = ggplot(connect) +
  geom_hline(aes(yintercept = 1), color = "red", linetype = "dashed", linewidth = 0.4) +
  geom_point(aes(larval, relative_mpa_c1, color = as.factor(adult))) +
  theme_bw() +
  scale_color_viridis_d() +
  labs(x = "Larval Movement",
       y = "Relative Adult / Larval Settlers", 
       color = "Adult Movement",
       linetype = "Adult Movement",
       title = "Realized Connectivity") +
  geom_smooth(aes(larval, relative_mpa_c1, linetype = as.factor(adult)), se=FALSE, alpha = 0.3, color = "black", linewidth = 0.4) +
  scale_y_log10() 
p4 = ggplot(connect) +
  geom_hline(aes(yintercept = 1), color = "red", linetype = "dashed", linewidth = 0.4) +
  geom_point(aes(adult, relative_mpa_c1, color = as.factor(larval))) +
  theme_bw() +
  scale_color_viridis_d() +
  labs(x = "Adult Movement",
       y = "Relative Adult / Larval Settlers", 
       color = "Larval Movement",
       linetype = "Larval Movement") +
  geom_smooth(aes(adult, relative_mpa_c1, linetype = as.factor(larval)), se=FALSE, alpha = 0.3, color = "black", linewidth = 0.4) +
  scale_y_log10() 

p1 = ggplot(connect) +
  geom_hline(aes(yintercept = 1), color = "red", linetype = "dashed", linewidth = 0.4) +
  geom_point(aes(larval, relative_c2, color = as.factor(adult))) +
  theme_bw() +
  scale_color_viridis_d() +
  labs(x = "Larval Movement",
       y = "Relative Adult / Larval Settlers", 
       color = "Adult Movement",
       title = "Theorectical Connectivity") +
  scale_y_log10() +
  theme(legend.position = "none")
p2 = ggplot(connect) +
  geom_hline(aes(yintercept = 1), color = "red", linetype = "dashed", linewidth = 0.4) +
  geom_point(aes(adult, relative_c2, color = as.factor(larval))) +
  theme_bw() +
  scale_color_viridis_d() +
  labs(x = "Adult Movement",
       y = "Relative Adult / Larval Settlers", 
       color = "Larval Movement") +
  scale_y_log10() +
  theme(legend.position = "none")

plot = (p1 + p2) / (p3 + p4) + plot_annotation(tag_level = "A") + plot_layout(guides = "collect")

ggsave(plot, path = here::here("figs"), file = paste0("relative_connectivity.pdf"), height = 8, width = 12, limitsize = FALSE)

# Combine Biomass Data ------------------------------------------------------------

# output_df1 <- read_csv(here::here("outputs", "2x2_0.csv")) %>%
#   mutate(
#     mpa_size = 2,
#     mpa_spacing = 0,
#     mpa = case_when(
#       (lat %in% c(25, 26) & lon %in% c(25, 26)) ~ "MPA 1",
#       TRUE ~ "Non-MPA"
#     )
#   )
# output_df2 <- read_csv(here::here("outputs", "4x4_0.csv")) %>%
#   mutate(
#     mpa_size = 4,
#     mpa_spacing = 0,
#     mpa = case_when(
#       (lat %in% c(24:27) & lon %in% c(24:27)) ~ "MPA 1",
#       TRUE ~ "Non-MPA"
#     )
#   )
# output_df3 <- read_csv(here::here("outputs", "8x8_0.csv")) %>%
#   mutate(
#     mpa_size = 8,
#     mpa_spacing = 0,
#     mpa = case_when(
#       (lat %in% c(22:29) & lon %in% c(22:29)) ~ "MPA 1",
#       TRUE ~ "Non-MPA"
#     )
#   )
# output_df4 <- read_csv(here::here("outputs", "16x16_0.csv")) %>%
#   mutate(
#     mpa_size = 16,
#     mpa_spacing = 0,
#     mpa = case_when(
#       (lat %in% c(17:32) & lon %in% c(17:32)) ~ "MPA 1",
#       TRUE ~ "Non-MPA"
#     )
#   )
# output_df5 <- read_csv(here::here("outputs", "4x4_2.csv")) %>%
#   mutate(
#     mpa_size = 4,
#     mpa_spacing = 2,
#     mpa = case_when(
#       (lat %in% c(21:24) & lon %in% c(24:27)) ~ "MPA 1",
#       (lat %in% c(27:30) & lon %in% c(24:27)) ~ "MPA 2",
#       TRUE ~ "Non-MPA"
#     )
#   )
# output_df6 <- read_csv(here::here("outputs", "4x4_4.csv")) %>%
#   mutate(
#     mpa_size = 4,
#     mpa_spacing = 4,
#     mpa = case_when(
#       (lat %in% c(20:23) & lon %in% c(24:27)) ~ "MPA 1",
#       (lat %in% c(28:31) & lon %in% c(24:27)) ~ "MPA 2",
#       TRUE ~ "Non-MPA"
#     )
#   )
# output_df7 <- read_csv(here::here("outputs", "4x4_8.csv")) %>%
#   mutate(
#     mpa_size = 4,
#     mpa_spacing = 8,
#     mpa = case_when(
#       (lat %in% c(18:21) & lon %in% c(24:27)) ~ "MPA 1",
#       (lat %in% c(30:33) & lon %in% c(24:27)) ~ "MPA 2",
#       TRUE ~ "Non-MPA"
#     )
#   )
# output_df8 <- read_csv(here::here("outputs", "4x4_16.csv")) %>%
#   mutate(
#     mpa_size = 4,
#     mpa_spacing = 16,
#     mpa = case_when(
#       (lat %in% c(14:17) & lon %in% c(24:27)) ~ "MPA 1",
#       (lat %in% c(34:37) & lon %in% c(24:27)) ~ "MPA 2",
#       TRUE ~ "Non-MPA"
#     )
#   )
# output_df9 <- read_csv(here::here("outputs", "8x8_2.csv")) %>%
#   mutate(
#     mpa_size = 8,
#     mpa_spacing = 2,
#     mpa = case_when(
#       (lat %in% c(17:24) & lon %in% c(22:29)) ~ "MPA 1",
#       (lat %in% c(27:34) & lon %in% c(22:29)) ~ "MPA 2",
#       TRUE ~ "Non-MPA"
#     )
#   )
# output_df10 <- read_csv(here::here("outputs", "8x8_4.csv")) %>%
#   mutate(
#     mpa_size = 8,
#     mpa_spacing = 4,
#     mpa = case_when(
#       (lat %in% c(16:23) & lon %in% c(22:29)) ~ "MPA 1",
#       (lat %in% c(28:35) & lon %in% c(22:29)) ~ "MPA 2",
#       TRUE ~ "Non-MPA"
#     )
#   )
# output_df11 <- read_csv(here::here("outputs", "8x8_8.csv")) %>%
#   mutate(
#     mpa_size = 8,
#     mpa_spacing = 8,
#     mpa = case_when(
#       (lat %in% c(14:21) & lon %in% c(22:29)) ~ "MPA 1",
#       (lat %in% c(30:37) & lon %in% c(22:29)) ~ "MPA 2",
#       TRUE ~ "Non-MPA"
#     )
#   )
# output_df12 <- read_csv(here::here("outputs", "8x8_16.csv")) %>%
#   mutate(
#     mpa_size = 8,
#     mpa_spacing = 16,
#     mpa = case_when(
#       (lat %in% c(10:17) & lon %in% c(22:29)) ~ "MPA 1",
#       (lat %in% c(34:41) & lon %in% c(22:29)) ~ "MPA 2",
#       TRUE ~ "Non-MPA"
#     )
#   )
# 
# output <- list(
#   output_df1, output_df2, output_df3, output_df4, output_df5, output_df6,
#   output_df7, output_df8, output_df9, output_df10, output_df11, output_df12
# ) %>%
#   reduce(full_join)
# 
# rm(
#   output_df1, output_df2, output_df3, output_df4, output_df5, output_df6,
#   output_df7, output_df8, output_df9, output_df10, output_df11, output_df12
# )
#
#write_csv(output, here::here("data", "processed_data", "model_results.csv"))

# Combine Connectivity Data -----------------------------------------------

# output_df1 <- read_csv(here::here("outputs", "connectivity_2x2_0.csv")) %>%
#   mutate(
#     mpa_size = 2,
#     mpa_spacing = 0
#   )
# output_df2 <- read_csv(here::here("outputs", "connectivity_4x4_0.csv")) %>%
#   mutate(
#     mpa_size = 4,
#     mpa_spacing = 0
#   )
# output_df3 <- read_csv(here::here("outputs", "connectivity_8x8_0.csv")) %>%
#   mutate(
#     mpa_size = 8,
#     mpa_spacing = 0
#   )
# output_df4 <- read_csv(here::here("outputs", "connectivity_16x16_0.csv")) %>%
#   mutate(
#     mpa_size = 16,
#     mpa_spacing = 0
#   )
# output_df5 <- read_csv(here::here("outputs", "connectivity_4x4_2.csv")) %>%
#   mutate(
#     mpa_size = 4,
#     mpa_spacing = 2
#   )
# output_df6 <- read_csv(here::here("outputs", "connectivity_4x4_4.csv")) %>%
#   mutate(
#     mpa_size = 4,
#     mpa_spacing = 4
#   )
# output_df7 <- read_csv(here::here("outputs", "connectivity_4x4_8.csv")) %>%
#   mutate(
#     mpa_size = 4,
#     mpa_spacing = 8
#   )
# output_df8 <- read_csv(here::here("outputs", "connectivity_4x4_16.csv")) %>%
#   mutate(
#     mpa_size = 4,
#     mpa_spacing = 16
#   )
# output_df9 <- read_csv(here::here("outputs", "connectivity_8x8_2.csv")) %>%
#   mutate(
#     mpa_size = 8,
#     mpa_spacing = 2
#   )
# output_df10 <- read_csv(here::here("outputs", "connectivity_8x8_4.csv")) %>%
#   mutate(
#     mpa_size = 8,
#     mpa_spacing = 4
#   )
# output_df11 <- read_csv(here::here("outputs", "connectivity_8x8_8.csv")) %>%
#   mutate(
#     mpa_size = 8,
#     mpa_spacing = 8
#   )
# output_df12 <- read_csv(here::here("outputs", "connectivity_8x8_16.csv")) %>%
#   mutate(
#     mpa_size = 8,
#     mpa_spacing = 16
#   )
# 
# output <- list(
#   output_df1, output_df2, output_df3, output_df4, output_df5, output_df6,
#   output_df7, output_df8, output_df9, output_df10, output_df11, output_df12
# ) %>%
#   reduce(full_join)
# 
# rm(
#   output_df1, output_df2, output_df3, output_df4, output_df5, output_df6,
#   output_df7, output_df8, output_df9, output_df10, output_df11, output_df12
# )
# 
# write_csv(output, here::here("data", "processed_data", "connectivity_results.csv"))

