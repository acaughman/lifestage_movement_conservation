library(tidyverse)
library(patchwork)

# Combine Data ------------------------------------------------------------

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


# Process Data ------------------------------------------------------------

output = read_csv(here::here("data", "processed_data", "model_results.csv"))

mpa <- output %>%
  filter(mpa != "Non-MPA") %>%
  group_by(mpa, mpa_size, mpa_spacing, larval, adult, generation, age) %>%
  summarize(mean_pop = mean(pop, na.rm = TRUE)) %>%
  filter(mpa == "MPA 1") %>% 
  mutate(movement = c(paste0(adult, " / ", larval))) %>%
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


p1 = ggplot(mpa %>% filter(age == "adult") %>% filter(generation == 90)) +
  geom_point(aes(mpa_spacing, mean_pop, color = movement), size = 3) +
  geom_line(aes(mpa_spacing, mean_pop, color = movement), linewidth = 2) +
  theme_bw() +
  facet_wrap(~ mpa_size, nrow = 1, scales = "free_x") +
  labs(
    x = "MPA Spacing",
    y = "Average Population Size in MPA"
  ) +
  scale_color_viridis_d()

ggsave(p1, path = here::here("figs"), file = paste0("mpa_spacing.pdf"), height = 8, width = 12, limitsize = FALSE)

p2 = ggplot(mpa %>% filter(age == "adult") %>% filter(generation == 90)) +
  geom_point(aes(mpa_size, mean_pop, color = movement), size = 3) +
  geom_line(aes(mpa_size, mean_pop, color = movement), linewidth = 2) +
  theme_bw() +
  facet_wrap(~ mpa_spacing, nrow = 1, scales = "free_x") +
  labs(
    x = "MPA Size",
    y = "Average Population Size in MPA"
  ) +
  scale_color_viridis_d()

ggsave(p2, path = here::here("figs"), file = paste0("mpa_size.pdf"), height = 8, width = 12, limitsize = FALSE)
