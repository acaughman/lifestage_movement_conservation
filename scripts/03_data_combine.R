library(tidyverse)

# Combine Biomass Data ------------------------------------------------------------

output_df1 <- read_csv(here::here("outputs", "2x2_0_lowF.csv")) %>%
  mutate(
    mpa_size = 2,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(25, 26) & lon %in% c(25, 26)) ~ "MPA 1",
      (lat %in% c(25, 26) & lon %in% c(22, 23)) ~ "Reference",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  mutate(sensitivity = "fishing") %>% 
  filter(mpa != "Non-MPA") 

output_df2 <- read_csv(here::here("outputs", "2x2_0_lowR0.csv")) %>%
  mutate(
    mpa_size = 2,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(25, 26) & lon %in% c(25, 26)) ~ "MPA 1",
      (lat %in% c(25, 26) & lon %in% c(22, 23)) ~ "Reference",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "reproductive") %>% 
  rbind(output_df1)

rm(output_df1)

output_df3 <- read_csv(here::here("outputs", "2x2_0_lowdd.csv")) %>%
  mutate(
    mpa_size = 2,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(25, 26) & lon %in% c(25, 26)) ~ "MPA 1",
      (lat %in% c(25, 26) & lon %in% c(22, 23)) ~ "Reference",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "density_dependence") %>% 
  rbind(output_df2)

rm(output_df2)

output_df4 <- read_csv(here::here("outputs", "2x2_0_medF.csv")) %>%
  mutate(
    mpa_size = 2,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(25, 26) & lon %in% c(25, 26)) ~ "MPA 1",
      (lat %in% c(25, 26) & lon %in% c(22, 23)) ~ "Reference",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  mutate(sensitivity = "fishing") %>% 
  filter(mpa != "Non-MPA") %>%
  rbind(output_df3)

rm(output_df3)

output_df5 <- read_csv(here::here("outputs", "2x2_0_medR0.csv")) %>%
  mutate(
    mpa_size = 2,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(25, 26) & lon %in% c(25, 26)) ~ "MPA 1",
      (lat %in% c(25, 26) & lon %in% c(22, 23)) ~ "Reference",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "reproductive") %>%
  rbind(output_df4)

rm(output_df4)

output_df6 <- read_csv(here::here("outputs", "2x2_0_meddd.csv")) %>%
  mutate(
    mpa_size = 2,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(25, 26) & lon %in% c(25, 26)) ~ "MPA 1",
      (lat %in% c(25, 26) & lon %in% c(22, 23)) ~ "Reference",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "density_dependence") %>%
  rbind(output_df5)

rm(output_df5)

output_df7 <- read_csv(here::here("outputs", "2x2_0_highF.csv")) %>%
  mutate(
    mpa_size = 2,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(25, 26) & lon %in% c(25, 26)) ~ "MPA 1",
      (lat %in% c(25, 26) & lon %in% c(22, 23)) ~ "Reference",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  mutate(sensitivity = "fishing") %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df6)

rm(output_df6)

output_df8 <- read_csv(here::here("outputs", "2x2_0_highR0.csv")) %>%
  mutate(
    mpa_size = 2,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(25, 26) & lon %in% c(25, 26)) ~ "MPA 1",
      (lat %in% c(25, 26) & lon %in% c(22, 23)) ~ "Reference",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "reproductive") %>%
  rbind(output_df7)

rm(output_df7)

output_df9 <- read_csv(here::here("outputs", "2x2_0_highdd.csv")) %>%
  mutate(
    mpa_size = 2,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(25, 26) & lon %in% c(25, 26)) ~ "MPA 1",
      (lat %in% c(25, 26) & lon %in% c(22, 23)) ~ "Reference",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  mutate(sensitivity = "density_dependence") %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df8)

rm(output_df8)

output_df1 <- read_csv(here::here("outputs", "4x4_0_lowF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(24:27) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(24:27) & lon %in% c(19:22)) ~ "Reference",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  mutate(sensitivity = "fishing") %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df9)

rm(output_df9)

output_df2 <- read_csv(here::here("outputs", "4x4_0_lowR0.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(24:27) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(24:27) & lon %in% c(19:22)) ~ "Reference",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "reproductive") %>%
  rbind(output_df1)

rm(output_df1)

output_df3 <- read_csv(here::here("outputs", "4x4_0_lowdd.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(24:27) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(24:27) & lon %in% c(19:22)) ~ "Reference",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "density_dependence") %>%
  rbind(output_df2)

rm(output_df2)

output_df4 <- read_csv(here::here("outputs", "4x4_0_medF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(24:27) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(24:27) & lon %in% c(19:22)) ~ "Reference",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  mutate(sensitivity = "fishing") %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df3)

rm(output_df3)

output_df5 <- read_csv(here::here("outputs", "4x4_0_medR0.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(24:27) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(24:27) & lon %in% c(19:22)) ~ "Reference",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "reproductive") %>%
  rbind(output_df4)

rm(output_df4)

output_df6 <- read_csv(here::here("outputs", "4x4_0_meddd.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(24:27) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(24:27) & lon %in% c(19:22)) ~ "Reference",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "density_dependence") %>%
  rbind(output_df5)

rm(output_df5)

output_df7 <- read_csv(here::here("outputs", "4x4_0_highF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(24:27) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(24:27) & lon %in% c(19:22)) ~ "Reference",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "fishing") %>%
  rbind(output_df6)

rm(output_df6)

output_df8 <- read_csv(here::here("outputs", "4x4_0_highR0.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(24:27) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(24:27) & lon %in% c(19:22)) ~ "Reference",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "reproductive") %>%
  rbind(output_df7)

rm(output_df7)

output_df9 <- read_csv(here::here("outputs", "4x4_0_highdd.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(24:27) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(24:27) & lon %in% c(19:22)) ~ "Reference",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "density_dependence") %>%
  rbind(output_df8)

rm(output_df8)

output_df1 <- read_csv(here::here("outputs", "8x8_0_lowF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(22:29) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(22:29) & lon %in% c(13:20)) ~ "Reference",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "fishing") %>%
  rbind(output_df9)

rm(output_df9)

output_df2 <- read_csv(here::here("outputs", "8x8_0_lowR0.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(22:29) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(22:29) & lon %in% c(13:20)) ~ "Reference",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  mutate(sensitivity = "reproductive") %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df1)

rm(output_df1)

output_df3 <- read_csv(here::here("outputs", "8x8_0_lowdd.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(22:29) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(22:29) & lon %in% c(13:20)) ~ "Reference",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "density_dependence") %>%
  rbind(output_df2)

rm(output_df2)

output_df4 <- read_csv(here::here("outputs", "8x8_0_medF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(22:29) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(22:29) & lon %in% c(13:20)) ~ "Reference",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "fishing") %>%
  rbind(output_df3)

rm(output_df3)

output_df5 <- read_csv(here::here("outputs", "8x8_0_medR0.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(22:29) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(22:29) & lon %in% c(13:20)) ~ "Reference",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "reproductive") %>%
  rbind(output_df4)

rm(output_df4)

output_df6 <- read_csv(here::here("outputs", "8x8_0_meddd.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(22:29) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(22:29) & lon %in% c(13:20)) ~ "Reference",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "density_dependence") %>%
  rbind(output_df5)

rm(output_df5)

output_df7 <- read_csv(here::here("outputs", "8x8_0_highF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(22:29) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(22:29) & lon %in% c(13:20)) ~ "Reference",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "fishing") %>%
  rbind(output_df6)

rm(output_df6)

output_df8 <- read_csv(here::here("outputs", "8x8_0_highR0.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(22:29) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(22:29) & lon %in% c(13:20)) ~ "Reference",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "reproductive") %>%
  rbind(output_df7)

rm(output_df7)

output_df9 <- read_csv(here::here("outputs", "8x8_0_highdd.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(22:29) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(22:29) & lon %in% c(13:20)) ~ "Reference",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  mutate(sensitivity = "density_dependence") %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df8)

rm(output_df8)

output_df1 <- read_csv(here::here("outputs", "16x16_0_lowF.csv")) %>%
  mutate(
    mpa_size = 16,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(17:32) & lon %in% c(17:32)) ~ "MPA 1",
      (lat %in% c(17:32) & lon %in% c(1:15)) ~ "Reference",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "fishing") %>%
  rbind(output_df9)

rm(output_df9)

output_df2 <- read_csv(here::here("outputs", "16x16_0_lowR0.csv")) %>%
  mutate(
    mpa_size = 16,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(17:32) & lon %in% c(17:32)) ~ "MPA 1",
      (lat %in% c(17:32) & lon %in% c(1:15)) ~ "Reference",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "reproductive") %>%
  rbind(output_df1)

rm(output_df1)

output_df3 <- read_csv(here::here("outputs", "16x16_0_lowdd.csv")) %>%
  mutate(
    mpa_size = 16,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(17:32) & lon %in% c(17:32)) ~ "MPA 1",
      (lat %in% c(17:32) & lon %in% c(1:15)) ~ "Reference",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "density_dependence") %>%
  rbind(output_df2)

rm(output_df2)

output_df4 <- read_csv(here::here("outputs", "16x16_0_medF.csv")) %>%
  mutate(
    mpa_size = 16,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(17:32) & lon %in% c(17:32)) ~ "MPA 1",
      (lat %in% c(17:32) & lon %in% c(1:15)) ~ "Reference",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "fishing") %>%
  rbind(output_df3)

rm(output_df3)

output_df5 <- read_csv(here::here("outputs", "16x16_0_medR0.csv")) %>%
  mutate(
    mpa_size = 16,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(17:32) & lon %in% c(17:32)) ~ "MPA 1",
      (lat %in% c(17:32) & lon %in% c(1:15)) ~ "Reference",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "reproductive") %>%
  rbind(output_df4)

rm(output_df4)

output_df6 <- read_csv(here::here("outputs", "16x16_0_meddd.csv")) %>%
  mutate(
    mpa_size = 16,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(17:32) & lon %in% c(17:32)) ~ "MPA 1",
      (lat %in% c(17:32) & lon %in% c(1:15)) ~ "Reference",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "density_dependence") %>%
  rbind(output_df5)

rm(output_df5)

output_df7 <- read_csv(here::here("outputs", "16x16_0_highF.csv")) %>%
  mutate(
    mpa_size = 16,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(17:32) & lon %in% c(17:32)) ~ "MPA 1",
      (lat %in% c(17:32) & lon %in% c(1:15)) ~ "Reference",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "fishing") %>%
  rbind(output_df6)

rm(output_df6)

output_df8 <- read_csv(here::here("outputs", "16x16_0_highR0.csv")) %>%
  mutate(
    mpa_size = 16,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(17:32) & lon %in% c(17:32)) ~ "MPA 1",
      (lat %in% c(17:32) & lon %in% c(1:15)) ~ "Reference",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "reproductive") %>%
  rbind(output_df7)

rm(output_df7)

output_df9 <- read_csv(here::here("outputs", "16x16_0_highdd.csv")) %>%
  mutate(
    mpa_size = 16,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(17:32) & lon %in% c(17:32)) ~ "MPA 1",
      (lat %in% c(17:32) & lon %in% c(1:15)) ~ "Reference",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  mutate(sensitivity = "density_dependence") %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df8)

rm(output_df8)

output_df1 <- read_csv(here::here("outputs", "4x4_2_lowF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 2,
    mpa = case_when(
      (lat %in% c(21:24) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(21:24) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(27:30) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "fishing") %>%
  rbind(output_df9)

rm(output_df9)

output_df2 <- read_csv(here::here("outputs", "4x4_2_lowR0.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 2,
    mpa = case_when(
      (lat %in% c(21:24) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(21:24) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(27:30) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "reproductive") %>%
  rbind(output_df1)

rm(output_df1)

output_df3 <- read_csv(here::here("outputs", "4x4_2_lowdd.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 2,
    mpa = case_when(
      (lat %in% c(21:24) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(21:24) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(27:30) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  mutate(sensitivity = "density_dependence") %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df2)

rm(output_df2)

output_df4 <- read_csv(here::here("outputs", "4x4_2_medF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 2,
    mpa = case_when(
      (lat %in% c(21:24) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(21:24) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(27:30) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "fishing") %>%
  rbind(output_df3)

rm(output_df3)

output_df5 <- read_csv(here::here("outputs", "4x4_2_medR0.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 2,
    mpa = case_when(
      (lat %in% c(21:24) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(21:24) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(27:30) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "reproductive") %>%
  rbind(output_df4)

rm(output_df4)

output_df6 <- read_csv(here::here("outputs", "4x4_2_meddd.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 2,
    mpa = case_when(
      (lat %in% c(21:24) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(21:24) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(27:30) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "density_dependence") %>%
  rbind(output_df5)

rm(output_df5)

output_df7 <- read_csv(here::here("outputs", "4x4_2_highF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 2,
    mpa = case_when(
      (lat %in% c(21:24) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(21:24) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(27:30) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "fishing") %>%
  rbind(output_df6)

rm(output_df6)

output_df8 <- read_csv(here::here("outputs", "4x4_2_highR0.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 2,
    mpa = case_when(
      (lat %in% c(21:24) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(21:24) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(27:30) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "reproductive") %>%
  rbind(output_df7)

rm(output_df7)

output_df9 <- read_csv(here::here("outputs", "4x4_2_highdd.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 2,
    mpa = case_when(
      (lat %in% c(21:24) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(21:24) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(27:30) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "density_dependence") %>%
  rbind(output_df8)

rm(output_df8)

output_df1 <- read_csv(here::here("outputs", "4x4_4_lowF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 4,
    mpa = case_when(
      (lat %in% c(20:23) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(20:23) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(28:31) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "fishing") %>%
  rbind(output_df9)

rm(output_df9)

output_df2 <- read_csv(here::here("outputs", "4x4_4_lowR0.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 4,
    mpa = case_when(
      (lat %in% c(20:23) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(20:23) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(28:31) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "reproductive") %>%
  rbind(output_df1)

rm(output_df1)

output_df3 <- read_csv(here::here("outputs", "4x4_4_lowdd.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 4,
    mpa = case_when(
      (lat %in% c(20:23) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(20:23) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(28:31) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "density_dependence") %>%
  rbind(output_df2)

rm(output_df2)

output_df4 <- read_csv(here::here("outputs", "4x4_4_medF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 4,
    mpa = case_when(
      (lat %in% c(20:23) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(20:23) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(28:31) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "fishing") %>%
  rbind(output_df3)

rm(output_df3)

output_df5 <- read_csv(here::here("outputs", "4x4_4_medR0.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 4,
    mpa = case_when(
      (lat %in% c(20:23) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(20:23) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(28:31) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "reproductive") %>%
  rbind(output_df4)

rm(output_df4)

output_df6 <- read_csv(here::here("outputs", "4x4_4_meddd.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 4,
    mpa = case_when(
      (lat %in% c(20:23) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(20:23) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(28:31) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "density_dependence") %>%
  rbind(output_df5)

rm(output_df5)

output_df7 <- read_csv(here::here("outputs", "4x4_4_highF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 4,
    mpa = case_when(
      (lat %in% c(20:23) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(20:23) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(28:31) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  mutate(sensitivity = "fishing") %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df6)

rm(output_df6)

output_df8 <- read_csv(here::here("outputs", "4x4_4_highR0.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 4,
    mpa = case_when(
      (lat %in% c(20:23) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(20:23) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(28:31) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  mutate(sensitivity = "reproductive") %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df7)

rm(output_df7)

output_df9 <- read_csv(here::here("outputs", "4x4_4_highdd.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 4,
    mpa = case_when(
      (lat %in% c(20:23) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(20:23) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(28:31) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  mutate(sensitivity = "density_dependence") %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df8)

rm(output_df8)
gc()

output_df1 <- read_csv(here::here("outputs", "4x4_8_lowF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 8,
    mpa = case_when(
      (lat %in% c(18:21) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(18:21) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(30:33) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "fishing") %>%
  rbind(output_df9)

rm(output_df9)

output_df2 <- read_csv(here::here("outputs", "4x4_8_lowR0.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 8,
    mpa = case_when(
      (lat %in% c(18:21) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(18:21) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(30:33) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "reproductive") %>%
  rbind(output_df1)

rm(output_df1)

output_df3 <- read_csv(here::here("outputs", "4x4_8_lowdd.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 8,
    mpa = case_when(
      (lat %in% c(18:21) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(18:21) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(30:33) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "density_dependence") %>%
  rbind(output_df2)

rm(output_df2)

output_df4 <- read_csv(here::here("outputs", "4x4_8_medF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 8,
    mpa = case_when(
      (lat %in% c(18:21) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(18:21) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(30:33) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "fishing") %>%
  rbind(output_df3)

rm(output_df3)

output_df5 <- read_csv(here::here("outputs", "4x4_8_medR0.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 8,
    mpa = case_when(
      (lat %in% c(18:21) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(18:21) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(30:33) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "reproductive") %>%
  rbind(output_df4)

rm(output_df4)

output_df6 <- read_csv(here::here("outputs", "4x4_8_meddd.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 8,
    mpa = case_when(
      (lat %in% c(18:21) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(18:21) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(30:33) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "density_dependence") %>%
  rbind(output_df5)

rm(output_df5)

output_df7 <- read_csv(here::here("outputs", "4x4_8_highF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 8,
    mpa = case_when(
      (lat %in% c(18:21) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(18:21) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(30:33) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "fishing") %>%
  rbind(output_df6)

rm(output_df6)

output_df8 <- read_csv(here::here("outputs", "4x4_8_highR0.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 8,
    mpa = case_when(
      (lat %in% c(18:21) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(18:21) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(30:33) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "reproductive") %>%
  rbind(output_df7)

rm(output_df7)

output_df9 <- read_csv(here::here("outputs", "4x4_8_highdd.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 8,
    mpa = case_when(
      (lat %in% c(18:21) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(18:21) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(30:33) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "density_dependence") %>%
  rbind(output_df8)

rm(output_df8)

output_df1 <- read_csv(here::here("outputs", "4x4_16_lowF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 16,
    mpa = case_when(
      (lat %in% c(14:17) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(14:17) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(34:37) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "fishing") %>%
  rbind(output_df9)

rm(output_df9)

output_df2 <- read_csv(here::here("outputs", "4x4_16_lowR0.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 16,
    mpa = case_when(
      (lat %in% c(14:17) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(14:17) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(34:37) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "reproductive") %>%
  rbind(output_df1)

rm(output_df1)

output_df3 <- read_csv(here::here("outputs", "4x4_16_lowdd.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 16,
    mpa = case_when(
      (lat %in% c(14:17) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(14:17) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(34:37) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "density_dependence") %>%
  rbind(output_df2)

rm(output_df2)

output_df4 <- read_csv(here::here("outputs", "4x4_16_medF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 16,
    mpa = case_when(
      (lat %in% c(14:17) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(14:17) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(34:37) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  mutate(sensitivity = "fishing") %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df3)

rm(output_df3)

output_df5 <- read_csv(here::here("outputs", "4x4_16_medR0.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 16,
    mpa = case_when(
      (lat %in% c(14:17) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(14:17) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(34:37) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "reproductive") %>%
  rbind(output_df4)

rm(output_df4)

output_df6 <- read_csv(here::here("outputs", "4x4_16_meddd.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 16,
    mpa = case_when(
      (lat %in% c(14:17) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(14:17) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(34:37) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "density_dependence") %>%
  rbind(output_df5)

rm(output_df5)

output_df7 <- read_csv(here::here("outputs", "4x4_16_highF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 16,
    mpa = case_when(
      (lat %in% c(14:17) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(14:17) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(34:37) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "fishing") %>%
  rbind(output_df6)

rm(output_df6)

output_df8 <- read_csv(here::here("outputs", "4x4_16_highR0.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 16,
    mpa = case_when(
      (lat %in% c(14:17) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(14:17) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(34:37) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "reproductive") %>%
  rbind(output_df7)

rm(output_df7)

output_df9 <- read_csv(here::here("outputs", "4x4_16_highdd.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 16,
    mpa = case_when(
      (lat %in% c(14:17) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(14:17) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(34:37) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "density_dependence") %>%
  rbind(output_df8)

rm(output_df8)

output_df1 <- read_csv(here::here("outputs", "8x8_2_lowF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 2,
    mpa = case_when(
      (lat %in% c(17:24) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(17:24) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(27:34) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "fishing") %>%
  rbind(output_df9)

rm(output_df9)

output_df2 <- read_csv(here::here("outputs", "8x8_2_lowR0.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 2,
    mpa = case_when(
      (lat %in% c(17:24) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(17:24) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(27:34) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "reproductive") %>%
  rbind(output_df1)

rm(output_df1)

output_df3 <- read_csv(here::here("outputs", "8x8_2_lowdd.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 2,
    mpa = case_when(
      (lat %in% c(17:24) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(17:24) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(27:34) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "density_dependence") %>%
  rbind(output_df2)

rm(output_df2)

output_df4 <- read_csv(here::here("outputs", "8x8_2_medF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 2,
    mpa = case_when(
      (lat %in% c(17:24) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(17:24) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(27:34) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "fishing") %>%
  rbind(output_df3)

rm(output_df3)

output_df5 <- read_csv(here::here("outputs", "8x8_2_medR0.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 2,
    mpa = case_when(
      (lat %in% c(17:24) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(17:24) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(27:34) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "reproductive") %>%
  rbind(output_df4)

rm(output_df4)

output_df6 <- read_csv(here::here("outputs", "8x8_2_meddd.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 2,
    mpa = case_when(
      (lat %in% c(17:24) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(17:24) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(27:34) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "density_dependence") %>%
  rbind(output_df5)

rm(output_df5)

output_df7 <- read_csv(here::here("outputs", "8x8_2_highF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 2,
    mpa = case_when(
      (lat %in% c(17:24) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(17:24) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(27:34) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "fishing") %>%
  rbind(output_df6)

rm(output_df6)

output_df8 <- read_csv(here::here("outputs", "8x8_2_highR0.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 2,
    mpa = case_when(
      (lat %in% c(17:24) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(17:24) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(27:34) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "reproductive") %>%
  rbind(output_df7)

rm(output_df7)

output_df9 <- read_csv(here::here("outputs", "8x8_2_highdd.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 2,
    mpa = case_when(
      (lat %in% c(17:24) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(17:24) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(27:34) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  mutate(sensitivity = "density_dependence") %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df8)

rm(output_df8)
gc()

output_df1 <- read_csv(here::here("outputs", "8x8_4_lowF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 4,
    mpa = case_when(
      (lat %in% c(16:23) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(16:23) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(28:35) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "fishing") %>%
  rbind(output_df9)

rm(output_df9)

output_df2 <- read_csv(here::here("outputs", "8x8_4_lowR0.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 4,
    mpa = case_when(
      (lat %in% c(16:23) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(16:23) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(28:35) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  mutate(sensitivity = "reproductive") %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df1)

rm(output_df1)

output_df3 <- read_csv(here::here("outputs", "8x8_4_lowdd.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 4,
    mpa = case_when(
      (lat %in% c(16:23) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(16:23) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(28:35) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "density_dependence") %>%
  rbind(output_df2)

rm(output_df2)

output_df4 <- read_csv(here::here("outputs", "8x8_4_medF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 4,
    mpa = case_when(
      (lat %in% c(16:23) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(16:23) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(28:35) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "fishing") %>%
  rbind(output_df3)

rm(output_df3)

output_df5 <- read_csv(here::here("outputs", "8x8_4_medR0.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 4,
    mpa = case_when(
      (lat %in% c(16:23) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(16:23) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(28:35) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "reproductive") %>%
  rbind(output_df4)

rm(output_df4)

output_df6 <- read_csv(here::here("outputs", "8x8_4_meddd.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 4,
    mpa = case_when(
      (lat %in% c(16:23) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(16:23) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(28:35) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  mutate(sensitivity = "density_dependence") %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df5)

rm(output_df5)

output_df7 <- read_csv(here::here("outputs", "8x8_4_highF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 4,
    mpa = case_when(
      (lat %in% c(16:23) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(16:23) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(28:35) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "fishing") %>%
  rbind(output_df6)

rm(output_df6)

output_df8 <- read_csv(here::here("outputs", "8x8_4_highR0.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 4,
    mpa = case_when(
      (lat %in% c(16:23) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(16:23) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(28:35) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "reproductive") %>%
  rbind(output_df7)

rm(output_df7)

output_df9 <- read_csv(here::here("outputs", "8x8_4_highdd.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 4,
    mpa = case_when(
      (lat %in% c(16:23) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(16:23) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(28:35) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "density_dependence") %>%
  rbind(output_df8)

rm(output_df8)

output_df1 <- read_csv(here::here("outputs", "8x8_8_lowF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    mpa = case_when(
      (lat %in% c(14:21) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(14:21) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(30:37) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "fishing") %>%
  rbind(output_df9)

rm(output_df9)

output_df2 <- read_csv(here::here("outputs", "8x8_8_lowR0.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    mpa = case_when(
      (lat %in% c(14:21) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(14:21) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(30:37) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "reproductive") %>%
  rbind(output_df1)

rm(output_df1)

output_df3 <- read_csv(here::here("outputs", "8x8_8_lowdd.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    mpa = case_when(
      (lat %in% c(14:21) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(14:21) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(30:37) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "density_dependence") %>%
  rbind(output_df2)

rm(output_df2)

output_df4 <- read_csv(here::here("outputs", "8x8_8_medF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    mpa = case_when(
      (lat %in% c(14:21) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(14:21) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(30:37) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "fishing") %>%
  rbind(output_df3)

rm(output_df3)

output_df5 <- read_csv(here::here("outputs", "8x8_8_medR0.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    mpa = case_when(
      (lat %in% c(14:21) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(14:21) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(30:37) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "reproductive") %>%
  rbind(output_df4)

rm(output_df4)

output_df6 <- read_csv(here::here("outputs", "8x8_8_meddd.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    mpa = case_when(
      (lat %in% c(14:21) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(14:21) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(30:37) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "density_dependence") %>%
  rbind(output_df5)

rm(output_df5)

output_df7 <- read_csv(here::here("outputs", "8x8_8_highF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    mpa = case_when(
      (lat %in% c(14:21) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(14:21) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(30:37) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "fishing") %>%
  rbind(output_df6)

rm(output_df6)

output_df8 <- read_csv(here::here("outputs", "8x8_8_highR0.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    mpa = case_when(
      (lat %in% c(14:21) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(14:21) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(30:37) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "reproductive") %>%
  rbind(output_df7)

rm(output_df7)

output_df9 <- read_csv(here::here("outputs", "8x8_8_highdd.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    mpa = case_when(
      (lat %in% c(14:21) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(14:21) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(30:37) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "density_dependence") %>%
  rbind(output_df8)

rm(output_df8)

output_df1 <- read_csv(here::here("outputs", "8x8_16_lowF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 16,
    mpa = case_when(
      (lat %in% c(10:17) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(10:17) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(34:41) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "fishing") %>%
  rbind(output_df9)

rm(output_df9)

output_df2 <- read_csv(here::here("outputs", "8x8_16_lowR0.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 16,
    mpa = case_when(
      (lat %in% c(10:17) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(10:17) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(34:41) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "reproductive") %>%
  rbind(output_df1)

rm(output_df1)

output_df3 <- read_csv(here::here("outputs", "8x8_16_lowdd.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 16,
    mpa = case_when(
      (lat %in% c(10:17) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(10:17) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(34:41) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "density_dependence") %>%
  rbind(output_df2)

rm(output_df2)

output_df4 <- read_csv(here::here("outputs", "8x8_16_medF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 16,
    mpa = case_when(
      (lat %in% c(10:17) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(10:17) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(34:41) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "fishing") %>%
  rbind(output_df3)

rm(output_df3)

output_df5 <- read_csv(here::here("outputs", "8x8_16_medR0.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 16,
    mpa = case_when(
      (lat %in% c(10:17) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(10:17) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(34:41) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "reproductive") %>%
  rbind(output_df4)

rm(output_df4)

output_df6 <- read_csv(here::here("outputs", "8x8_16_meddd.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 16,
    mpa = case_when(
      (lat %in% c(10:17) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(10:17) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(34:41) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "density_dependence") %>%
  rbind(output_df5)

rm(output_df5)

output_df7 <- read_csv(here::here("outputs", "8x8_16_highF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 16,
    mpa = case_when(
      (lat %in% c(10:17) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(10:17) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(34:41) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "fishing") %>%
  rbind(output_df6)

rm(output_df6)

output_df8 <- read_csv(here::here("outputs", "8x8_16_highR0.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 16,
    mpa = case_when(
      (lat %in% c(10:17) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(10:17) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(34:41) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "reproductive") %>%
  rbind(output_df7)

rm(output_df7)

output_df9 <- read_csv(here::here("outputs", "8x8_16_highdd.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 16,
    mpa = case_when(
      (lat %in% c(10:17) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(10:17) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(34:41) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  ) %>%
  filter(mpa != "Non-MPA") %>%
  mutate(sensitivity = "density_dependence") %>%
  rbind(output_df8)

rm(output_df8)
gc()

write_csv(output_df9, here::here("data", "processed_data", "model_results.csv"))

# Combine Connectivity Data -----------------------------------------------

output_df1 <- read_csv(here::here("outputs", "connectivity_2x2_0_lowF.csv")) %>%
  mutate(sensitivity = "fishing") %>%
  mutate(sen_value = "low") %>%
  mutate(
    mpa_size = 2,
    mpa_spacing = 0
  )

output_df2 <- read_csv(here::here("outputs", "connectivity_2x2_0_lowR0.csv")) %>%
  mutate(sensitivity = "reproduction") %>%
  mutate(sen_value = "low") %>%
  mutate(
    mpa_size = 2,
    mpa_spacing = 0
  ) %>%
  rbind(output_df1)

rm(output_df1)

output_df3 <- read_csv(here::here("outputs", "connectivity_2x2_0_lowdd.csv")) %>%
  mutate(sensitivity = "density_dependence") %>%
  mutate(sen_value = "low") %>%
  mutate(
    mpa_size = 2,
    mpa_spacing = 0
  ) %>%
  rbind(output_df2)

rm(output_df2)

output_df4 <- read_csv(here::here("outputs", "connectivity_2x2_0_medF.csv")) %>%
  mutate(sensitivity = "fishing") %>%
  mutate(sen_value = "med") %>%
  mutate(
    mpa_size = 2,
    mpa_spacing = 0
  ) %>%
  rbind(output_df3)

rm(output_df3)

output_df5 <- read_csv(here::here("outputs", "connectivity_2x2_0_medR0.csv")) %>%
  mutate(sensitivity = "reproduction") %>%
  mutate(sen_value = "med") %>%
  mutate(
    mpa_size = 2,
    mpa_spacing = 0
  ) %>%
  rbind(output_df4)

rm(output_df4)

output_df6 <- read_csv(here::here("outputs", "connectivity_2x2_0_meddd.csv")) %>%
  mutate(sensitivity = "density_dependence") %>%
  mutate(sen_value = "med") %>%
  mutate(
    mpa_size = 2,
    mpa_spacing = 0
  ) %>%
  rbind(output_df5)

rm(output_df5)

output_df7 <- read_csv(here::here("outputs", "connectivity_2x2_0_highF.csv")) %>%
  mutate(sensitivity = "fishing") %>%
  mutate(sen_value = "high") %>%
  mutate(
    mpa_size = 2,
    mpa_spacing = 0
  ) %>%
  rbind(output_df6)

rm(output_df6)

output_df8 <- read_csv(here::here("outputs", "connectivity_2x2_0_highR0.csv")) %>%
  mutate(sensitivity = "reproduction") %>%
  mutate(sen_value = "high") %>%
  mutate(
    mpa_size = 2,
    mpa_spacing = 0
  ) %>%
  rbind(output_df7)

rm(output_df7)

output_df9 <- read_csv(here::here("outputs", "connectivity_2x2_0_highdd.csv")) %>%
  mutate(sensitivity = "density_dependence") %>%
  mutate(sen_value = "high") %>%
  mutate(
    mpa_size = 2,
    mpa_spacing = 0
  ) %>%
  rbind(output_df8)

rm(output_df8)

output_df1 <- read_csv(here::here("outputs", "connectivity_4x4_0_lowF.csv")) %>%
  mutate(sensitivity = "fishing") %>%
  mutate(sen_value = "low") %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 0
  ) %>%
  rbind(output_df9)

rm(output_df9)

output_df2 <- read_csv(here::here("outputs", "connectivity_4x4_0_lowR0.csv")) %>%
  mutate(sensitivity = "reproduction") %>%
  mutate(sen_value = "low") %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 0
  ) %>%
  rbind(output_df1)

rm(output_df1)

output_df3 <- read_csv(here::here("outputs", "connectivity_4x4_0_lowdd.csv")) %>%
  mutate(sensitivity = "density_dependence") %>%
  mutate(sen_value = "low") %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 0
  ) %>%
  rbind(output_df2)

rm(output_df2)

output_df4 <- read_csv(here::here("outputs", "connectivity_4x4_0_medF.csv")) %>%
  mutate(sensitivity = "fishing") %>%
  mutate(sen_value = "med") %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 0
  ) %>%
  rbind(output_df3)

rm(output_df3)

output_df5 <- read_csv(here::here("outputs", "connectivity_4x4_0_medR0.csv")) %>%
  mutate(sensitivity = "reproduction") %>%
  mutate(sen_value = "med") %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 0
  ) %>%
  rbind(output_df4)

rm(output_df4)

output_df6 <- read_csv(here::here("outputs", "connectivity_4x4_0_meddd.csv")) %>%
  mutate(sensitivity = "density_dependence") %>%
  mutate(sen_value = "med") %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 0
  ) %>%
  rbind(output_df5)

rm(output_df5)

output_df7 <- read_csv(here::here("outputs", "connectivity_4x4_0_highF.csv")) %>%
  mutate(sensitivity = "fishing") %>%
  mutate(sen_value = "high") %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 0
  ) %>%
  rbind(output_df6)

rm(output_df6)

output_df8 <- read_csv(here::here("outputs", "connectivity_4x4_0_highR0.csv")) %>%
  mutate(sensitivity = "reproduction") %>%
  mutate(sen_value = "high") %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 0
  ) %>%
  rbind(output_df7)

rm(output_df7)

output_df9 <- read_csv(here::here("outputs", "connectivity_4x4_0_highdd.csv")) %>%
  mutate(sensitivity = "density_dependence") %>%
  mutate(sen_value = "high") %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 0
  ) %>%
  rbind(output_df8)

rm(output_df8)

output_df1 <- read_csv(here::here("outputs", "connectivity_8x8_0_lowF.csv")) %>%
  mutate(sensitivity = "fishing") %>%
  mutate(sen_value = "low") %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0
  ) %>%
  rbind(output_df9)

rm(output_df9)

output_df2 <- read_csv(here::here("outputs", "connectivity_8x8_0_lowR0.csv")) %>%
  mutate(sensitivity = "reproduction") %>%
  mutate(sen_value = "low") %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0
  ) %>%
  rbind(output_df1)

rm(output_df1)

output_df3 <- read_csv(here::here("outputs", "connectivity_8x8_0_lowdd.csv")) %>%
  mutate(sensitivity = "density_dependence") %>%
  mutate(sen_value = "low") %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0
  ) %>%
  rbind(output_df2)

rm(output_df2)

output_df4 <- read_csv(here::here("outputs", "connectivity_8x8_0_medF.csv")) %>%
  mutate(sensitivity = "fishing") %>%
  mutate(sen_value = "med") %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0
  ) %>%
  rbind(output_df3)

rm(output_df3)

output_df5 <- read_csv(here::here("outputs", "connectivity_8x8_0_medR0.csv")) %>%
  mutate(sensitivity = "reproduction") %>%
  mutate(sen_value = "med") %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0
  ) %>%
  rbind(output_df4)

rm(output_df4)

output_df6 <- read_csv(here::here("outputs", "connectivity_8x8_0_meddd.csv")) %>%
  mutate(sensitivity = "density_dependence") %>%
  mutate(sen_value = "med") %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0
  ) %>%
  rbind(output_df5)

rm(output_df5)

output_df7 <- read_csv(here::here("outputs", "connectivity_8x8_0_highF.csv")) %>%
  mutate(sensitivity = "fishing") %>%
  mutate(sen_value = "high") %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0
  ) %>%
  rbind(output_df6)

rm(output_df6)

output_df8 <- read_csv(here::here("outputs", "connectivity_8x8_0_highR0.csv")) %>%
  mutate(sensitivity = "reproduction") %>%
  mutate(sen_value = "high") %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0
  ) %>%
  rbind(output_df7)

rm(output_df7)

output_df9 <- read_csv(here::here("outputs", "connectivity_8x8_0_highdd.csv")) %>%
  mutate(sensitivity = "density_dependence") %>%
  mutate(sen_value = "high") %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0
  ) %>%
  rbind(output_df8)

rm(output_df8)

output_df1 <- read_csv(here::here("outputs", "connectivity_16x16_0_lowF.csv")) %>%
  mutate(sensitivity = "fishing") %>%
  mutate(sen_value = "low") %>%
  mutate(
    mpa_size = 16,
    mpa_spacing = 0
  ) %>%
  rbind(output_df9)

rm(output_df9)

output_df2 <- read_csv(here::here("outputs", "connectivity_16x16_0_lowR0.csv")) %>%
  mutate(sensitivity = "reproduction") %>%
  mutate(sen_value = "low") %>%
  mutate(
    mpa_size = 16,
    mpa_spacing = 0
  ) %>%
  rbind(output_df1)

rm(output_df1)

output_df3 <- read_csv(here::here("outputs", "connectivity_16x16_0_lowdd.csv")) %>%
  mutate(sensitivity = "density_dependence") %>%
  mutate(sen_value = "low") %>%
  mutate(
    mpa_size = 16,
    mpa_spacing = 0
  ) %>%
  rbind(output_df2)

rm(output_df2)

output_df4 <- read_csv(here::here("outputs", "connectivity_16x16_0_medF.csv")) %>%
  mutate(sensitivity = "fishing") %>%
  mutate(sen_value = "med") %>%
  mutate(
    mpa_size = 16,
    mpa_spacing = 0
  ) %>%
  rbind(output_df3)

rm(output_df3)

output_df5 <- read_csv(here::here("outputs", "connectivity_16x16_0_medR0.csv")) %>%
  mutate(sensitivity = "reproduction") %>%
  mutate(sen_value = "med") %>%
  mutate(
    mpa_size = 16,
    mpa_spacing = 0
  ) %>%
  rbind(output_df4)

rm(output_df4)

output_df6 <- read_csv(here::here("outputs", "connectivity_16x16_0_meddd.csv")) %>%
  mutate(sensitivity = "density_dependence") %>%
  mutate(sen_value = "med") %>%
  mutate(
    mpa_size = 16,
    mpa_spacing = 0
  ) %>%
  rbind(output_df5)

rm(output_df5)

output_df7 <- read_csv(here::here("outputs", "connectivity_16x16_0_highF.csv")) %>%
  mutate(sensitivity = "fishing") %>%
  mutate(sen_value = "high") %>%
  mutate(
    mpa_size = 16,
    mpa_spacing = 0
  ) %>%
  rbind(output_df6)

rm(output_df6)

output_df8 <- read_csv(here::here("outputs", "connectivity_16x16_0_highR0.csv")) %>%
  mutate(sensitivity = "reproduction") %>%
  mutate(sen_value = "high") %>%
  mutate(
    mpa_size = 16,
    mpa_spacing = 0
  ) %>%
  rbind(output_df7)

rm(output_df7)

output_df9 <- read_csv(here::here("outputs", "connectivity_16x16_0_highdd.csv")) %>%
  mutate(sensitivity = "density_dependence") %>%
  mutate(sen_value = "high") %>%
  mutate(
    mpa_size = 16,
    mpa_spacing = 0
  ) %>%
  rbind(output_df8)

rm(output_df8)

output_df1 <- read_csv(here::here("outputs", "connectivity_4x4_2_lowF.csv")) %>%
  mutate(sensitivity = "fishing") %>%
  mutate(sen_value = "low") %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 2
  ) %>%
  rbind(output_df9)

rm(output_df9)

output_df2 <- read_csv(here::here("outputs", "connectivity_4x4_2_lowR0.csv")) %>%
  mutate(sensitivity = "reproduction") %>%
  mutate(sen_value = "low") %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 2
  ) %>%
  rbind(output_df1)

rm(output_df1)

output_df3 <- read_csv(here::here("outputs", "connectivity_4x4_2_lowdd.csv")) %>%
  mutate(sensitivity = "density_dependence") %>%
  mutate(sen_value = "low") %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 2
  ) %>%
  rbind(output_df2)

rm(output_df2)

output_df4 <- read_csv(here::here("outputs", "connectivity_4x4_2_medF.csv")) %>%
  mutate(sensitivity = "fishing") %>%
  mutate(sen_value = "med") %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 2
  ) %>%
  rbind(output_df3)

rm(output_df3)

output_df5 <- read_csv(here::here("outputs", "connectivity_4x4_2_medR0.csv")) %>%
  mutate(sensitivity = "reproduction") %>%
  mutate(sen_value = "med") %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 2
  ) %>%
  rbind(output_df4)

rm(output_df4)

output_df6 <- read_csv(here::here("outputs", "connectivity_4x4_2_meddd.csv")) %>%
  mutate(sensitivity = "density_dependence") %>%
  mutate(sen_value = "med") %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 2
  ) %>%
  rbind(output_df5)

rm(output_df5)

output_df7 <- read_csv(here::here("outputs", "connectivity_4x4_2_highF.csv")) %>%
  mutate(sensitivity = "fishing") %>%
  mutate(sen_value = "high") %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 2
  ) %>%
  rbind(output_df6)

rm(output_df6)

output_df8 <- read_csv(here::here("outputs", "connectivity_4x4_2_highR0.csv")) %>%
  mutate(sensitivity = "reproduction") %>%
  mutate(sen_value = "high") %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 2
  ) %>%
  rbind(output_df7)

rm(output_df7)

output_df9 <- read_csv(here::here("outputs", "connectivity_4x4_2_highdd.csv")) %>%
  mutate(sensitivity = "density_dependence") %>%
  mutate(sen_value = "high") %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 2
  ) %>%
  rbind(output_df8)

rm(output_df8)

output_df1 <- read_csv(here::here("outputs", "connectivity_4x4_4_lowF.csv")) %>%
  mutate(sensitivity = "fishing") %>%
  mutate(sen_value = "low") %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 4
  ) %>%
  rbind(output_df9)

rm(output_df9)

output_df2 <- read_csv(here::here("outputs", "connectivity_4x4_4_lowR0.csv")) %>%
  mutate(sensitivity = "reproduction") %>%
  mutate(sen_value = "low") %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 4
  ) %>%
  rbind(output_df1)

rm(output_df1)

output_df3 <- read_csv(here::here("outputs", "connectivity_4x4_4_lowdd.csv")) %>%
  mutate(sensitivity = "density_dependence") %>%
  mutate(sen_value = "low") %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 4
  ) %>%
  rbind(output_df2)

rm(output_df2)

output_df4 <- read_csv(here::here("outputs", "connectivity_4x4_4_medF.csv")) %>%
  mutate(sensitivity = "fishing") %>%
  mutate(sen_value = "med") %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 4
  ) %>%
  rbind(output_df3)

rm(output_df3)

output_df5 <- read_csv(here::here("outputs", "connectivity_4x4_4_medR0.csv")) %>%
  mutate(sensitivity = "reproduction") %>%
  mutate(sen_value = "med") %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 4
  ) %>%
  rbind(output_df4)

rm(output_df4)

output_df6 <- read_csv(here::here("outputs", "connectivity_4x4_4_meddd.csv")) %>%
  mutate(sensitivity = "density_dependence") %>%
  mutate(sen_value = "med") %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 4
  ) %>%
  rbind(output_df5)

rm(output_df5)

output_df7 <- read_csv(here::here("outputs", "connectivity_4x4_4_highF.csv")) %>%
  mutate(sensitivity = "fishing") %>%
  mutate(sen_value = "high") %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 4
  ) %>%
  rbind(output_df6)

rm(output_df6)

output_df8 <- read_csv(here::here("outputs", "connectivity_4x4_4_highR0.csv")) %>%
  mutate(sensitivity = "reproduction") %>%
  mutate(sen_value = "high") %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 4
  ) %>%
  rbind(output_df7)

rm(output_df7)

output_df9 <- read_csv(here::here("outputs", "connectivity_4x4_4_highdd.csv")) %>%
  mutate(sensitivity = "density_dependence") %>%
  mutate(sen_value = "high") %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 4
  ) %>%
  rbind(output_df8)

rm(output_df8)
gc()

output_df1 <- read_csv(here::here("outputs", "connectivity_4x4_8_lowF.csv")) %>%
  mutate(sensitivity = "fishing") %>%
  mutate(sen_value = "low") %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 8
  ) %>%
  rbind(output_df9)

rm(output_df9)

output_df2 <- read_csv(here::here("outputs", "connectivity_4x4_8_lowR0.csv")) %>%
  mutate(sensitivity = "reproduction") %>%
  mutate(sen_value = "low") %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 8
  ) %>%
  rbind(output_df1)

rm(output_df1)

output_df3 <- read_csv(here::here("outputs", "connectivity_4x4_8_lowdd.csv")) %>%
  mutate(sensitivity = "density_dependence") %>%
  mutate(sen_value = "low") %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 8
  ) %>%
  rbind(output_df2)

rm(output_df2)

output_df4 <- read_csv(here::here("outputs", "connectivity_4x4_8_medF.csv")) %>%
  mutate(sensitivity = "fishing") %>%
  mutate(sen_value = "med") %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 8
  ) %>%
  rbind(output_df3)

rm(output_df3)

output_df5 <- read_csv(here::here("outputs", "connectivity_4x4_8_medR0.csv")) %>%
  mutate(sensitivity = "reproduction") %>%
  mutate(sen_value = "med") %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 8
  ) %>%
  rbind(output_df4)

rm(output_df4)

output_df6 <- read_csv(here::here("outputs", "connectivity_4x4_8_meddd.csv")) %>%
  mutate(sensitivity = "density_dependence") %>%
  mutate(sen_value = "med") %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 8
  ) %>%
  rbind(output_df5)

rm(output_df5)

output_df7 <- read_csv(here::here("outputs", "connectivity_4x4_8_highF.csv")) %>%
  mutate(sensitivity = "fishing") %>%
  mutate(sen_value = "high") %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 8
  ) %>%
  rbind(output_df6)

rm(output_df6)

output_df8 <- read_csv(here::here("outputs", "connectivity_4x4_8_highR0.csv")) %>%
  mutate(sensitivity = "reproduction") %>%
  mutate(sen_value = "high") %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 8
  ) %>%
  rbind(output_df7)

rm(output_df7)

output_df9 <- read_csv(here::here("outputs", "connectivity_4x4_8_highdd.csv")) %>%
  mutate(sensitivity = "density_dependence") %>%
  mutate(sen_value = "high") %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 8
  ) %>%
  rbind(output_df8)

rm(output_df8)

output_df1 <- read_csv(here::here("outputs", "connectivity_4x4_16_lowF.csv")) %>%
  mutate(sensitivity = "fishing") %>%
  mutate(sen_value = "low") %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 16
  ) %>%
  rbind(output_df9)

rm(output_df9)

output_df2 <- read_csv(here::here("outputs", "connectivity_4x4_16_lowR0.csv")) %>%
  mutate(sensitivity = "reproduction") %>%
  mutate(sen_value = "low") %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 16
  ) %>%
  rbind(output_df1)

rm(output_df1)

output_df3 <- read_csv(here::here("outputs", "connectivity_4x4_16_lowdd.csv")) %>%
  mutate(sensitivity = "density_dependence") %>%
  mutate(sen_value = "low") %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 16
  ) %>%
  rbind(output_df2)

rm(output_df2)

output_df4 <- read_csv(here::here("outputs", "connectivity_4x4_16_medF.csv")) %>%
  mutate(sensitivity = "fishing") %>%
  mutate(sen_value = "med") %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 16
  ) %>%
  rbind(output_df3)

rm(output_df3)

output_df5 <- read_csv(here::here("outputs", "connectivity_4x4_16_medR0.csv")) %>%
  mutate(sensitivity = "reproduction") %>%
  mutate(sen_value = "med") %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 16
  ) %>%
  rbind(output_df4)

rm(output_df4)

output_df6 <- read_csv(here::here("outputs", "connectivity_4x4_16_meddd.csv")) %>%
  mutate(sensitivity = "density_dependence") %>%
  mutate(sen_value = "med") %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 16
  ) %>%
  rbind(output_df5)

rm(output_df5)

output_df7 <- read_csv(here::here("outputs", "connectivity_4x4_16_highF.csv")) %>%
  mutate(sensitivity = "fishing") %>%
  mutate(sen_value = "high") %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 16
  ) %>%
  rbind(output_df6)

rm(output_df6)

output_df8 <- read_csv(here::here("outputs", "connectivity_4x4_16_highR0.csv")) %>%
  mutate(sensitivity = "reproduction") %>%
  mutate(sen_value = "high") %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 16
  ) %>%
  rbind(output_df7)

rm(output_df7)

output_df9 <- read_csv(here::here("outputs", "connectivity_4x4_16_highdd.csv")) %>%
  mutate(sensitivity = "density_dependence") %>%
  mutate(sen_value = "high") %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 16
  ) %>%
  rbind(output_df8)

rm(output_df8)

output_df1 <- read_csv(here::here("outputs", "connectivity_8x8_2_lowF.csv")) %>%
  mutate(sensitivity = "fishing") %>%
  mutate(sen_value = "low") %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 2
  ) %>%
  rbind(output_df9)

rm(output_df9)

output_df2 <- read_csv(here::here("outputs", "connectivity_8x8_2_lowR0.csv")) %>%
  mutate(sensitivity = "reproduction") %>%
  mutate(sen_value = "low") %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 2
  ) %>%
  rbind(output_df1)

rm(output_df1)

output_df3 <- read_csv(here::here("outputs", "connectivity_8x8_2_lowdd.csv")) %>%
  mutate(sensitivity = "density_dependence") %>%
  mutate(sen_value = "low") %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 2
  ) %>%
  rbind(output_df2)

rm(output_df2)

output_df4 <- read_csv(here::here("outputs", "connectivity_8x8_2_medF.csv")) %>%
  mutate(sensitivity = "fishing") %>%
  mutate(sen_value = "med") %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 2
  ) %>%
  rbind(output_df3)

rm(output_df3)

output_df5 <- read_csv(here::here("outputs", "connectivity_8x8_2_medR0.csv")) %>%
  mutate(sensitivity = "reproduction") %>%
  mutate(sen_value = "med") %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 2
  ) %>%
  rbind(output_df4)

rm(output_df4)

output_df6 <- read_csv(here::here("outputs", "connectivity_8x8_2_meddd.csv")) %>%
  mutate(sensitivity = "density_dependence") %>%
  mutate(sen_value = "med") %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 2
  ) %>%
  rbind(output_df5)

rm(output_df5)

output_df7 <- read_csv(here::here("outputs", "connectivity_8x8_2_highF.csv")) %>%
  mutate(sensitivity = "fishing") %>%
  mutate(sen_value = "high") %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 2
  ) %>%
  rbind(output_df6)

rm(output_df6)

output_df8 <- read_csv(here::here("outputs", "connectivity_8x8_2_highR0.csv")) %>%
  mutate(sensitivity = "reproduction") %>%
  mutate(sen_value = "high") %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 2
  ) %>%
  rbind(output_df7)

rm(output_df7)

output_df9 <- read_csv(here::here("outputs", "connectivity_8x8_2_highdd.csv")) %>%
  mutate(sensitivity = "density_dependence") %>%
  mutate(sen_value = "high") %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 2
  ) %>%
  rbind(output_df8)

rm(output_df8)
gc()

output_df1 <- read_csv(here::here("outputs", "connectivity_8x8_4_lowF.csv")) %>%
  mutate(sensitivity = "fishing") %>%
  mutate(sen_value = "low") %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 4
  ) %>%
  rbind(output_df9)

rm(output_df9)

output_df2 <- read_csv(here::here("outputs", "connectivity_8x8_4_lowR0.csv")) %>%
  mutate(sensitivity = "reproduction") %>%
  mutate(sen_value = "low") %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 4
  ) %>%
  rbind(output_df1)

rm(output_df1)

output_df3 <- read_csv(here::here("outputs", "connectivity_8x8_4_lowdd.csv")) %>%
  mutate(sensitivity = "density_dependence") %>%
  mutate(sen_value = "low") %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 4
  ) %>%
  rbind(output_df2)

rm(output_df2)

output_df4 <- read_csv(here::here("outputs", "connectivity_8x8_4_medF.csv")) %>%
  mutate(sensitivity = "fishing") %>%
  mutate(sen_value = "med") %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 4
  ) %>%
  rbind(output_df3)

rm(output_df3)

output_df5 <- read_csv(here::here("outputs", "connectivity_8x8_4_medR0.csv")) %>%
  mutate(sensitivity = "reproduction") %>%
  mutate(sen_value = "med") %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 4
  ) %>%
  rbind(output_df4)

rm(output_df4)

output_df6 <- read_csv(here::here("outputs", "connectivity_8x8_4_meddd.csv")) %>%
  mutate(sensitivity = "density_dependence") %>%
  mutate(sen_value = "med") %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 4
  ) %>%
  rbind(output_df5)

rm(output_df5)

output_df7 <- read_csv(here::here("outputs", "connectivity_8x8_4_highF.csv")) %>%
  mutate(sensitivity = "fishing") %>%
  mutate(sen_value = "high") %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 4
  ) %>%
  rbind(output_df6)

rm(output_df6)

output_df8 <- read_csv(here::here("outputs", "connectivity_8x8_4_highR0.csv")) %>%
  mutate(sensitivity = "reproduction") %>%
  mutate(sen_value = "high") %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 4
  ) %>%
  rbind(output_df7)

rm(output_df7)

output_df9 <- read_csv(here::here("outputs", "connectivity_8x8_4_highdd.csv")) %>%
  mutate(sensitivity = "density_dependence") %>%
  mutate(sen_value = "high") %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 4
  ) %>%
  rbind(output_df8)

rm(output_df8)

output_df1 <- read_csv(here::here("outputs", "connectivity_8x8_8_lowF.csv")) %>%
  mutate(sensitivity = "fishing") %>%
  mutate(sen_value = "low") %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
  ) %>%
  rbind(output_df9)

rm(output_df9)

output_df2 <- read_csv(here::here("outputs", "connectivity_8x8_8_lowR0.csv")) %>%
  mutate(sensitivity = "reproduction") %>%
  mutate(sen_value = "low") %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8
  ) %>%
  rbind(output_df1)

rm(output_df1)

output_df3 <- read_csv(here::here("outputs", "connectivity_8x8_8_lowdd.csv")) %>%
  mutate(sensitivity = "density_dependence") %>%
  mutate(sen_value = "low") %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8
  ) %>%
  rbind(output_df2)

rm(output_df2)

output_df4 <- read_csv(here::here("outputs", "connectivity_8x8_8_medF.csv")) %>%
  mutate(sensitivity = "fishing") %>%
  mutate(sen_value = "med") %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8
  ) %>%
  rbind(output_df3)

rm(output_df3)

output_df5 <- read_csv(here::here("outputs", "connectivity_8x8_8_medR0.csv")) %>%
  mutate(sensitivity = "reproduction") %>%
  mutate(sen_value = "med") %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8
  ) %>%
  rbind(output_df4)

rm(output_df4)

output_df6 <- read_csv(here::here("outputs", "connectivity_8x8_8_meddd.csv")) %>%
  mutate(sensitivity = "density_dependence") %>%
  mutate(sen_value = "med") %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8
  ) %>%
  rbind(output_df5)

rm(output_df5)

output_df7 <- read_csv(here::here("outputs", "connectivity_8x8_8_highF.csv")) %>%
  mutate(sensitivity = "fishing") %>%
  mutate(sen_value = "high") %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8
  ) %>%
  rbind(output_df6)

rm(output_df6)

output_df8 <- read_csv(here::here("outputs", "connectivity_8x8_8_highR0.csv")) %>%
  mutate(sensitivity = "reproduction") %>%
  mutate(sen_value = "high") %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8
  ) %>%
  rbind(output_df7)

rm(output_df7)

output_df9 <- read_csv(here::here("outputs", "connectivity_8x8_8_highdd.csv")) %>%
  mutate(sensitivity = "density_dependence") %>%
  mutate(sen_value = "high") %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8
  ) %>%
  rbind(output_df8)

rm(output_df8)

output_df1 <- read_csv(here::here("outputs", "connectivity_8x8_16_lowF.csv")) %>%
  mutate(sensitivity = "fishing") %>%
  mutate(sen_value = "low") %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 16
  ) %>%
  rbind(output_df9)

rm(output_df9)

output_df2 <- read_csv(here::here("outputs", "connectivity_8x8_16_lowR0.csv")) %>%
  mutate(sensitivity = "reproduction") %>%
  mutate(sen_value = "low") %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 16
  ) %>%
  rbind(output_df1)

rm(output_df1)

output_df3 <- read_csv(here::here("outputs", "connectivity_8x8_16_lowdd.csv")) %>%
  mutate(sensitivity = "density_dependence") %>%
  mutate(sen_value = "low") %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 16
  ) %>%
  rbind(output_df2)

rm(output_df2)

output_df4 <- read_csv(here::here("outputs", "connectivity_8x8_16_medF.csv")) %>%
  mutate(sensitivity = "fishing") %>%
  mutate(sen_value = "med") %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 16
  ) %>%
  rbind(output_df3)

rm(output_df3)

output_df5 <- read_csv(here::here("outputs", "connectivity_8x8_16_medR0.csv")) %>%
  mutate(sensitivity = "reproduction") %>%
  mutate(sen_value = "med") %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 16
  ) %>%
  rbind(output_df4)

rm(output_df4)

output_df6 <- read_csv(here::here("outputs", "connectivity_8x8_16_meddd.csv")) %>%
  mutate(sensitivity = "density_dependence") %>%
  mutate(sen_value = "med") %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 16
  ) %>%
  rbind(output_df5)

rm(output_df5)

output_df7 <- read_csv(here::here("outputs", "connectivity_8x8_16_highF.csv")) %>%
  mutate(sensitivity = "fishing") %>%
  mutate(sen_value = "high") %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 16
  ) %>%
  rbind(output_df6)

rm(output_df6)

output_df8 <- read_csv(here::here("outputs", "connectivity_8x8_16_highR0.csv")) %>%
  mutate(sensitivity = "reproduction") %>%
  mutate(sen_value = "high") %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 16
  ) %>%
  rbind(output_df7)

rm(output_df7)

output_df9 <- read_csv(here::here("outputs", "connectivity_8x8_16_highdd.csv")) %>%
  mutate(sensitivity = "density_dependence") %>%
  mutate(sen_value = "high") %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 16
  ) %>%
  rbind(output_df8)

rm(output_df8)
gc()

write_csv(output_df9, here::here("data", "processed_data", "connectivity_results.csv"))
