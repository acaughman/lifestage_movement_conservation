library(tidyverse)

# Combine Biomass Data ------------------------------------------------------------

output_df1 <- read_csv(here::here("outputs", "2x2_0_lowElowF.csv")) %>%
  mutate(
    mpa_size = 2,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(25, 26) & lon %in% c(25, 26)) ~ "MPA 1",
      (lat %in% c(25, 26) & lon %in% c(22, 23)) ~ "Reference",
      TRUE ~ "Non-MPA"
    ),
    fp = "low",
    eggs = "low"
  ) %>%
  filter(mpa != "Non-MPA")

output_df2 <- read_csv(here::here("outputs", "2x2_0_medElowF.csv")) %>%
  mutate(
    mpa_size = 2,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(25, 26) & lon %in% c(25, 26)) ~ "MPA 1",
      (lat %in% c(25, 26) & lon %in% c(22, 23)) ~ "Reference",
      TRUE ~ "Non-MPA"
    ),
    fp = "low",
    eggs = "med"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df1)

rm(output_df1)

output_df3 <- read_csv(here::here("outputs", "2x2_0_highElowF.csv")) %>%
  mutate(
    mpa_size = 2,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(25, 26) & lon %in% c(25, 26)) ~ "MPA 1",
      (lat %in% c(25, 26) & lon %in% c(22, 23)) ~ "Reference",
      TRUE ~ "Non-MPA"
    ),
    fp = "low",
    eggs = "high"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df2)

rm(output_df2)

output_df4 <- read_csv(here::here("outputs", "2x2_0_lowEmedF.csv")) %>%
  mutate(
    mpa_size = 2,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(25, 26) & lon %in% c(25, 26)) ~ "MPA 1",
      (lat %in% c(25, 26) & lon %in% c(22, 23)) ~ "Reference",
      TRUE ~ "Non-MPA"
    ),
    fp = "med",
    eggs = "low"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df3)

rm(output_df3)

output_df5 <- read_csv(here::here("outputs", "2x2_0_medEmedF.csv")) %>%
  mutate(
    mpa_size = 2,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(25, 26) & lon %in% c(25, 26)) ~ "MPA 1",
      (lat %in% c(25, 26) & lon %in% c(22, 23)) ~ "Reference",
      TRUE ~ "Non-MPA"
    ),
    fp = "med",
    eggs = "med"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df4)

rm(output_df4)

output_df6 <- read_csv(here::here("outputs", "2x2_0_highEmedF.csv")) %>%
  mutate(
    mpa_size = 2,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(25, 26) & lon %in% c(25, 26)) ~ "MPA 1",
      (lat %in% c(25, 26) & lon %in% c(22, 23)) ~ "Reference",
      TRUE ~ "Non-MPA"
    ),
    fp = "med",
    eggs = "high"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df5)

rm(output_df5)

output_df7 <- read_csv(here::here("outputs", "2x2_0_lowEhighF.csv")) %>%
  mutate(
    mpa_size = 2,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(25, 26) & lon %in% c(25, 26)) ~ "MPA 1",
      (lat %in% c(25, 26) & lon %in% c(22, 23)) ~ "Reference",
      TRUE ~ "Non-MPA"
    ),
    fp = "high",
    eggs = "low"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df6)

rm(output_df6)

output_df8 <- read_csv(here::here("outputs", "2x2_0_medEhighF.csv")) %>%
  mutate(
    mpa_size = 2,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(25, 26) & lon %in% c(25, 26)) ~ "MPA 1",
      (lat %in% c(25, 26) & lon %in% c(22, 23)) ~ "Reference",
      TRUE ~ "Non-MPA"
    ),
    fp = "high",
    eggs = "med"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df7)

rm(output_df7)

output_df9 <- read_csv(here::here("outputs", "2x2_0_highEhighF.csv")) %>%
  mutate(
    mpa_size = 2,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(25, 26) & lon %in% c(25, 26)) ~ "MPA 1",
      (lat %in% c(25, 26) & lon %in% c(22, 23)) ~ "Reference",
      TRUE ~ "Non-MPA"
    ),
    fp = "high",
    eggs = "high"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df8)

rm(output_df8)

output_df1 <- read_csv(here::here("outputs", "4x4_0_lowElowF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(24:27) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(24:27) & lon %in% c(19:22)) ~ "Reference",
      TRUE ~ "Non-MPA"
    ),
    fp = "low",
    eggs = "low"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df9)

rm(output_df9)

output_df2 <- read_csv(here::here("outputs", "4x4_0_medElowF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(24:27) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(24:27) & lon %in% c(19:22)) ~ "Reference",
      TRUE ~ "Non-MPA"
    ),
    fp = "low",
    eggs = "med"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df1)

rm(output_df1)

output_df3 <- read_csv(here::here("outputs", "4x4_0_highElowF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(24:27) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(24:27) & lon %in% c(19:22)) ~ "Reference",
      TRUE ~ "Non-MPA"
    ),
    fp = "low",
    eggs = "high"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df2)

rm(output_df2)

output_df4 <- read_csv(here::here("outputs", "4x4_0_lowEmedF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(24:27) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(24:27) & lon %in% c(19:22)) ~ "Reference",
      TRUE ~ "Non-MPA"
    ),
    fp = "med",
    eggs = "low"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df3)

rm(output_df3)

output_df5 <- read_csv(here::here("outputs", "4x4_0_medEmedF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(24:27) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(24:27) & lon %in% c(19:22)) ~ "Reference",
      TRUE ~ "Non-MPA"
    ),
    fp = "med",
    eggs = "med"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df4)

rm(output_df4)

output_df6 <- read_csv(here::here("outputs", "4x4_0_highEmedF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(24:27) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(24:27) & lon %in% c(19:22)) ~ "Reference",
      TRUE ~ "Non-MPA"
    ),
    fp = "med",
    eggs = "high"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df5)

rm(output_df5)

output_df7 <- read_csv(here::here("outputs", "4x4_0_lowEhighF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(24:27) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(24:27) & lon %in% c(19:22)) ~ "Reference",
      TRUE ~ "Non-MPA"
    ),
    fp = "high",
    eggs = "low"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df6)

rm(output_df6)

output_df8 <- read_csv(here::here("outputs", "4x4_0_medEhighF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(24:27) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(24:27) & lon %in% c(19:22)) ~ "Reference",
      TRUE ~ "Non-MPA"
    ),
    fp = "high",
    eggs = "med"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df7)

rm(output_df7)

output_df9 <- read_csv(here::here("outputs", "4x4_0_highEhighF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(24:27) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(24:27) & lon %in% c(19:22)) ~ "Reference",
      TRUE ~ "Non-MPA"
    ),
    fp = "high",
    eggs = "high"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df8)

rm(output_df8)

output_df1 <- read_csv(here::here("outputs", "8x8_0_lowElowF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(22:29) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(22:29) & lon %in% c(13:20)) ~ "Reference",
      TRUE ~ "Non-MPA"
    ),
    fp = "low",
    eggs = "low"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df9)

rm(output_df9)

output_df2 <- read_csv(here::here("outputs", "8x8_0_medElowF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(22:29) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(22:29) & lon %in% c(13:20)) ~ "Reference",
      TRUE ~ "Non-MPA"
    ),
    fp = "low",
    eggs = "med"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df1)

rm(output_df1)

output_df3 <- read_csv(here::here("outputs", "8x8_0_highElowF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(22:29) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(22:29) & lon %in% c(13:20)) ~ "Reference",
      TRUE ~ "Non-MPA"
    ),
    fp = "low",
    eggs = "high"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df2)

rm(output_df2)

output_df4 <- read_csv(here::here("outputs", "8x8_0_lowEmedF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(22:29) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(22:29) & lon %in% c(13:20)) ~ "Reference",
      TRUE ~ "Non-MPA"
    ),
    fp = "med",
    eggs = "low"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df3)

rm(output_df3)

output_df5 <- read_csv(here::here("outputs", "8x8_0_medEmedF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(22:29) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(22:29) & lon %in% c(13:20)) ~ "Reference",
      TRUE ~ "Non-MPA"
    ),
    fp = "med",
    eggs = "med"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df4)

rm(output_df4)

output_df6 <- read_csv(here::here("outputs", "8x8_0_highEmedF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(22:29) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(22:29) & lon %in% c(13:20)) ~ "Reference",
      TRUE ~ "Non-MPA"
    ),
    fp = "med",
    eggs = "high"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df5)

rm(output_df5)

output_df7 <- read_csv(here::here("outputs", "8x8_0_lowEhighF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(22:29) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(22:29) & lon %in% c(13:20)) ~ "Reference",
      TRUE ~ "Non-MPA"
    ),
    fp = "high",
    eggs = "low"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df6)

rm(output_df6)

output_df8 <- read_csv(here::here("outputs", "8x8_0_medEhighF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(22:29) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(22:29) & lon %in% c(13:20)) ~ "Reference",
      TRUE ~ "Non-MPA"
    ),
    fp = "high",
    eggs = "med"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df7)

rm(output_df7)

output_df9 <- read_csv(here::here("outputs", "8x8_0_highEhighF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(22:29) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(22:29) & lon %in% c(13:20)) ~ "Reference",
      TRUE ~ "Non-MPA"
    ),
    fp = "high",
    eggs = "high"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df8)

rm(output_df8)

output_df1 <- read_csv(here::here("outputs", "16x16_0_lowElowF.csv")) %>%
  mutate(
    mpa_size = 16,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(17:32) & lon %in% c(17:32)) ~ "MPA 1",
      (lat %in% c(17:32) & lon %in% c(1:15)) ~ "Reference",
      TRUE ~ "Non-MPA"
    ),
    fp = "low",
    eggs = "low"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df9)

rm(output_df9)

output_df2 <- read_csv(here::here("outputs", "16x16_0_medElowF.csv")) %>%
  mutate(
    mpa_size = 16,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(17:32) & lon %in% c(17:32)) ~ "MPA 1",
      (lat %in% c(17:32) & lon %in% c(1:15)) ~ "Reference",
      TRUE ~ "Non-MPA"
    ),
    fp = "low",
    eggs = "med"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df1)

rm(output_df1)

output_df3 <- read_csv(here::here("outputs", "16x16_0_highElowF.csv")) %>%
  mutate(
    mpa_size = 16,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(17:32) & lon %in% c(17:32)) ~ "MPA 1",
      (lat %in% c(17:32) & lon %in% c(1:15)) ~ "Reference",
      TRUE ~ "Non-MPA"
    ),
    fp = "low",
    eggs = "high"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df2)

rm(output_df2)

output_df4 <- read_csv(here::here("outputs", "16x16_0_lowEmedF.csv")) %>%
  mutate(
    mpa_size = 16,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(17:32) & lon %in% c(17:32)) ~ "MPA 1",
      (lat %in% c(17:32) & lon %in% c(1:15)) ~ "Reference",
      TRUE ~ "Non-MPA"
    ),
    fp = "med",
    eggs = "low"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df3)

rm(output_df3)

output_df5 <- read_csv(here::here("outputs", "16x16_0_medEmedF.csv")) %>%
  mutate(
    mpa_size = 16,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(17:32) & lon %in% c(17:32)) ~ "MPA 1",
      (lat %in% c(17:32) & lon %in% c(1:15)) ~ "Reference",
      TRUE ~ "Non-MPA"
    ),
    fp = "med",
    eggs = "med"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df4)

rm(output_df4)

output_df6 <- read_csv(here::here("outputs", "16x16_0_highEmedF.csv")) %>%
  mutate(
    mpa_size = 16,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(17:32) & lon %in% c(17:32)) ~ "MPA 1",
      (lat %in% c(17:32) & lon %in% c(1:15)) ~ "Reference",
      TRUE ~ "Non-MPA"
    ),
    fp = "med",
    eggs = "high"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df5)

rm(output_df5)

output_df7 <- read_csv(here::here("outputs", "16x16_0_lowEhighF.csv")) %>%
  mutate(
    mpa_size = 16,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(17:32) & lon %in% c(17:32)) ~ "MPA 1",
      (lat %in% c(17:32) & lon %in% c(1:15)) ~ "Reference",
      TRUE ~ "Non-MPA"
    ),
    fp = "high",
    eggs = "low"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df6)

rm(output_df6)

output_df8 <- read_csv(here::here("outputs", "16x16_0_medEhighF.csv")) %>%
  mutate(
    mpa_size = 16,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(17:32) & lon %in% c(17:32)) ~ "MPA 1",
      (lat %in% c(17:32) & lon %in% c(1:15)) ~ "Reference",
      TRUE ~ "Non-MPA"
    ),
    fp = "high",
    eggs = "med"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df7)

rm(output_df7)

output_df9 <- read_csv(here::here("outputs", "16x16_0_highEhighF.csv")) %>%
  mutate(
    mpa_size = 16,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(17:32) & lon %in% c(17:32)) ~ "MPA 1",
      (lat %in% c(17:32) & lon %in% c(1:15)) ~ "Reference",
      TRUE ~ "Non-MPA"
    ),
    fp = "high",
    eggs = "high"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df8)

rm(output_df8)

output_df1 <- read_csv(here::here("outputs", "4x4_2_lowElowF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 2,
    mpa = case_when(
      (lat %in% c(21:24) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(21:24) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(27:30) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "low",
    eggs = "low"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df9)

rm(output_df9)

output_df2 <- read_csv(here::here("outputs", "4x4_2_medElowF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 2,
    mpa = case_when(
      (lat %in% c(21:24) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(21:24) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(27:30) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "low",
    eggs = "med"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df1)

rm(output_df1)

output_df3 <- read_csv(here::here("outputs", "4x4_2_highElowF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 2,
    mpa = case_when(
      (lat %in% c(21:24) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(21:24) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(27:30) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "low",
    eggs = "high"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df2)

rm(output_df2)

output_df4 <- read_csv(here::here("outputs", "4x4_2_lowEmedF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 2,
    mpa = case_when(
      (lat %in% c(21:24) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(21:24) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(27:30) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "med",
    eggs = "low"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df3)

rm(output_df3)

output_df5 <- read_csv(here::here("outputs", "4x4_2_medEmedF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 2,
    mpa = case_when(
      (lat %in% c(21:24) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(21:24) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(27:30) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "med",
    eggs = "med"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df4)

rm(output_df4)

output_df6 <- read_csv(here::here("outputs", "4x4_2_highEmedF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 2,
    mpa = case_when(
      (lat %in% c(21:24) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(21:24) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(27:30) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "med",
    eggs = "high"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df5)

rm(output_df5)

output_df7 <- read_csv(here::here("outputs", "4x4_2_lowEhighF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 2,
    mpa = case_when(
      (lat %in% c(21:24) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(21:24) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(27:30) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "high",
    eggs = "low"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df6)

rm(output_df6)

output_df8 <- read_csv(here::here("outputs", "4x4_2_medEhighF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 2,
    mpa = case_when(
      (lat %in% c(21:24) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(21:24) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(27:30) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "high",
    eggs = "med"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df7)

rm(output_df7)

output_df9 <- read_csv(here::here("outputs", "4x4_2_highEhighF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 2,
    mpa = case_when(
      (lat %in% c(21:24) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(21:24) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(27:30) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "high",
    eggs = "high"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df8)

rm(output_df8)

output_df1 <- read_csv(here::here("outputs", "4x4_4_lowElowF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 4,
    mpa = case_when(
      (lat %in% c(20:23) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(20:23) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(28:31) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "low",
    eggs = "low"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df9)

rm(output_df9)

output_df2 <- read_csv(here::here("outputs", "4x4_4_medElowF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 4,
    mpa = case_when(
      (lat %in% c(20:23) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(20:23) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(28:31) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "low",
    eggs = "med"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df1)

rm(output_df1)

output_df3 <- read_csv(here::here("outputs", "4x4_4_highElowF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 4,
    mpa = case_when(
      (lat %in% c(20:23) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(20:23) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(28:31) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "low",
    eggs = "high"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df2)

rm(output_df2)

output_df4 <- read_csv(here::here("outputs", "4x4_4_lowEmedF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 4,
    mpa = case_when(
      (lat %in% c(20:23) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(20:23) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(28:31) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "med",
    eggs = "low"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df3)

rm(output_df3)

output_df5 <- read_csv(here::here("outputs", "4x4_4_medEmedF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 4,
    mpa = case_when(
      (lat %in% c(20:23) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(20:23) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(28:31) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "med",
    eggs = "med"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df4)

rm(output_df4)

output_df6 <- read_csv(here::here("outputs", "4x4_4_highEmedF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 4,
    mpa = case_when(
      (lat %in% c(20:23) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(20:23) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(28:31) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "med",
    eggs = "high"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df5)

rm(output_df5)

output_df7 <- read_csv(here::here("outputs", "4x4_4_lowEhighF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 4,
    mpa = case_when(
      (lat %in% c(20:23) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(20:23) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(28:31) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "high",
    eggs = "low"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df6)

rm(output_df6)

output_df8 <- read_csv(here::here("outputs", "4x4_4_medEhighF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 4,
    mpa = case_when(
      (lat %in% c(20:23) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(20:23) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(28:31) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "high",
    eggs = "med"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df7)

rm(output_df7)

output_df9 <- read_csv(here::here("outputs", "4x4_4_highEhighF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 4,
    mpa = case_when(
      (lat %in% c(20:23) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(20:23) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(28:31) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "high",
    eggs = "high"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df8)

rm(output_df8)
gc()

output_df1 <- read_csv(here::here("outputs", "4x4_8_lowElowF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 8,
    mpa = case_when(
      (lat %in% c(18:21) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(18:21) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(30:33) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "low",
    eggs = "low"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df9)

rm(output_df9)

output_df2 <- read_csv(here::here("outputs", "4x4_8_medElowF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 8,
    mpa = case_when(
      (lat %in% c(18:21) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(18:21) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(30:33) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "low",
    eggs = "med"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df1)

rm(output_df1)

output_df3 <- read_csv(here::here("outputs", "4x4_8_highElowF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 8,
    mpa = case_when(
      (lat %in% c(18:21) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(18:21) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(30:33) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "low",
    eggs = "high"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df2)

rm(output_df2)

output_df4 <- read_csv(here::here("outputs", "4x4_8_lowEmedF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 8,
    mpa = case_when(
      (lat %in% c(18:21) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(18:21) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(30:33) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "med",
    eggs = "low"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df3)

rm(output_df3)

output_df5 <- read_csv(here::here("outputs", "4x4_8_medEmedF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 8,
    mpa = case_when(
      (lat %in% c(18:21) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(18:21) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(30:33) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "med",
    eggs = "med"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df4)

rm(output_df4)

output_df6 <- read_csv(here::here("outputs", "4x4_8_highEmedF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 8,
    mpa = case_when(
      (lat %in% c(18:21) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(18:21) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(30:33) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "med",
    eggs = "high"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df5)

rm(output_df5)

output_df7 <- read_csv(here::here("outputs", "4x4_8_lowEhighF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 8,
    mpa = case_when(
      (lat %in% c(18:21) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(18:21) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(30:33) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "high",
    eggs = "low"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df6)

rm(output_df6)

output_df8 <- read_csv(here::here("outputs", "4x4_8_medEhighF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 8,
    mpa = case_when(
      (lat %in% c(18:21) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(18:21) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(30:33) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "high",
    eggs = "med"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df7)

rm(output_df7)

output_df9 <- read_csv(here::here("outputs", "4x4_8_highEhighF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 8,
    mpa = case_when(
      (lat %in% c(18:21) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(18:21) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(30:33) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "high",
    eggs = "high"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df8)

rm(output_df8)

output_df1 <- read_csv(here::here("outputs", "4x4_16_lowElowF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 16,
    mpa = case_when(
      (lat %in% c(14:17) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(14:17) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(34:37) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "low",
    eggs = "low"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df9)

rm(output_df9)

output_df2 <- read_csv(here::here("outputs", "4x4_16_medElowF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 16,
    mpa = case_when(
      (lat %in% c(14:17) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(14:17) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(34:37) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "low",
    eggs = "med"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df1)

rm(output_df1)

output_df3 <- read_csv(here::here("outputs", "4x4_16_highElowF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 16,
    mpa = case_when(
      (lat %in% c(14:17) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(14:17) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(34:37) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "low",
    eggs = "high"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df2)

rm(output_df2)

output_df4 <- read_csv(here::here("outputs", "4x4_16_lowEmedF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 16,
    mpa = case_when(
      (lat %in% c(14:17) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(14:17) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(34:37) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "med",
    eggs = "low"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df3)

rm(output_df3)

output_df5 <- read_csv(here::here("outputs", "4x4_16_medEmedF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 16,
    mpa = case_when(
      (lat %in% c(14:17) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(14:17) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(34:37) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "med",
    eggs = "med"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df4)

rm(output_df4)

output_df6 <- read_csv(here::here("outputs", "4x4_16_highEmedF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 16,
    mpa = case_when(
      (lat %in% c(14:17) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(14:17) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(34:37) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "med",
    eggs = "high"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df5)

rm(output_df5)

output_df7 <- read_csv(here::here("outputs", "4x4_16_lowEhighF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 16,
    mpa = case_when(
      (lat %in% c(14:17) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(14:17) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(34:37) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "high",
    eggs = "low"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df6)

rm(output_df6)

output_df8 <- read_csv(here::here("outputs", "4x4_16_medEhighF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 16,
    mpa = case_when(
      (lat %in% c(14:17) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(14:17) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(34:37) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "high",
    eggs = "med"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df7)

rm(output_df7)

output_df9 <- read_csv(here::here("outputs", "4x4_16_highEhighF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 16,
    mpa = case_when(
      (lat %in% c(14:17) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(14:17) & lon %in% c(19:22)) ~ "Reference",
      (lat %in% c(34:37) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "high",
    eggs = "high"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df8)

rm(output_df8)

output_df1 <- read_csv(here::here("outputs", "8x8_2_lowElowF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 2,
    mpa = case_when(
      (lat %in% c(17:24) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(17:24) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(27:34) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "low",
    eggs = "low"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df9)

rm(output_df9)

output_df2 <- read_csv(here::here("outputs", "8x8_2_medElowF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 2,
    mpa = case_when(
      (lat %in% c(17:24) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(17:24) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(27:34) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "low",
    eggs = "med"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df1)

rm(output_df1)

output_df3 <- read_csv(here::here("outputs", "8x8_2_highElowF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 2,
    mpa = case_when(
      (lat %in% c(17:24) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(17:24) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(27:34) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "low",
    eggs = "high"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df2)

rm(output_df2)

output_df4 <- read_csv(here::here("outputs", "8x8_2_lowEmedF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 2,
    mpa = case_when(
      (lat %in% c(17:24) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(17:24) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(27:34) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "med",
    eggs = "low"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df3)

rm(output_df3)

output_df5 <- read_csv(here::here("outputs", "8x8_2_medEmedF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 2,
    mpa = case_when(
      (lat %in% c(17:24) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(17:24) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(27:34) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "med",
    eggs = "med"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df4)

rm(output_df4)

output_df6 <- read_csv(here::here("outputs", "8x8_2_highEmedF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 2,
    mpa = case_when(
      (lat %in% c(17:24) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(17:24) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(27:34) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "med",
    eggs = "high"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df5)

rm(output_df5)

output_df7 <- read_csv(here::here("outputs", "8x8_2_lowEhighF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 2,
    mpa = case_when(
      (lat %in% c(17:24) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(17:24) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(27:34) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "high",
    eggs = "low"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df6)

rm(output_df6)

output_df8 <- read_csv(here::here("outputs", "8x8_2_medEhighF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 2,
    mpa = case_when(
      (lat %in% c(17:24) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(17:24) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(27:34) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "high",
    eggs = "med"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df7)

rm(output_df7)

output_df9 <- read_csv(here::here("outputs", "8x8_2_highEhighF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 2,
    mpa = case_when(
      (lat %in% c(17:24) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(17:24) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(27:34) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "high",
    eggs = "high"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df8)

rm(output_df8)
gc()

output_df1 <- read_csv(here::here("outputs", "8x8_4_lowElowF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 4,
    mpa = case_when(
      (lat %in% c(16:23) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(16:23) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(28:35) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "low",
    eggs = "low"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df9)

rm(output_df9)

output_df2 <- read_csv(here::here("outputs", "8x8_4_medElowF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 4,
    mpa = case_when(
      (lat %in% c(16:23) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(16:23) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(28:35) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "low",
    eggs = "med"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df1)

rm(output_df1)

output_df3 <- read_csv(here::here("outputs", "8x8_4_highElowF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 4,
    mpa = case_when(
      (lat %in% c(16:23) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(16:23) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(28:35) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "low",
    eggs = "high"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df2)

rm(output_df2)

output_df4 <- read_csv(here::here("outputs", "8x8_4_lowEmedF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 4,
    mpa = case_when(
      (lat %in% c(16:23) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(16:23) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(28:35) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "med",
    eggs = "low"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df3)

rm(output_df3)

output_df5 <- read_csv(here::here("outputs", "8x8_4_medEmedF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 4,
    mpa = case_when(
      (lat %in% c(16:23) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(16:23) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(28:35) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "med",
    eggs = "med"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df4)

rm(output_df4)

output_df6 <- read_csv(here::here("outputs", "8x8_4_highEmedF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 4,
    mpa = case_when(
      (lat %in% c(16:23) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(16:23) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(28:35) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "med",
    eggs = "high"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df5)

rm(output_df5)

output_df7 <- read_csv(here::here("outputs", "8x8_4_lowEhighF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 4,
    mpa = case_when(
      (lat %in% c(16:23) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(16:23) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(28:35) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "high",
    eggs = "low"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df6)

rm(output_df6)

output_df8 <- read_csv(here::here("outputs", "8x8_4_medEhighF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 4,
    mpa = case_when(
      (lat %in% c(16:23) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(16:23) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(28:35) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "high",
    eggs = "med"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df7)

rm(output_df7)

output_df9 <- read_csv(here::here("outputs", "8x8_4_highEhighF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 4,
    mpa = case_when(
      (lat %in% c(16:23) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(16:23) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(28:35) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "high",
    eggs = "high"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df8)

rm(output_df8)

output_df1 <- read_csv(here::here("outputs", "8x8_8_lowElowF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    mpa = case_when(
      (lat %in% c(14:21) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(14:21) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(30:37) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "low",
    eggs = "low"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df9)

rm(output_df9)

output_df2 <- read_csv(here::here("outputs", "8x8_8_medElowF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    mpa = case_when(
      (lat %in% c(14:21) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(14:21) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(30:37) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "low",
    eggs = "med"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df1)

rm(output_df1)

output_df3 <- read_csv(here::here("outputs", "8x8_8_highElowF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    mpa = case_when(
      (lat %in% c(14:21) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(14:21) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(30:37) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "low",
    eggs = "high"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df2)

rm(output_df2)

output_df4 <- read_csv(here::here("outputs", "8x8_8_lowEmedF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    mpa = case_when(
      (lat %in% c(14:21) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(14:21) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(30:37) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "med",
    eggs = "low"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df3)

rm(output_df3)

output_df5 <- read_csv(here::here("outputs", "8x8_8_medEmedF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    mpa = case_when(
      (lat %in% c(14:21) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(14:21) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(30:37) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "med",
    eggs = "med"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df4)

rm(output_df4)

output_df6 <- read_csv(here::here("outputs", "8x8_8_highEmedF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    mpa = case_when(
      (lat %in% c(14:21) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(14:21) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(30:37) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "med",
    eggs = "high"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df5)

rm(output_df5)

output_df7 <- read_csv(here::here("outputs", "8x8_8_lowEhighF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    mpa = case_when(
      (lat %in% c(14:21) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(14:21) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(30:37) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "high",
    eggs = "low"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df6)

rm(output_df6)

output_df8 <- read_csv(here::here("outputs", "8x8_8_medEhighF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    mpa = case_when(
      (lat %in% c(14:21) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(14:21) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(30:37) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "high",
    eggs = "med"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df7)

rm(output_df7)

output_df9 <- read_csv(here::here("outputs", "8x8_8_highEhighF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    mpa = case_when(
      (lat %in% c(14:21) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(14:21) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(30:37) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "high",
    eggs = "high"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df8)

rm(output_df8)

output_df1 <- read_csv(here::here("outputs", "8x8_16_lowElowF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 16,
    mpa = case_when(
      (lat %in% c(10:17) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(10:17) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(34:41) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "low",
    eggs = "low"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df9)

rm(output_df9)

output_df2 <- read_csv(here::here("outputs", "8x8_16_medElowF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 16,
    mpa = case_when(
      (lat %in% c(10:17) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(10:17) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(34:41) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "low",
    eggs = "med"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df1)

rm(output_df1)

output_df3 <- read_csv(here::here("outputs", "8x8_16_highElowF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 16,
    mpa = case_when(
      (lat %in% c(10:17) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(10:17) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(34:41) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "low",
    eggs = "high"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df2)

rm(output_df2)

output_df4 <- read_csv(here::here("outputs", "8x8_16_lowEmedF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 16,
    mpa = case_when(
      (lat %in% c(10:17) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(10:17) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(34:41) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "med",
    eggs = "low"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df3)

rm(output_df3)

output_df5 <- read_csv(here::here("outputs", "8x8_16_medEmedF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 16,
    mpa = case_when(
      (lat %in% c(10:17) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(10:17) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(34:41) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "med",
    eggs = "med"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df4)

rm(output_df4)

output_df6 <- read_csv(here::here("outputs", "8x8_16_highEmedF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 16,
    mpa = case_when(
      (lat %in% c(10:17) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(10:17) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(34:41) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "med",
    eggs = "high"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df5)

rm(output_df5)

output_df7 <- read_csv(here::here("outputs", "8x8_16_lowEhighF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 16,
    mpa = case_when(
      (lat %in% c(10:17) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(10:17) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(34:41) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "high",
    eggs = "low"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df6)

rm(output_df6)

output_df8 <- read_csv(here::here("outputs", "8x8_16_medEhighF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 16,
    mpa = case_when(
      (lat %in% c(10:17) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(10:17) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(34:41) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "high",
    eggs = "med"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df7)

rm(output_df7)

output_df9 <- read_csv(here::here("outputs", "8x8_16_highEhighF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 16,
    mpa = case_when(
      (lat %in% c(10:17) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(10:17) & lon %in% c(13:20)) ~ "Reference",
      (lat %in% c(34:41) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    fp = "high",
    eggs = "high"
  ) %>%
  filter(mpa != "Non-MPA") %>%
  rbind(output_df8)

rm(output_df8)
gc()

write_csv(output_df9, here::here("data", "processed_data", "model_results.csv"))

# Combine Connectivity Data -----------------------------------------------

output_df1 <- read_csv(here::here("outputs", "connectivity_2x2_0_lowElowF.csv")) %>%
  mutate(
    mpa_size = 2,
    mpa_spacing = 0,
    fp = "low",
    eggs = "low"
  )


output_df2 <- read_csv(here::here("outputs", "connectivity_2x2_0_medElowF.csv")) %>%
  mutate(
    mpa_size = 2,
    mpa_spacing = 0,
    fp = "low",
    eggs = "med"
  ) %>%
  rbind(output_df1)

rm(output_df1)

output_df3 <- read_csv(here::here("outputs", "connectivity_2x2_0_highElowF.csv")) %>%
  mutate(
    mpa_size = 2,
    mpa_spacing = 0,
    fp = "low",
    eggs = "high"
  ) %>%
  rbind(output_df2)

rm(output_df2)

output_df4 <- read_csv(here::here("outputs", "connectivity_2x2_0_lowEmedF.csv")) %>%
  mutate(
    mpa_size = 2,
    mpa_spacing = 0,
    fp = "med",
    eggs = "low"
  ) %>%
  rbind(output_df3)

rm(output_df3)

output_df5 <- read_csv(here::here("outputs", "connectivity_2x2_0_medEmedF.csv")) %>%
  mutate(
    mpa_size = 2,
    mpa_spacing = 0,
    fp = "med",
    eggs = "med"
  ) %>%
  rbind(output_df4)

rm(output_df4)

output_df6 <- read_csv(here::here("outputs", "connectivity_2x2_0_highEmedF.csv")) %>%
  mutate(
    mpa_size = 2,
    mpa_spacing = 0,
    fp = "med",
    eggs = "high"
  ) %>%
  rbind(output_df5)

rm(output_df5)

output_df7 <- read_csv(here::here("outputs", "connectivity_2x2_0_lowEhighF.csv")) %>%
  mutate(
    mpa_size = 2,
    mpa_spacing = 0,
    fp = "high",
    eggs = "low"
  ) %>%
  rbind(output_df6)

rm(output_df6)

output_df8 <- read_csv(here::here("outputs", "connectivity_2x2_0_medEhighF.csv")) %>%
  mutate(
    mpa_size = 2,
    mpa_spacing = 0,
    fp = "high",
    eggs = "med"
  ) %>%
  rbind(output_df7)

rm(output_df7)

output_df9 <- read_csv(here::here("outputs", "connectivity_2x2_0_highEhighF.csv")) %>%
  mutate(
    mpa_size = 2,
    mpa_spacing = 0,
    fp = "high",
    eggs = "high"
  ) %>%
  rbind(output_df8)

rm(output_df8)

output_df1 <- read_csv(here::here("outputs", "connectivity_4x4_0_lowElowF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 0,
    fp = "low",
    eggs = "low"
  ) %>%
  rbind(output_df9)

rm(output_df9)

output_df2 <- read_csv(here::here("outputs", "connectivity_4x4_0_medElowF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 0,
    fp = "low",
    eggs = "med"
  ) %>%
  rbind(output_df1)

rm(output_df1)

output_df3 <- read_csv(here::here("outputs", "connectivity_4x4_0_highElowF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 0,
    fp = "low",
    eggs = "high"
  ) %>%
  rbind(output_df2)

rm(output_df2)

output_df4 <- read_csv(here::here("outputs", "connectivity_4x4_0_lowEmedF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 0,
    fp = "med",
    eggs = "low"
  ) %>%
  rbind(output_df3)

rm(output_df3)

output_df5 <- read_csv(here::here("outputs", "connectivity_4x4_0_medEmedF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 0,
    fp = "med",
    eggs = "med"
  ) %>%
  rbind(output_df4)

rm(output_df4)

output_df6 <- read_csv(here::here("outputs", "connectivity_4x4_0_highEmedF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 0,
    fp = "med",
    eggs = "high"
  ) %>%
  rbind(output_df5)

rm(output_df5)

output_df7 <- read_csv(here::here("outputs", "connectivity_4x4_0_lowEhighF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 0,
    fp = "high",
    eggs = "low"
  ) %>%
  rbind(output_df6)

rm(output_df6)

output_df8 <- read_csv(here::here("outputs", "connectivity_4x4_0_medEhighF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 0,
    fp = "high",
    eggs = "med"
  ) %>%
  rbind(output_df7)

rm(output_df7)

output_df9 <- read_csv(here::here("outputs", "connectivity_4x4_0_highEhighF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 0,
    fp = "high",
    eggs = "high"
  ) %>%
  rbind(output_df8)

rm(output_df8)

output_df1 <- read_csv(here::here("outputs", "connectivity_8x8_0_lowElowF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    fp = "low",
    eggs = "low"
  ) %>%
  rbind(output_df9)

rm(output_df9)

output_df2 <- read_csv(here::here("outputs", "connectivity_8x8_0_medElowF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    fp = "low",
    eggs = "med"
  ) %>%
  rbind(output_df1)

rm(output_df1)

output_df3 <- read_csv(here::here("outputs", "connectivity_8x8_0_highElowF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    fp = "low",
    eggs = "high"
  ) %>%
  rbind(output_df2)

rm(output_df2)

output_df4 <- read_csv(here::here("outputs", "connectivity_8x8_0_lowEmedF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    fp = "med",
    eggs = "low"
  ) %>%
  rbind(output_df3)

rm(output_df3)

output_df5 <- read_csv(here::here("outputs", "connectivity_8x8_0_medEmedF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    fp = "med",
    eggs = "med"
  ) %>%
  rbind(output_df4)

rm(output_df4)

output_df6 <- read_csv(here::here("outputs", "connectivity_8x8_0_highEmedF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    fp = "med",
    eggs = "high"
  ) %>%
  rbind(output_df5)

rm(output_df5)

output_df7 <- read_csv(here::here("outputs", "connectivity_8x8_0_lowEhighF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    fp = "high",
    eggs = "low"
  ) %>%
  rbind(output_df6)

rm(output_df6)

output_df8 <- read_csv(here::here("outputs", "connectivity_8x8_0_medEhighF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    fp = "high",
    eggs = "med"
  ) %>%
  rbind(output_df7)

rm(output_df7)

output_df9 <- read_csv(here::here("outputs", "connectivity_8x8_0_highEhighF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    fp = "high",
    eggs = "high"
  ) %>%
  rbind(output_df8)

rm(output_df8)

output_df1 <- read_csv(here::here("outputs", "connectivity_16x16_0_lowElowF.csv")) %>%
  mutate(
    mpa_size = 16,
    mpa_spacing = 0,
    fp = "low",
    eggs = "low"
  ) %>%
  rbind(output_df9)

rm(output_df9)

output_df2 <- read_csv(here::here("outputs", "connectivity_16x16_0_medElowF.csv")) %>%
  mutate(
    mpa_size = 16,
    mpa_spacing = 0,
    fp = "low",
    eggs = "med"
  ) %>%
  rbind(output_df1)

rm(output_df1)

output_df3 <- read_csv(here::here("outputs", "connectivity_16x16_0_highElowF.csv")) %>%
  mutate(
    mpa_size = 16,
    mpa_spacing = 0,
    fp = "low",
    eggs = "high"
  ) %>%
  rbind(output_df2)

rm(output_df2)

output_df4 <- read_csv(here::here("outputs", "connectivity_16x16_0_lowEmedF.csv")) %>%
  mutate(
    mpa_size = 16,
    mpa_spacing = 0,
    fp = "med",
    eggs = "low"
  ) %>%
  rbind(output_df3)

rm(output_df3)

output_df5 <- read_csv(here::here("outputs", "connectivity_16x16_0_medEmedF.csv")) %>%
  mutate(
    mpa_size = 16,
    mpa_spacing = 0,
    fp = "med",
    eggs = "med"
  ) %>%
  rbind(output_df4)

rm(output_df4)

output_df6 <- read_csv(here::here("outputs", "connectivity_16x16_0_highEmedF.csv")) %>%
  mutate(
    mpa_size = 16,
    mpa_spacing = 0,
    fp = "med",
    eggs = "high"
  ) %>%
  rbind(output_df5)

rm(output_df5)

output_df7 <- read_csv(here::here("outputs", "connectivity_16x16_0_lowEhighF.csv")) %>%
  mutate(
    mpa_size = 16,
    mpa_spacing = 0,
    fp = "high",
    eggs = "low"
  ) %>%
  rbind(output_df6)

rm(output_df6)

output_df8 <- read_csv(here::here("outputs", "connectivity_16x16_0_medEhighF.csv")) %>%
  mutate(
    mpa_size = 16,
    mpa_spacing = 0,
    fp = "high",
    eggs = "med"
  ) %>%
  rbind(output_df7)

rm(output_df7)

output_df9 <- read_csv(here::here("outputs", "connectivity_16x16_0_highEhighF.csv")) %>%
  mutate(
    mpa_size = 16,
    mpa_spacing = 0,
    fp = "high",
    eggs = "high"
  ) %>%
  rbind(output_df8)

rm(output_df8)

output_df1 <- read_csv(here::here("outputs", "connectivity_4x4_2_lowElowF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 2,
    fp = "low",
    eggs = "low"
  ) %>%
  rbind(output_df9)

rm(output_df9)

output_df2 <- read_csv(here::here("outputs", "connectivity_4x4_2_medElowF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 2,
    fp = "low",
    eggs = "med"
  ) %>%
  rbind(output_df1)

rm(output_df1)

output_df3 <- read_csv(here::here("outputs", "connectivity_4x4_2_highElowF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 2,
    fp = "low",
    eggs = "high"
  ) %>%
  rbind(output_df2)

rm(output_df2)

output_df4 <- read_csv(here::here("outputs", "connectivity_4x4_2_lowEmedF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 2,
    fp = "med",
    eggs = "low"
  ) %>%
  rbind(output_df3)

rm(output_df3)

output_df5 <- read_csv(here::here("outputs", "connectivity_4x4_2_medEmedF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 2,
    fp = "med",
    eggs = "med"
  ) %>%
  rbind(output_df4)

rm(output_df4)

output_df6 <- read_csv(here::here("outputs", "connectivity_4x4_2_highEmedF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 2,
    fp = "med",
    eggs = "high"
  ) %>%
  rbind(output_df5)

rm(output_df5)

output_df7 <- read_csv(here::here("outputs", "connectivity_4x4_2_lowEhighF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 2,
    fp = "high",
    eggs = "low"
  ) %>%
  rbind(output_df6)

rm(output_df6)

output_df8 <- read_csv(here::here("outputs", "connectivity_4x4_2_medEhighF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 2,
    fp = "high",
    eggs = "med"
  ) %>%
  rbind(output_df7)

rm(output_df7)

output_df9 <- read_csv(here::here("outputs", "connectivity_4x4_2_highEhighF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 2,
    fp = "high",
    eggs = "high"
  ) %>%
  rbind(output_df8)

rm(output_df8)

output_df1 <- read_csv(here::here("outputs", "connectivity_4x4_4_lowElowF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 4,
    fp = "low",
    eggs = "low"
  ) %>%
  rbind(output_df9)

rm(output_df9)

output_df2 <- read_csv(here::here("outputs", "connectivity_4x4_4_medElowF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 4,
    fp = "low",
    eggs = "med"
  ) %>%
  rbind(output_df1)

rm(output_df1)

output_df3 <- read_csv(here::here("outputs", "connectivity_4x4_4_highElowF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 4,
    fp = "low",
    eggs = "high"
  ) %>%
  rbind(output_df2)

rm(output_df2)

output_df4 <- read_csv(here::here("outputs", "connectivity_4x4_4_lowEmedF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 4,
    fp = "med",
    eggs = "low"
  ) %>%
  rbind(output_df3)

rm(output_df3)

output_df5 <- read_csv(here::here("outputs", "connectivity_4x4_4_medEmedF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 4,
    fp = "med",
    eggs = "med"
  ) %>%
  rbind(output_df4)

rm(output_df4)

output_df6 <- read_csv(here::here("outputs", "connectivity_4x4_4_highEmedF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 4,
    fp = "med",
    eggs = "high"
  ) %>%
  rbind(output_df5)

rm(output_df5)

output_df7 <- read_csv(here::here("outputs", "connectivity_4x4_4_lowEhighF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 4,
    fp = "high",
    eggs = "low"
  ) %>%
  rbind(output_df6)

rm(output_df6)

output_df8 <- read_csv(here::here("outputs", "connectivity_4x4_4_medEhighF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 4,
    fp = "high",
    eggs = "med"
  ) %>%
  rbind(output_df7)

rm(output_df7)

output_df9 <- read_csv(here::here("outputs", "connectivity_4x4_4_highEhighF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 4,
    fp = "high",
    eggs = "high"
  ) %>%
  rbind(output_df8)

rm(output_df8)
gc()

output_df1 <- read_csv(here::here("outputs", "connectivity_4x4_8_lowElowF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 8,
    fp = "low",
    eggs = "low"
  ) %>%
  rbind(output_df9)

rm(output_df9)

output_df2 <- read_csv(here::here("outputs", "connectivity_4x4_8_medElowF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 8,
    fp = "low",
    eggs = "med"
  ) %>%
  rbind(output_df1)

rm(output_df1)

output_df3 <- read_csv(here::here("outputs", "connectivity_4x4_8_highElowF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 8,
    fp = "low",
    eggs = "high"
  ) %>%
  rbind(output_df2)

rm(output_df2)

output_df4 <- read_csv(here::here("outputs", "connectivity_4x4_8_lowEmedF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 8,
    fp = "med",
    eggs = "low"
  ) %>%
  rbind(output_df3)

rm(output_df3)

output_df5 <- read_csv(here::here("outputs", "connectivity_4x4_8_medEmedF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 8,
    fp = "med",
    eggs = "med"
  ) %>%
  rbind(output_df4)

rm(output_df4)

output_df6 <- read_csv(here::here("outputs", "connectivity_4x4_8_highEmedF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 8,
    fp = "med",
    eggs = "high"
  ) %>%
  rbind(output_df5)

rm(output_df5)

output_df7 <- read_csv(here::here("outputs", "connectivity_4x4_8_lowEhighF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 8,
    fp = "high",
    eggs = "low"
  ) %>%
  rbind(output_df6)

rm(output_df6)

output_df8 <- read_csv(here::here("outputs", "connectivity_4x4_8_medEhighF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 8,
    fp = "high",
    eggs = "med"
  ) %>%
  rbind(output_df7)

rm(output_df7)

output_df9 <- read_csv(here::here("outputs", "connectivity_4x4_8_highEhighF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 8,
    fp = "high",
    eggs = "high"
  ) %>%
  rbind(output_df8)

rm(output_df8)

output_df1 <- read_csv(here::here("outputs", "connectivity_4x4_16_lowElowF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 16,
    fp = "low",
    eggs = "low"
  ) %>%
  rbind(output_df9)

rm(output_df9)

output_df2 <- read_csv(here::here("outputs", "connectivity_4x4_16_medElowF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 16,
    fp = "low",
    eggs = "med"
  ) %>%
  rbind(output_df1)

rm(output_df1)

output_df3 <- read_csv(here::here("outputs", "connectivity_4x4_16_highElowF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 16,
    fp = "low",
    eggs = "high"
  ) %>%
  rbind(output_df2)

rm(output_df2)

output_df4 <- read_csv(here::here("outputs", "connectivity_4x4_16_lowEmedF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 16,
    fp = "med",
    eggs = "low"
  ) %>%
  rbind(output_df3)

rm(output_df3)

output_df5 <- read_csv(here::here("outputs", "connectivity_4x4_16_medEmedF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 16,
    fp = "med",
    eggs = "med"
  ) %>%
  rbind(output_df4)

rm(output_df4)

output_df6 <- read_csv(here::here("outputs", "connectivity_4x4_16_highEmedF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 16,
    fp = "med",
    eggs = "high"
  ) %>%
  rbind(output_df5)

rm(output_df5)

output_df7 <- read_csv(here::here("outputs", "connectivity_4x4_16_lowEhighF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 16,
    fp = "high",
    eggs = "low"
  ) %>%
  rbind(output_df6)

rm(output_df6)

output_df8 <- read_csv(here::here("outputs", "connectivity_4x4_16_medEhighF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 16,
    fp = "high",
    eggs = "med"
  ) %>%
  rbind(output_df7)

rm(output_df7)

output_df9 <- read_csv(here::here("outputs", "connectivity_4x4_16_highEhighF.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 16,
    fp = "high",
    eggs = "high"
  ) %>%
  rbind(output_df8)

rm(output_df8)

output_df1 <- read_csv(here::here("outputs", "connectivity_8x8_2_lowElowF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 2,
    fp = "low",
    eggs = "low"
  ) %>%
  rbind(output_df9)

rm(output_df9)

output_df2 <- read_csv(here::here("outputs", "connectivity_8x8_2_medElowF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 2,
    fp = "low",
    eggs = "med"
  ) %>%
  rbind(output_df1)

rm(output_df1)

output_df3 <- read_csv(here::here("outputs", "connectivity_8x8_2_highElowF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 2,
    fp = "low",
    eggs = "high"
  ) %>%
  rbind(output_df2)

rm(output_df2)

output_df4 <- read_csv(here::here("outputs", "connectivity_8x8_2_lowEmedF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 2,
    fp = "med",
    eggs = "low"
  ) %>%
  rbind(output_df3)

rm(output_df3)

output_df5 <- read_csv(here::here("outputs", "connectivity_8x8_2_medEmedF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 2,
    fp = "med",
    eggs = "med"
  ) %>%
  rbind(output_df4)

rm(output_df4)

output_df6 <- read_csv(here::here("outputs", "connectivity_8x8_2_highEmedF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 2,
    fp = "med",
    eggs = "high"
  ) %>%
  rbind(output_df5)

rm(output_df5)

output_df7 <- read_csv(here::here("outputs", "connectivity_8x8_2_lowEhighF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 2,
    fp = "high",
    eggs = "low"
  ) %>%
  rbind(output_df6)

rm(output_df6)

output_df8 <- read_csv(here::here("outputs", "connectivity_8x8_2_medEhighF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 2,
    fp = "high",
    eggs = "med"
  ) %>%
  rbind(output_df7)

rm(output_df7)

output_df9 <- read_csv(here::here("outputs", "connectivity_8x8_2_highEhighF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 2,
    fp = "high",
    eggs = "high"
  ) %>%
  rbind(output_df8)

rm(output_df8)
gc()

output_df1 <- read_csv(here::here("outputs", "connectivity_8x8_4_lowElowF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 4,
    fp = "low",
    eggs = "low"
  ) %>%
  rbind(output_df9)

rm(output_df9)

output_df2 <- read_csv(here::here("outputs", "connectivity_8x8_4_medElowF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 4,
    fp = "low",
    eggs = "med"
  ) %>%
  rbind(output_df1)

rm(output_df1)

output_df3 <- read_csv(here::here("outputs", "connectivity_8x8_4_highElowF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 4,
    fp = "low",
    eggs = "high"
  ) %>%
  rbind(output_df2)

rm(output_df2)

output_df4 <- read_csv(here::here("outputs", "connectivity_8x8_4_lowEmedF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 4,
    fp = "med",
    eggs = "low"
  ) %>%
  rbind(output_df3)

rm(output_df3)

output_df5 <- read_csv(here::here("outputs", "connectivity_8x8_4_medEmedF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 4,
    fp = "med",
    eggs = "med"
  ) %>%
  rbind(output_df4)

rm(output_df4)

output_df6 <- read_csv(here::here("outputs", "connectivity_8x8_4_highEmedF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 4,
    fp = "med",
    eggs = "high"
  ) %>%
  rbind(output_df5)

rm(output_df5)

output_df7 <- read_csv(here::here("outputs", "connectivity_8x8_4_lowEhighF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 4,
    fp = "high",
    eggs = "low"
  ) %>%
  rbind(output_df6)

rm(output_df6)

output_df8 <- read_csv(here::here("outputs", "connectivity_8x8_4_medEhighF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 4,
    fp = "high",
    eggs = "med"
  ) %>%
  rbind(output_df7)

rm(output_df7)

output_df9 <- read_csv(here::here("outputs", "connectivity_8x8_4_highEhighF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 4,
    fp = "high",
    eggs = "high"
  ) %>%
  rbind(output_df8)

rm(output_df8)

output_df1 <- read_csv(here::here("outputs", "connectivity_8x8_8_lowElowF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    fp = "low",
    eggs = "low"
  ) %>%
  rbind(output_df9)

rm(output_df9)

output_df2 <- read_csv(here::here("outputs", "connectivity_8x8_8_medElowF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    fp = "low",
    eggs = "med"
  ) %>%
  rbind(output_df1)

rm(output_df1)

output_df3 <- read_csv(here::here("outputs", "connectivity_8x8_8_highElowF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    fp = "low",
    eggs = "high"
  ) %>%
  rbind(output_df2)

rm(output_df2)

output_df4 <- read_csv(here::here("outputs", "connectivity_8x8_8_lowEmedF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    fp = "med",
    eggs = "low"
  ) %>%
  rbind(output_df3)

rm(output_df3)

output_df5 <- read_csv(here::here("outputs", "connectivity_8x8_8_medEmedF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    fp = "med",
    eggs = "med"
  ) %>%
  rbind(output_df4)

rm(output_df4)

output_df6 <- read_csv(here::here("outputs", "connectivity_8x8_8_highEmedF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    fp = "med",
    eggs = "high"
  ) %>%
  rbind(output_df5)

rm(output_df5)

output_df7 <- read_csv(here::here("outputs", "connectivity_8x8_8_lowEhighF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    fp = "high",
    eggs = "low"
  ) %>%
  rbind(output_df6)

rm(output_df6)

output_df8 <- read_csv(here::here("outputs", "connectivity_8x8_8_medEhighF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    fp = "high",
    eggs = "med"
  ) %>%
  rbind(output_df7)

rm(output_df7)

output_df9 <- read_csv(here::here("outputs", "connectivity_8x8_8_highEhighF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    fp = "high",
    eggs = "high"
  ) %>%
  rbind(output_df8)

rm(output_df8)

output_df1 <- read_csv(here::here("outputs", "connectivity_8x8_16_lowElowF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 16,
    fp = "low",
    eggs = "low"
  ) %>%
  rbind(output_df9)

rm(output_df9)

output_df2 <- read_csv(here::here("outputs", "connectivity_8x8_16_medElowF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 16,
    fp = "low",
    eggs = "med"
  ) %>%
  rbind(output_df1)

rm(output_df1)

output_df3 <- read_csv(here::here("outputs", "connectivity_8x8_16_highElowF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 16,
    fp = "low",
    eggs = "high"
  ) %>%
  rbind(output_df2)

rm(output_df2)

output_df4 <- read_csv(here::here("outputs", "connectivity_8x8_16_lowEmedF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 16,
    fp = "med",
    eggs = "low"
  ) %>%
  rbind(output_df3)

rm(output_df3)

output_df5 <- read_csv(here::here("outputs", "connectivity_8x8_16_medEmedF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 16,
    fp = "med",
    eggs = "med"
  ) %>%
  rbind(output_df4)

rm(output_df4)

output_df6 <- read_csv(here::here("outputs", "connectivity_8x8_16_highEmedF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 16,
    fp = "med",
    eggs = "high"
  ) %>%
  rbind(output_df5)

rm(output_df5)

output_df7 <- read_csv(here::here("outputs", "connectivity_8x8_16_lowEhighF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 16,
    fp = "high",
    eggs = "low"
  ) %>%
  rbind(output_df6)

rm(output_df6)

output_df8 <- read_csv(here::here("outputs", "connectivity_8x8_16_medEhighF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 16,
    fp = "high",
    eggs = "med"
  ) %>%
  rbind(output_df7)

rm(output_df7)

output_df9 <- read_csv(here::here("outputs", "connectivity_8x8_16_highEhighF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 16,
    fp = "high",
    eggs = "high"
  ) %>%
  rbind(output_df8)

rm(output_df8)
gc()

write_csv(output, here::here("data", "processed_data", "connectivity_results.csv"))
