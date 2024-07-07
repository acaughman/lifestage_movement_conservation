
output_df1 <- read_csv(here::here("outputs", "8x8_0_01F.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    fp = 0.1,
    mpa = case_when(
      (lat %in% c(22:29) & lon %in% c(22:29)) ~ "MPA 1",
      TRUE ~ "Non-MPA"
    )
  )

output_df2 <- read_csv(here::here("outputs", "8x8_2_01F.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 2,
    fp = 0.1,
    mpa = case_when(
      (lat %in% c(17:24) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(27:34) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  )

output_df3 <- read_csv(here::here("outputs", "8x8_8_01F.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    fp = 0.1,
    mpa = case_when(
      (lat %in% c(14:21) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(30:37) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  )


output_df4 <- read_csv(here::here("outputs", "8x8_0_03F.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    fp = 0.3,
    mpa = case_when(
      (lat %in% c(22:29) & lon %in% c(22:29)) ~ "MPA 1",
      TRUE ~ "Non-MPA"
    )
  )

output_df5 <- read_csv(here::here("outputs", "8x8_2_03F.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 2,
    fp = 0.3,
    mpa = case_when(
      (lat %in% c(17:24) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(27:34) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  )

output_df6 <- read_csv(here::here("outputs", "8x8_8_03F.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    fp = 0.3,
    mpa = case_when(
      (lat %in% c(14:21) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(30:37) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  )


output_df7 <- read_csv(here::here("outputs", "8x8_0.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    fp = 0.5,
    mpa = case_when(
      (lat %in% c(22:29) & lon %in% c(22:29)) ~ "MPA 1",
      TRUE ~ "Non-MPA"
    )
  )

output_df8 <- read_csv(here::here("outputs", "8x8_2.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 2,
    fp = 0.5,
    mpa = case_when(
      (lat %in% c(17:24) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(27:34) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  )

output_df9 <- read_csv(here::here("outputs", "8x8_8.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    fp = 0.5,
    mpa = case_when(
      (lat %in% c(14:21) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(30:37) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  )


output_df10 <- read_csv(here::here("outputs", "8x8_0_07F.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    fp = 0.7,
    mpa = case_when(
      (lat %in% c(22:29) & lon %in% c(22:29)) ~ "MPA 1",
      TRUE ~ "Non-MPA"
    )
  )

output_df11 <- read_csv(here::here("outputs", "8x8_2_07F.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 2,
    fp = 0.7,
    mpa = case_when(
      (lat %in% c(17:24) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(27:34) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  )

output_df12 <- read_csv(here::here("outputs", "8x8_8_07F.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    fp = 0.7,
    mpa = case_when(
      (lat %in% c(14:21) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(30:37) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  )


output_df13 <- read_csv(here::here("outputs", "8x8_0_09F.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    fp = 0.9,
    mpa = case_when(
      (lat %in% c(22:29) & lon %in% c(22:29)) ~ "MPA 1",
      TRUE ~ "Non-MPA"
    )
  )

output_df14 <- read_csv(here::here("outputs", "8x8_2_09F.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 2,
    fp = 0.9,
    mpa = case_when(
      (lat %in% c(17:24) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(27:34) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  )

output_df15 <- read_csv(here::here("outputs", "8x8_8_09F.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    fp = 0.9,
    mpa = case_when(
      (lat %in% c(14:21) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(30:37) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  )

output <- list(
  output_df1, output_df2, output_df3, output_df4, output_df5, output_df6,
  output_df7, output_df8, output_df9, output_df10, output_df11, output_df12,
  output_df13, output_df14, output_df15
) %>%
  reduce(full_join)

rm(
  output_df1, output_df2, output_df3, output_df4, output_df5, output_df6,
  output_df7, output_df8, output_df9, output_df10, output_df11, output_df12,
  output_df13, output_df14, output_df15
)

write_csv(output, here::here("data", "processed_data", "model_results_fishing.csv"))

# Combine Connectivity Data -----------------------------------------------

output_df1 <- read_csv(here::here("outputs", "connectivity_8x8_0_01F.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    fp = 0.1
  )
output_df2 <- read_csv(here::here("outputs", "connectivity_8x8_0_03F.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    fp = 0.3
  )
output_df3 <- read_csv(here::here("outputs", "connectivity_8x8_0.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    fp = 0.5
  )
output_df4 <- read_csv(here::here("outputs", "connectivity_8x8_0_07F.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    fp = 0.7
  )
output_df5 <- read_csv(here::here("outputs", "connectivity_8x8_0_09F.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    fp = 0.9
  )

output_df6 <- read_csv(here::here("outputs", "connectivity_8x8_2_01F.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 2,
    fp = 0.1
  )
output_df7 <- read_csv(here::here("outputs", "connectivity_8x8_2_03F.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 2,
    fp = 0.3
  )
output_df8 <- read_csv(here::here("outputs", "connectivity_8x8_2.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 2,
    fp = 0.5
  )
output_df9 <- read_csv(here::here("outputs", "connectivity_8x8_2_07F.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 2,
    fp = 0.7
  )
output_df10 <- read_csv(here::here("outputs", "connectivity_8x8_2_09F.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 2,
    fp = 0.9
  )

output_df11 <- read_csv(here::here("outputs", "connectivity_8x8_8_01F.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    fp = 0.1
  )
output_df12 <- read_csv(here::here("outputs", "connectivity_8x8_8_03F.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    fp = 0.3
  )
output_df13 <- read_csv(here::here("outputs", "connectivity_8x8_8.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    fp = 0.5
  )
output_df14 <- read_csv(here::here("outputs", "connectivity_8x8_8_07F.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    fp = 0.7
  )
output_df15 <- read_csv(here::here("outputs", "connectivity_8x8_8_09F.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    fp = 0.9
  )

output <- list(
  output_df1, output_df2, output_df3, output_df4, output_df5, output_df6,
  output_df7, output_df8, output_df9, output_df10, output_df11, output_df12,
  output_df13, output_df14, output_df15
) %>%
  reduce(full_join)

rm(
  output_df1, output_df2, output_df3, output_df4, output_df5, output_df6,
  output_df7, output_df8, output_df9, output_df10, output_df11, output_df12,
  output_df13, output_df14, output_df15
)

write_csv(output, here::here("data", "processed_data", "connectivity_results_fishing.csv"))
