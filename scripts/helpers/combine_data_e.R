library(tidyverse)

output_df1 <- read_csv(here::here("outputs", "8x8_0_1E.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    egg = 1,
    mpa = case_when(
      (lat %in% c(22:29) & lon %in% c(22:29)) ~ "MPA 1",
      TRUE ~ "Non-MPA"
    )
  )

output_df2 <- read_csv(here::here("outputs", "8x8_2_1E.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 2,
    egg = 1,
    mpa = case_when(
      (lat %in% c(17:24) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(27:34) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  )

output_df3 <- read_csv(here::here("outputs", "8x8_8_1E.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    egg = 1,
    mpa = case_when(
      (lat %in% c(14:21) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(30:37) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  )


output_df4 <- read_csv(here::here("outputs", "8x8_0_20E.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    egg = 20,
    mpa = case_when(
      (lat %in% c(22:29) & lon %in% c(22:29)) ~ "MPA 1",
      TRUE ~ "Non-MPA"
    )
  )

output_df5 <- read_csv(here::here("outputs", "8x8_2_20E.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 2,
    egg = 20,
    mpa = case_when(
      (lat %in% c(17:24) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(27:34) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  )

output_df6 <- read_csv(here::here("outputs", "8x8_8_20E.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    egg = 20,
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
    egg = 5,
    mpa = case_when(
      (lat %in% c(22:29) & lon %in% c(22:29)) ~ "MPA 1",
      TRUE ~ "Non-MPA"
    )
  )

output_df8 <- read_csv(here::here("outputs", "8x8_2.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 2,
    egg = 5,
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
    egg = 5,
    mpa = case_when(
      (lat %in% c(14:21) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(30:37) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  )


output_df10 <- read_csv(here::here("outputs", "8x8_0_100E.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    egg = 100,
    mpa = case_when(
      (lat %in% c(22:29) & lon %in% c(22:29)) ~ "MPA 1",
      TRUE ~ "Non-MPA"
    )
  )

output_df11 <- read_csv(here::here("outputs", "8x8_2_100E.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 2,
    egg = 100,
    mpa = case_when(
      (lat %in% c(17:24) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(27:34) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  )

output_df12 <- read_csv(here::here("outputs", "8x8_8_100E.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    egg = 100,
    mpa = case_when(
      (lat %in% c(14:21) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(30:37) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  )


output_df13 <- read_csv(here::here("outputs", "8x8_0_10000E.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    egg = 10000,
    mpa = case_when(
      (lat %in% c(22:29) & lon %in% c(22:29)) ~ "MPA 1",
      TRUE ~ "Non-MPA"
    )
  )

output_df14 <- read_csv(here::here("outputs", "8x8_2_10000E.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 2,
    egg = 10000,
    mpa = case_when(
      (lat %in% c(17:24) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(27:34) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  )

output_df15 <- read_csv(here::here("outputs", "8x8_8_10000E.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    egg = 10000,
    mpa = case_when(
      (lat %in% c(14:21) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(30:37) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  )

output_df16 <- read_csv(here::here("outputs", "4x4_0_1E.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 0,
    eggs = 1,
    mpa = case_when(
      (lat %in% c(24:27) & lon %in% c(24:27)) ~ "MPA 1",
      TRUE ~ "Non-MPA"
    )
  )

output_df17 <- read_csv(here::here("outputs", "4x4_0.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 0,
    eggs = 5,
    mpa = case_when(
      (lat %in% c(24:27) & lon %in% c(24:27)) ~ "MPA 1",
      TRUE ~ "Non-MPA"
    )
  )

output_df18 <- read_csv(here::here("outputs", "4x4_0_20E.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 0,
    eggs = 20,
    mpa = case_when(
      (lat %in% c(24:27) & lon %in% c(24:27)) ~ "MPA 1",
      TRUE ~ "Non-MPA"
    )
  )

output_df19 <- read_csv(here::here("outputs", "4x4_0_100E.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 0,
    eggs = 100,
    mpa = case_when(
      (lat %in% c(24:27) & lon %in% c(24:27)) ~ "MPA 1",
      TRUE ~ "Non-MPA"
    )
  )

output_df20 <- read_csv(here::here("outputs", "4x4_0_10000E.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 0,
    eggs = 10000,
    mpa = case_when(
      (lat %in% c(24:27) & lon %in% c(24:27)) ~ "MPA 1",
      TRUE ~ "Non-MPA"
    )
  )

output <- list(
  output_df1, output_df2, output_df3, output_df4, output_df5, output_df6,
  output_df7, output_df8, output_df9, output_df10, output_df11, output_df12,
  output_df13, output_df14, output_df15, output_df16,
  output_df17, output_df18, output_df19, output_df20
) %>%
  reduce(full_join)

rm(
  output_df1, output_df2, output_df3, output_df4, output_df5, output_df6,
  output_df7, output_df8, output_df9, output_df10, output_df11, output_df12,
  output_df13, output_df14, output_df15, output_df16,
  output_df17, output_df18, output_df19, output_df20
)

write_csv(output, here::here("data", "processed_data", "model_results_fecund.csv"))

# Combine Connectivity Data -----------------------------------------------

output_df1 <- read_csv(here::here("outputs", "connectivity_8x8_0_1E.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    eggs = 1
  )

output_df2 <- read_csv(here::here("outputs", "connectivity_8x8_0.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    eggs = 5
  )

output_df3 <- read_csv(here::here("outputs", "connectivity_8x8_0_20E.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    eggs = 20
  )

output_df4 <- read_csv(here::here("outputs", "connectivity_8x8_0_100E.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    eggs = 100
  )

output_df5 <- read_csv(here::here("outputs", "connectivity_8x8_0_10000E.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    eggs = 10000
  )

output_df6 <- read_csv(here::here("outputs", "connectivity_8x8_2_1E.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 2,
    eggs = 1
  )
output_df7 <- read_csv(here::here("outputs", "connectivity_8x8_2.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 2,
    eggs = 5
  )
output_df8 <- read_csv(here::here("outputs", "connectivity_8x8_2_20E.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 2,
    eggs = 20
  )
output_df9 <- read_csv(here::here("outputs", "connectivity_8x8_2_100E.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 2,
    eggs = 100
  )
output_df10 <- read_csv(here::here("outputs", "connectivity_8x8_2_10000E.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 2,
    eggs = 10000
  )

output_df11 <- read_csv(here::here("outputs", "connectivity_8x8_8_1E.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    eggs = 1
  )
output_df12 <- read_csv(here::here("outputs", "connectivity_8x8_8.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    eggs = 5
  )
output_df13 <- read_csv(here::here("outputs", "connectivity_8x8_8_20E.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    eggs = 20
  )
output_df14 <- read_csv(here::here("outputs", "connectivity_8x8_8_100E.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    eggs = 100
  )
output_df15 <- read_csv(here::here("outputs", "connectivity_8x8_8_10000E.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    eggs = 10000
  )

output_df16 <- read_csv(here::here("outputs", "connectivity_4x4_0_1E.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 0,
    eggs = 1
  )

output_df17 <- read_csv(here::here("outputs", "connectivity_4x4_0.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 0,
    eggs = 5
  )

output_df18 <- read_csv(here::here("outputs", "connectivity_4x4_0_20E.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 0,
    eggs = 20
  )

output_df19 <- read_csv(here::here("outputs", "connectivity_4x4_0_100E.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 0,
    eggs = 100
  )

output_df20 <- read_csv(here::here("outputs", "connectivity_4x4_0_10000E.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 0,
    eggs = 10000
  )

output <- list(
  output_df1, output_df2, output_df3, output_df4, output_df5, output_df6,
  output_df7, output_df8, output_df9, output_df10, output_df11, output_df12,
  output_df13, output_df14, output_df15, output_df16,
  output_df17, output_df18, output_df19, output_df20
) %>%
  reduce(full_join)

rm(
  output_df1, output_df2, output_df3, output_df4, output_df5, output_df6,
  output_df7, output_df8, output_df9, output_df10, output_df11, output_df12,
  output_df13, output_df14, output_df15, output_df16,
  output_df17, output_df18, output_df19, output_df20
)

write_csv(output, here::here("data", "processed_data", "connectivity_results_fecund.csv"))
