library(tidyverse)

# Combine Biomass Data ------------------------------------------------------------

output_df1 <- read_csv(here::here("outputs", "2x2_0.csv")) %>%
  mutate(
    mpa_size = 2,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(25, 26) & lon %in% c(25, 26)) ~ "MPA 1",
      TRUE ~ "Non-MPA"
    )
  )
output_df2 <- read_csv(here::here("outputs", "4x4_0.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(24:27) & lon %in% c(24:27)) ~ "MPA 1",
      TRUE ~ "Non-MPA"
    )
  )

output_df3 <- read_csv(here::here("outputs", "8x8_0.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(22:29) & lon %in% c(22:29)) ~ "MPA 1",
      TRUE ~ "Non-MPA"
    )
  )

output_df4 <- read_csv(here::here("outputs", "16x16_0.csv")) %>%
  mutate(
    mpa_size = 16,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(17:32) & lon %in% c(17:32)) ~ "MPA 1",
      TRUE ~ "Non-MPA"
    )
  )
output_df5 <- read_csv(here::here("outputs", "4x4_2.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 2,
    mpa = case_when(
      (lat %in% c(21:24) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(27:30) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  )
output_df6 <- read_csv(here::here("outputs", "4x4_4.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 4,
    mpa = case_when(
      (lat %in% c(20:23) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(28:31) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  )
output_df7 <- read_csv(here::here("outputs", "4x4_8.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 8,
    mpa = case_when(
      (lat %in% c(18:21) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(30:33) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  )
output_df8 <- read_csv(here::here("outputs", "4x4_16.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 16,
    mpa = case_when(
      (lat %in% c(14:17) & lon %in% c(24:27)) ~ "MPA 1",
      (lat %in% c(34:37) & lon %in% c(24:27)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  )
output_df9 <- read_csv(here::here("outputs", "8x8_2.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 2,
    mpa = case_when(
      (lat %in% c(17:24) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(27:34) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  )
output_df10 <- read_csv(here::here("outputs", "8x8_4.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 4,
    mpa = case_when(
      (lat %in% c(16:23) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(28:35) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  )

output_df11 <- read_csv(here::here("outputs", "8x8_8.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    mpa = case_when(
      (lat %in% c(14:21) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(30:37) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  )

output_df12 <- read_csv(here::here("outputs", "8x8_16.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 16,
    mpa = case_when(
      (lat %in% c(10:17) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(34:41) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    )
  )

output <- list(
  output_df1, output_df2, output_df3, output_df4, output_df5, output_df6,
  output_df7, output_df8, output_df9, output_df10, output_df11, output_df12
) %>%
  reduce(full_join)

rm(
  output_df1, output_df2, output_df3, output_df4, output_df5, output_df6,
  output_df7, output_df8, output_df9, output_df10, output_df11, output_df12
)

write_csv(output, here::here("data", "processed_data", "model_results.csv"))

# Combine Connectivity Data -----------------------------------------------

output_df1 <- read_csv(here::here("outputs", "connectivity_2x2_0.csv")) %>%
  mutate(
    mpa_size = 2,
    mpa_spacing = 0
  )
output_df2 <- read_csv(here::here("outputs", "connectivity_4x4_0.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 0
  )
output_df3 <- read_csv(here::here("outputs", "connectivity_8x8_0.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0
  )
output_df4 <- read_csv(here::here("outputs", "connectivity_16x16_0.csv")) %>%
  mutate(
    mpa_size = 16,
    mpa_spacing = 0
  )
output_df5 <- read_csv(here::here("outputs", "connectivity_4x4_2.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 2
  )
output_df6 <- read_csv(here::here("outputs", "connectivity_4x4_4.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 4
  )
output_df7 <- read_csv(here::here("outputs", "connectivity_4x4_8.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 8
  )
output_df8 <- read_csv(here::here("outputs", "connectivity_4x4_16.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 16
  )
output_df9 <- read_csv(here::here("outputs", "connectivity_8x8_2.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 2
  )
output_df10 <- read_csv(here::here("outputs", "connectivity_8x8_4.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 4
  )
output_df11 <- read_csv(here::here("outputs", "connectivity_8x8_8.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8
  )
output_df12 <- read_csv(here::here("outputs", "connectivity_8x8_16.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 16
  )

output <- list(
  output_df1, output_df2, output_df3, output_df4, output_df5, output_df6,
  output_df7, output_df8, output_df9, output_df10, output_df11, output_df12
) %>%
  reduce(full_join)

rm(
  output_df1, output_df2, output_df3, output_df4, output_df5, output_df6,
  output_df7, output_df8, output_df9, output_df10, output_df11, output_df12
)

write_csv(output, here::here("data", "processed_data", "connectivity_results.csv"))


# Combine Sensitivity Biomass ---------------------------------------------

output_df1 <- read_csv(here::here("outputs", "8x8_0.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(22:29) & lon %in% c(22:29)) ~ "MPA 1",
      TRUE ~ "Non-MPA"
    ),
    eggs = "low",
    fp = "med"
  )

output_df2 <- read_csv(here::here("outputs", "8x8_8.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    mpa = case_when(
      (lat %in% c(14:21) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(30:37) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    eggs = "low",
    fp = "med"
  )

output_df3 <- read_csv(here::here("outputs", "8x8_0_lowF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(22:29) & lon %in% c(22:29)) ~ "MPA 1",
      TRUE ~ "Non-MPA"
    ),
    eggs = "low",
    fp = "low"
  )

output_df4 <- read_csv(here::here("outputs", "8x8_8_lowF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    mpa = case_when(
      (lat %in% c(14:21) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(30:37) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    eggs = "low",
    fp = "low"
  )

output_df5 <- read_csv(here::here("outputs", "8x8_0_highF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(22:29) & lon %in% c(22:29)) ~ "MPA 1",
      TRUE ~ "Non-MPA"
    ),
    eggs = "low",
    fp = "high"
  )

output_df6 <- read_csv(here::here("outputs", "8x8_8_highF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    mpa = case_when(
      (lat %in% c(14:21) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(30:37) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    eggs = "low",
    fp = "high"
  )

output_df7 <- read_csv(here::here("outputs", "8x8_0_medE.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(22:29) & lon %in% c(22:29)) ~ "MPA 1",
      TRUE ~ "Non-MPA"
    ),
    eggs = "med",
    fp = "med"
  )

output_df8 <- read_csv(here::here("outputs", "8x8_8_medE.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    mpa = case_when(
      (lat %in% c(14:21) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(30:37) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    eggs = "med",
    fp = "med"
  )

output_df9 <- read_csv(here::here("outputs", "8x8_0_highE.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(22:29) & lon %in% c(22:29)) ~ "MPA 1",
      TRUE ~ "Non-MPA"
    ),
    eggs = "high",
    fp = "med"
  )

output_df10 <- read_csv(here::here("outputs", "8x8_8_highE.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    mpa = case_when(
      (lat %in% c(14:21) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(30:37) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    eggs = "high",
    fp = "med"
  )

output_df11 <- read_csv(here::here("outputs", "8x8_0_medEhighF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(22:29) & lon %in% c(22:29)) ~ "MPA 1",
      TRUE ~ "Non-MPA"
    ),
    eggs = "med",
    fp = "high"
  )

output_df12 <- read_csv(here::here("outputs", "8x8_8_medEhighF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    mpa = case_when(
      (lat %in% c(14:21) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(30:37) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    eggs = "med",
    fp = "high"
  )

output_df13 <- read_csv(here::here("outputs", "8x8_0_highEhighF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(22:29) & lon %in% c(22:29)) ~ "MPA 1",
      TRUE ~ "Non-MPA"
    ),
    eggs = "high",
    fp = "high"
  )

output_df14 <- read_csv(here::here("outputs", "8x8_8_highEhighF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    mpa = case_when(
      (lat %in% c(14:21) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(30:37) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    eggs = "high",
    fp = "high"
  )

output_df15 <- read_csv(here::here("outputs", "8x8_0_medElowF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(22:29) & lon %in% c(22:29)) ~ "MPA 1",
      TRUE ~ "Non-MPA"
    ),
    eggs = "med",
    fp = "low"
  )

output_df16 <- read_csv(here::here("outputs", "8x8_8_medElowF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    mpa = case_when(
      (lat %in% c(14:21) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(30:37) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    eggs = "med",
    fp = "low"
  )

output_df17 <- read_csv(here::here("outputs", "8x8_0_highElowF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(22:29) & lon %in% c(22:29)) ~ "MPA 1",
      TRUE ~ "Non-MPA"
    ),
    eggs = "high",
    fp = "low"
  )

output_df18 <- read_csv(here::here("outputs", "8x8_8_highElowF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    mpa = case_when(
      (lat %in% c(14:21) & lon %in% c(22:29)) ~ "MPA 1",
      (lat %in% c(30:37) & lon %in% c(22:29)) ~ "MPA 2",
      TRUE ~ "Non-MPA"
    ),
    eggs = "high",
    fp = "low"
  )

output <- list(
  output_df1, output_df2, output_df3, output_df4, output_df5, output_df6,
  output_df7, output_df8, output_df9, output_df10, output_df11, output_df12,
  output_df13, output_df14, output_df15, output_df16, output_df17, output_df18
) %>%
  reduce(full_join) %>% 
  filter(adult %in% c(2, 8, 16)) %>% 
  filter(larval %in% c(2, 8, 16))

rm(
  output_df1, output_df2, output_df3, output_df4, output_df5, output_df6,
  output_df7, output_df8, output_df9, output_df10, output_df11, output_df12,
  output_df13, output_df14, output_df15, output_df16, output_df17, output_df18
)

write_csv(output, here::here("data", "processed_data", "sensitivity_results.csv"))


# Combine Sensitivity Connectivity ----------------------------------------

output_df1 <- read_csv(here::here("outputs", "connectivity_8x8_0.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    eggs = "low",
    fp = "med"
  )

output_df2 <- read_csv(here::here("outputs", "connectivity_8x8_8.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    eggs = "low",
    fp = "med"
  )

output_df3 <- read_csv(here::here("outputs", "8x8_0_lowF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    eggs = "low",
    fp = "low"
  )

output_df4 <- read_csv(here::here("outputs", "8x8_8_lowF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    eggs = "low",
    fp = "low"
  )

output_df5 <- read_csv(here::here("outputs", "8x8_0_highF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    eggs = "low",
    fp = "high"
  )

output_df6 <- read_csv(here::here("outputs", "8x8_8_highF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    eggs = "low",
    fp = "high"
  )

output_df7 <- read_csv(here::here("outputs", "8x8_0_medE.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    eggs = "med",
    fp = "med"
  )

output_df8 <- read_csv(here::here("outputs", "8x8_8_medE.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    eggs = "med",
    fp = "med"
  )

output_df9 <- read_csv(here::here("outputs", "8x8_0_highE.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    eggs = "high",
    fp = "med"
  )

output_df10 <- read_csv(here::here("outputs", "8x8_8_highE.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    eggs = "high",
    fp = "med"
  )

output_df11 <- read_csv(here::here("outputs", "8x8_0_medEhighF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    eggs = "med",
    fp = "high"
  )

output_df12 <- read_csv(here::here("outputs", "8x8_8_medEhighF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    eggs = "med",
    fp = "high"
  )

output_df13 <- read_csv(here::here("outputs", "8x8_0_highEhighF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    eggs = "high",
    fp = "high"
  )

output_df14 <- read_csv(here::here("outputs", "8x8_8_highEhighF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    eggs = "high",
    fp = "high"
  )

output_df15 <- read_csv(here::here("outputs", "8x8_0_medElowF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    eggs = "med",
    fp = "low"
  )

output_df16 <- read_csv(here::here("outputs", "8x8_8_medElowF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    eggs = "med",
    fp = "low"
  )

output_df17 <- read_csv(here::here("outputs", "8x8_0_highElowF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    eggs = "high",
    fp = "low"
  )

output_df18 <- read_csv(here::here("outputs", "8x8_8_highElowF.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 8,
    eggs = "high",
    fp = "low"
  )

output <- list(
  output_df1, output_df2, output_df3, output_df4, output_df5, output_df6,
  output_df7, output_df8, output_df9, output_df10, output_df11, output_df12,
  output_df13, output_df14, output_df15, output_df16, output_df17, output_df18
) %>%
  reduce(full_join)

rm(
  output_df1, output_df2, output_df3, output_df4, output_df5, output_df6,
  output_df7, output_df8, output_df9, output_df10, output_df11, output_df12,
  output_df13, output_df14, output_df15, output_df16, output_df17, output_df18
)

write_csv(output, here::here("data", "processed_data", "sensitivity_results_connectivity.csv"))
