library(tidyverse)
library(pheatmap)

adult_move <- c(1, 4, 16) # c(1, 2, 4, 8, 16, 32)
larval_move <- c(2, 8, 32) # c(2, 4, 8, 16, 32, 190)
move_combos <- expand.grid(adult_move, larval_move)
names(move_combos) <- c("adult", "larval")

resolution <- c(50, 50)
world <- array(1:2500, resolution)

output_df <- read_csv(here::here("outputs", "8x8_0.csv")) %>%
  mutate(
    mpa_size = 4,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(24:27) & lon %in% c(24:27)) ~ "MPA 1",
      TRUE ~ "Non-MPA"
    )
  )

fish_eq <- output_df %>%
  filter(generation == 40) %>%
  filter(mpa != "Non-MPA")
mpa_eq <- output_df %>%
  filter(generation == 100) %>%
  filter(mpa != "Non-MPA")

rm(output_df)

lat <- sort(unique(mpa_eq$lat))
lon <- sort(unique(mpa_eq$lon))

fish_eq <- fish_eq %>%
  filter(adult == 1) %>%
  filter(larval == 2)
mpa_eq <- mpa_eq %>%
  filter(adult == 1) %>%
  filter(larval == 2)

fish_a_pop <- fish_eq$pop[1]
fish_l_pop <- fish_eq$pop[2]

mpa_a_pop <- mpa_eq %>%
  filter(age == "adult") %>%
  mutate(loc = (lat * 50) - (50 - lon)) %>%
  arrange(loc) %>%
  pull(pop)
mpa_l_pop <- mpa_eq %>%
  filter(age == "larvae") %>%
  mutate(loc = (lat * 50) - (50 - lon)) %>%
  arrange(loc) %>%
  pull(pop)

rm(fish_eq, mpa_eq)

mpa <- as.vector(world[lat, lon])

connect_df = data.frame()

for (i in 1:nrow(move_combos)) {
  print(i)
  # set movement rates
  adult_diffusion <- move_combos$adult[i] # km2/year
  recruit_diffusion <- move_combos$larval[i] # km2/year
  
  # load habitats
  load(here::here("outputs", paste0("adult_diffusion_", move_combos$adult[i], ".rda")))
  load(here::here("outputs", paste0("recruit_diffusion_", move_combos$larval[i], ".rda")))
  
  ### P Matrix - limited utility unless focus is on larval retention
  p_a <- adult_mm
  p_l <- recruit_mm
  
  rm(recruit_mm, adult_mm)
  
  ### F matrix
  # equilibrium prior to MPA
  f_a_fish <- p_a * fish_a_pop
  f_l_fish <- p_l * fish_l_pop
  
  # equilibrium with MPA
  f_a_mpa <- t(p_a * mpa_a_pop)
  f_l_mpa <- t(p_l * mpa_l_pop)
  
  
  ### M matrix
  m_a <- sweep(f_a_fish, 2, colSums(f_a_fish), FUN = "/")
  m_l <- sweep(f_l_fish, 2, colSums(f_l_fish), FUN = "/")
  
  ### Select MPA cells
  p_a = p_a[mpa, mpa]
  p_l = p_l[mpa, mpa]
  f_a_fish = f_a_fish[mpa, mpa]
  f_l_fish = f_l_fish[mpa, mpa]
  f_a_mpa = f_a_mpa[mpa, mpa]
  f_l_mpa = f_l_mpa[mpa, mpa]
  m_a = m_a[mpa, mpa]
  m_l = m_l[mpa, mpa]
  
  ### Connectivity Metrics
  ### Retention = sum x_i L_i,i(M_r) = sum of local retention at MPA sites
  ### Export = sum sum x_i (1 - x_j) L_i,j(M_e) # Do not care about export to unprotected sources
  ### Import = sum sum x_i x_j L_j,i(M_i) # sum of larval import from protected area source j to protected source i
  
  ### where x is the status of the location (always 1 here), L is the dispersal, and M is matrix: P, F, or M
  
  ### Sum of Sums
  ### Mean of Sums
  ### Mean of Means
  
  ### Overall Connectivity (C) = w_r R + w_i I + w_e E; w_r = 1, w_i = 1, w_e = 0
  
  # RETENTION
  
  ### Retention Strength - absolute number of native settlers
  
  RS_a_fish <- mean(diag(f_a_fish))
  RS_l_fish <- mean(diag(f_l_fish))
  RS_a_mpa <- mean(diag(f_a_mpa))
  RS_l_mpa <- mean(diag(f_l_mpa))
  
  ### Self-recruitment - native settlers relative to total settlement
  SR_a <- mean(diag(m_a))
  SR_l <- mean(diag(m_l))
  
  ### Local Retention - Native settlers relative to output
  LR_a <- mean(diag(p_a))
  LR_l <- mean(diag(p_l))
  
  # IMPORT
  diag(f_a_fish) <- 0
  diag(f_l_fish) <- 0
  diag(f_a_mpa) <- 0
  diag(f_l_mpa) <- 0
  diag(m_a) <- 0
  diag(m_l) <- 0
  
  ### Import Strength - non-native settlers
  IS_a_fish <- mean(f_a_fish)
  IS_l_fish <- mean(f_l_fish)
  IS_a_mpa <- mean(f_a_mpa)
  IS_l_mpa <- mean(f_l_mpa)
  
  ### Import Influence - non-native settlers relative to total settlement
  II_a <- mean(m_a)
  II_l <- mean(m_l)
  
  ### Import Diversity - non-native settlers relative to total settlement (weighted)
  ID_a_fish <- mean(f_a_fish^(10^-10))
  ID_l_fish <- mean(f_l_fish^(10^-10))
  ID_a_mpa <- mean(f_a_mpa^(10^-10))
  ID_l_mpa <- mean(f_l_mpa^(10^-10))
  
  c_metrics = c(RS_a_fish, RS_l_fish, RS_a_mpa, RS_l_mpa, SR_a, SR_l, LR_a, LR_l,
                IS_a_fish, IS_l_fish, IS_a_mpa, IS_l_mpa, II_a, II_l,
                ID_a_fish, ID_l_fish, ID_a_mpa, ID_l_mpa, adult_diffusion, recruit_diffusion)
  
  connect_df = rbind(connect_df, c_metrics)
}

names(connect_df) = c("adult_RS_fished", "larvae_RS_fished", "adult_RS_mpa", "larvae_RS_mpa", "adult_SR", "larvae_SR", "adult_LR", "larvae_LR",
                      "adult_IS_fished", "larvae_IS_fished", "adult_IS_mpa", "larvae_IS_mpa", "adult_II", "larvae_II",
                      "adult_ID_fished", "larvae_ID_fished", "adult_ID_mpa", "larvae_ID_mpa", "adult", "larval")

write_csv(connect_df, here::here("outputs", "connectivity_8x8_0.csv"))