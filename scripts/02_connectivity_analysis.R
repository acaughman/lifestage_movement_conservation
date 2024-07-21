library(tidyverse)
library(pheatmap)

resolution <- c(50, 50)
adult_move <- c(1, 2, 4, 8, 16, 32) # c(1, 4, 16) 
larval_move <-  c(2, 4, 8, 16, 32, 190) #c(2, 8, 32)
move_combos <- expand.grid(adult_move, larval_move)
names(move_combos) <- c("adult", "larval")

resolution <- c(50, 50)
world <- array(1:2500, resolution)

fp <- 0.5

output_df <- read_csv(here::here("outputs", "8x8_0.csv")) %>%
  mutate(
    mpa_size = 8,
    mpa_spacing = 0,
    mpa = case_when(
      (lat %in% c(22:29) & lon %in% c(22:29)) ~ "MPA 1",
      TRUE ~ "Non-MPA"
    )
  )

fish_eq <- output_df %>%
  filter(generation == 40)
mpa_eq <- output_df %>%
  filter(generation == 100)

mpa1_df <- output_df %>%
  filter(mpa == "MPA 1")
mpa2_df <- output_df %>%
  filter(mpa == "MPA 2")

rm(output_df)

lat1 <- sort(unique(mpa1_df$lat))
lon1 <- sort(unique(mpa1_df$lon))

lat2 <- sort(unique(mpa2_df$lat))
lon2 <- sort(unique(mpa2_df$lon))

mpa1 <- as.vector(world[lat1, lon1])
mpa2 <- as.vector(world[lat2, lon2])
non_mpa <- world[-mpa1]
if (length(mpa2) != 0) {
  non_mpa <- non_mpa[-mpa2]
}


connect_df <- data.frame()

for (i in 1:nrow(move_combos)) {
  print(i)

  # set population based on data
  fish_eq_i <- fish_eq %>%
    filter(adult == move_combos$adult[i]) %>%
    filter(larval == move_combos$larval[i])
  mpa_eq_i <- mpa_eq %>%
    filter(adult == move_combos$adult[i]) %>%
    filter(larval == move_combos$larval[i])

  fish_a_pop <- fish_eq_i$pop[1] ## Update to just be on export related things
  fish_l_pop <- fish_eq_i$pop[2]

  mpa_a_pop <- mpa_eq_i %>%
    filter(age == "adult") %>%
    mutate(loc = (lat * 50) - (50 - lon)) %>%
    arrange(loc) %>%
    pull(pop)
  mpa_l_pop <- mpa_eq_i %>%
    filter(age == "larvae") %>%
    mutate(loc = (lat * 50) - (50 - lon)) %>%
    arrange(loc) %>%
    pull(pop)

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
  f_a_mpa <- p_a * mpa_a_pop
  f_l_mpa <- p_l * mpa_l_pop

  # equilibrium prior to MPA and fishing of adults considered
  f_a_fish_f <- p_a * fish_a_pop * fp

  # equilibrium with MPA and fishing of adults considered
  f_a_mpa_f <- p_a * mpa_a_pop * fp

  ### M matrix
  m_a <- sweep(f_a_fish, 2, colSums(f_a_fish), FUN = "/")
  m_l <- sweep(f_l_fish, 2, colSums(f_l_fish), FUN = "/")

  ### M matrix with fishing of adults
  m_a_f <- sweep(f_a_fish_f, 2, colSums(f_a_fish), FUN = "/")

  ### Select MPA cells
  # p_a <- p_a[mpa, mpa]
  # p_l <- p_l[mpa, mpa]
  # m_a = m_a[mpa, mpa]
  # m_l = m_l[mpa, mpa]
  # f_a_fish = f_a_fish[mpa, mpa]
  # f_l_fish = f_l_fish[mpa, mpa]
  # f_a_mpa =f_a_mpa[mpa, mpa]
  # f_l_mpa= f_l_mpa[mpa, mpa]

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

  RS_a_fish <- mean(f_a_fish[mpa1, mpa1])
  RS_l_fish <- mean(f_l_fish[mpa1, mpa1])
  RS_a_mpa <- mean(f_a_mpa[mpa1, mpa1])
  RS_l_mpa <- mean(f_l_mpa[mpa1, mpa1])

  ### Self-recruitment - native settlers relative to total settlement
  SR_a <- mean(m_a[mpa1, mpa1])
  SR_l <- mean(m_l[mpa1, mpa1])

  ### Local Retention - Native settlers relative to output
  LR_a <- mean(p_a[mpa1, mpa1])
  LR_l <- mean(p_l[mpa1, mpa1])

  ### Import Strength - non-native settlers from other MPA
  IS_a_fish <- mean(f_a_fish_f[mpa2, mpa1])
  IS_l_fish <- mean(f_l_fish[mpa2, mpa1])
  IS_a_mpa <- mean(f_a_mpa_f[mpa2, mpa1])
  IS_l_mpa <- mean(f_l_mpa[mpa2, mpa1])

  ### Import Influence - non-native settlers relative to total settlement from other MPA
  II_a <- mean(m_a_f[mpa2, mpa1])
  II_l <- mean(m_l[mpa2, mpa1])

  ### Import Diversity - non-native settlers relative to total settlement from other MPA (weighted)
  ID_a_fish <- mean((f_a_fish_f[mpa2, mpa1] * fp)^(10^-10))
  ID_l_fish <- mean((f_l_fish[mpa2, mpa1])^(10^-10))
  ID_a_mpa <- mean((f_a_mpa_f[mpa2, mpa1] * fp)^(10^-10))
  ID_l_mpa <- mean((f_l_mpa[mpa2, mpa1])^(10^-10))

  #### EXPORT

  ### Export Strength - external settlers
  ES_a_fish <- mean(f_a_fish[mpa1, non_mpa])
  ES_l_fish <- mean(f_l_fish[mpa1, non_mpa])
  ES_a_mpa <- mean(f_a_mpa[mpa1, non_mpa])
  ES_l_mpa <- mean(f_l_mpa[mpa1, non_mpa])

  ### Export Influence - exports relative to total exports
  EI_a <- mean(m_a_f[mpa1, non_mpa])
  EI_l <- mean(m_l[mpa1, non_mpa])

  ### Export Diversity -  exports relative to total exports (weighted)
  ED_a_fish <- mean((f_a_fish[mpa1, non_mpa] * fp)^(10^-10))
  ED_l_fish <- mean((f_l_fish[mpa1, non_mpa])^(10^-10))
  ED_a_mpa <- mean((f_a_mpa[mpa1, non_mpa] * fp)^(10^-10))
  ED_l_mpa <- mean((f_l_mpa[mpa1, non_mpa])^(10^-10))


  c_metrics <- c(
    RS_a_fish, RS_l_fish, RS_a_mpa, RS_l_mpa, SR_a, SR_l, LR_a, LR_l,
    IS_a_fish, IS_l_fish, IS_a_mpa, IS_l_mpa, II_a, II_l,
    ID_a_fish, ID_l_fish, ID_a_mpa, ID_l_mpa, 
    ES_a_fish, ES_l_fish, ES_a_mpa, ES_l_mpa, EI_a, EI_l,
    ED_a_fish, ED_l_fish, ED_a_mpa, ED_l_mpa,
    adult_diffusion, recruit_diffusion
  )

  connect_df <- rbind(connect_df, c_metrics)
}

names(connect_df) <- c(
  "adult_RS_fished", "larvae_RS_fished", "adult_RS_mpa", "larvae_RS_mpa", "adult_SR", "larvae_SR", "adult_LR", "larvae_LR",
  "adult_IS_fished", "larvae_IS_fished", "adult_IS_mpa", "larvae_IS_mpa", "adult_II", "larvae_II",
  "adult_ID_fished", "larvae_ID_fished", "adult_ID_mpa", "larvae_ID_mpa", 
  "adult_ES_fished", "larvae_ES_fished", "adult_ES_mpa", "larvae_ES_mpa", "adult_EI", "larvae_EI",
  "adult_ED_fished", "larvae_ED_fished", "adult_ED_mpa", "larvae_ED_mpa", 
  "adult", "larval"
)

write_csv(connect_df, here::here("outputs", "connectivity_8x8_0.csv"))
