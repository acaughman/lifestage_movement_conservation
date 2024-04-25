library(tidyverse)
library(pracma)

addTaskCallback(function(...) {
  set.seed(42)
  TRUE
})
options(warn = -1)
options(dplyr.summarise.inform = FALSE)

# simulation variables
resolution <- c(50, 50)
years <- 100 # eventually set up for pre fishing, pre MPA

age_classes <- 2 # babies, adult
sexes <- 2 # female, male

# fish variables
num_eggs <- 5 # number of eggs per female fish
n_mort <- 1 - 0.3 # natural mortality
f_mort <- 1 - array(0.5, c(resolution, sexes)) # fishing mortality (same dimension as simulation)
opt.temp <- 25 # optimal temperature of species
temp.range <- 4 # thermal breath of species

adult_move <- c(1, 2, 4, 8, 16, 32)
larval_move <- c(1, 2, 4, 8, 16, 32)
move_combos <- expand.grid(adult_move, larval_move)
names(move_combos) <- c("adult", "larval")

initial <- 100
recruit_0 <- initial * num_eggs

dd <- (5 * 0.7 - 1) / (4 * .7 * recruit_0) # density dependence for larval mortality
a <- (initial / recruit_0) - initial * dd # slope of beverton holt function

pop <- array(0, c(resolution, age_classes, sexes)) # initialize grid
pop[, , 2, ] <- initial / 2 # add initial adults

output <- array(0, c(resolution, age_classes, sexes, years)) # create array to hold outputs
output_df <- data.frame() # create dataframe to hold results

# # Simulation --------------------------------------------------------------

start_time <- Sys.time()

for (i in 1:nrow(move_combos)) {
  print(i)
  # set movement rates
  adult_diffusion <- move_combos$adult[i] # km2/year
  recruit_diffusion <- move_combos$larval[i] # km2/year

  # load habitats
  load(here::here("outputs", paste0("adult_movement_matrix_", move_combos$adult[i], ".rda")))
  load(here::here("outputs", paste0("recruit_movement_matrix_", move_combos$larval[i], ".rda")))

  # reset fishing
  f_mort <- 1 - array(0.5, c(resolution, sexes)) # fishing mortality (same dimension as simulation)

  for (t in 1:years) {
    print(t)
    output[, , , , t] <- pop
    # births
    pop[, , 1, ] <- pop[, , 2, 1] * num_eggs / 2
    # larvae move
    pop[, , 1, 1] <- rowSums(recruit_movement_matrix * array(rep(pop[, , 1, 1], each = resolution[1] * resolution[2]), c(resolution, resolution[1] * resolution[2])), dims = 2)
    pop[, , 1, 2] <- rowSums(recruit_movement_matrix * array(rep(pop[, , 1, 2], each = resolution[1] * resolution[2]), c(resolution, resolution[1] * resolution[2])), dims = 2)
    # juvenile mortality with density dependence
    pop[, , 1, ] <- pop[, , 1, ] * array((n_mort / (a + (dd * rowSums(pop[, , 1, ], dim = 2)))), c(resolution, 2))
    # adult natural mortality
    pop[, , 2, ] <- pop[, , 2, ] * n_mort
    # add MPA
    if (t > 40) {
      # resdistribution fishing effort
      f_mort[, , ] <- f_mort / (1 - ((2 * 2) / (resolution[1] * resolution[2]))) # size 2x2
      # f_mort[, ,] = f_mort / (1 - ((4 * 4) / (resolution[1] * resolution[2])))
      # f_mort[, ,] = f_mort / (1 - ((8 * 8) / (resolution[1] * resolution[2])))
      # f_mort[, ,] = f_mort / (1 - ((16 * 16) / (resolution[1] * resolution[2])))

      # create MPA
      f_mort[25:26, 25:26, ] <- 1 # size 2x2
      # f_mort[24:27, 24:27, ] <- 1 # size 4x4
      # f_mort[22:29, 22:29, ] <- 1 # size 8x8
      # f_mort[17:32, 17:32, ] <- 1 # size 16x16
      # f_mort[21:24, 24:27, ] <- 1 # size 4x4, spacing 2
      # f_mort[27:30, 24:27, ] <- 1 # size 4x4, spacing 2
      # f_mort[20:23, 24:27, ] <- 1 # size 4x4, spacing 4
      # f_mort[28:31, 24:27, ] <- 1 # size 4x4, spacing 4
      # f_mort[18:21, 24:27, ] <- 1 # size 4x4, spacing 8
      # f_mort[30:33, 24:27, ] <- 1 # size 4x4, spacing 8
      # f_mort[14:17, 24:27, ] <- 1 # size 4x4, spacing 16
      # f_mort[34:37, 24:27, ] <- 1 # size 4x4, spacing 16
      # f_mort[17:24, 22:29, ] <- 1 # size 8x8, spacing 2
      # f_mort[27:34, 22:29, ] <- 1 # size 8x8, spacing 2
      # f_mort[16:23, 22:29, ] <- 1 # size 8x8, spacing 4
      # f_mort[28:35, 22:29, ] <- 1 # size 8x8, spacing 4
      # f_mort[14:21, 22:29, ] <- 1 # size 8x8, spacing 8
      # f_mort[30:37, 22:29, ] <- 1 # size 8x8, spacing 8
      # f_mort[10:17, 22:29, ] <- 1 # size 8x8, spacing 16
      # f_mort[34:41, 22:29, ] <- 1 # size 8x8, spacing 16
    }
    # fishing mortality
    if (t > 20) {
      pop[, , 2, ] <- pop[, , 2, ] * f_mort
    }
    # adult move
    pop[, , 2, 1] <- rowSums(adult_movement_matrix * array(rep(pop[, , 2, 1], each = resolution[1] * resolution[2]), c(resolution, resolution[1] * resolution[2])), dims = 2)
    pop[, , 2, 2] <- rowSums(adult_movement_matrix * array(rep(pop[, , 2, 1], each = resolution[1] * resolution[2]), c(resolution, resolution[1] * resolution[2])), dims = 2)
    # larvae become adults
    pop[, , 2, ] <- pop[, , 1, ] + pop[, , 2, ]

    output[, , 1, , t] <- pop[, , 1, ]
    pop[, , 1, ] <- 0
  }

  # creates dataframe from array data
  for (a in 1:years) {
    for (b in 1:sexes) {
      for (c in 1:age_classes) {
        world_sub <- output[, , c, b, a] %>%
          as.data.frame()
        world_sub$generation <- paste0(a)
        world_sub$sex <- paste0(b)
        world_sub$age <- paste0(c)
        world_sub$lat <- c(1:resolution[2])
        world_sub$adult <- adult_diffusion
        world_sub$larval <- recruit_diffusion
        output_df <- bind_rows(output_df, world_sub)
      }
    }
  }

  rm(adult_movement_matrix, recruit_movement_matrix) # remove movement matrices
  gc() # clear memory
}
gc()

end_time <- Sys.time()
end_time - start_time

names(output_df) <- c(1:resolution[1], "generation", "sex", "age", "lat", "adult", "larval")

output_df <- output_df %>%
  pivot_longer(1:resolution[1],
    names_to = "lon",
    values_to = "pop"
  ) %>%
  mutate(sex = case_when( # assign real values to sex
    sex == 1 ~ "female",
    sex == 2 ~ "male"
  )) %>%
  mutate(sex = as.factor(sex)) %>% # turn sex into factor
  mutate(age = case_when( # assign real values to age
    age == 1 ~ "larvae",
    age == 2 ~ "adult"
  )) %>%
  mutate(sex = as.factor(sex)) %>%
  mutate(age = as.factor(age)) %>% # age as factor
  mutate(sex = as.factor(adult)) %>%
  mutate(sex = as.factor(larval)) %>%
  mutate(lat = as.numeric(lat)) %>%
  mutate(lon = as.numeric(lon)) %>%
  mutate(generation = as.numeric(generation)) %>%
  group_by(lat, lon, age, generation, adult, larval) %>%
  summarize(pop = sum(pop))

write_csv(output_df, here::here("outputs", "4x4_0.csv"))
