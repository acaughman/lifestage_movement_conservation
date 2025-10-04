library(tidyverse)
library(pracma)

addTaskCallback(function(...) {
  set.seed(42)
  TRUE
})
options(warn = -1)
options(dplyr.summarise.inform = FALSE)

n_mort = 0.7

calc_fmsy <- function(num_eggs, h) {
  dd <- (5 * h - 1) / (4 * h * num_eggs)   # density dependence
  s <- n_mort / (1 + dd * num_eggs)            # larval survival
  Rf <- num_eggs * s                           # recruits per female
  r <- log(Rf + n_mort)                        # intrinsic growth rate
  Fmsy <- r / 2                                # fishing mortality at MSY
  H <- 1 - exp(-Fmsy)                          # harvest proportion
  
  return(H)
}

for (reproductive_output in c(10, 100, 10000)) {
  if (reproductive_output == 10) {
    reproductive_output_c <- "low"
  } else if (reproductive_output == 100) {
    reproductive_output_c <- "med"
  } else if (reproductive_output == 10000) {
    reproductive_output_c <- "high"
  }

  # simulation variables
  resolution <- c(50, 50)
  years <- 100 # eventually set up for pre fishing, pre MPA

  age_classes <- 2 # babies, adult
  sexes <- 2 # female, male

  # fish variables
  num_eggs <- reproductive_output # number of eggs per female fish
  n_mort <- 0.7 # natural survival
  h <- 0.7 # density dependence strength

  dd <- (5 * h - 1) / (4 * h * num_eggs) # density dependence for larval mortality

  f_mort_orig <- array(calc_fmsy(num_eggs, h), c(resolution, sexes)) # fishing mortality (same dimension as simulation)

  adult_move <- c(0.5, 8, 32)
  larval_move <- c(8, 32, 192)
  move_combos <- expand.grid(adult_move, larval_move)
  names(move_combos) <- c("adult", "larval")
  move_combos <- move_combos

  initial <- 300

  pop <- array(0, c(resolution, age_classes, sexes)) # initialize grid
  pop[, , 2, ] <- initial / 2 # add initial adults

  output <- array(0, c(resolution, age_classes, sexes, years)) # create array to hold outputs
  output_df <- data.frame() # create dataframe to hold results

  fished_array <- array(0, c(resolution, sexes, years))
  fished_df <- data.frame()

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
    f_mort <- f_mort_orig # fishing mortality (same dimension as simulation)

    for (t in 1:years) {
      print(t)
      output[, , , , t] <- pop

      # births
      pop[, , 1, ] <- pop[, , 2, 1] * num_eggs

      # larvae move
      pop[, , 1, 1] <- rowSums(recruit_movement_matrix * array(rep(pop[, , 1, 1], each = resolution[1] * resolution[2]), c(resolution, resolution[1] * resolution[2])), dims = 2)
      pop[, , 1, 2] <- rowSums(recruit_movement_matrix * array(rep(pop[, , 1, 2], each = resolution[1] * resolution[2]), c(resolution, resolution[1] * resolution[2])), dims = 2)

      # juvenile mortality with density dependence
      pop[, , 1, ] <- pop[, , 1, ] * array((n_mort / (1 + (dd * rowSums(pop[, , 1, ], dim = 2)))), c(resolution, 2))

      # adult natural mortality
      pop[, , 2, ] <- pop[, , 2, ] * n_mort
      # add MPA
      if (t > 40) {
        # resdistribution fishing effort
        if (t == 41) {
          # f_mort[, , ] <- f_mort / (1 - ((2 * 2) / (resolution[1] * resolution[2]))) # size 2x2
          # f_mort[, ,] = f_mort / (1 - ((4 * 4) / (resolution[1] * resolution[2])))
          # f_mort[, ,] = f_mort / (1 - ((4 * 4 * 2) / (resolution[1] * resolution[2])))
          # f_mort[, , ] <- f_mort / (1 - ((8 * 8) / (resolution[1] * resolution[2])))
          f_mort[, , ] <- f_mort / (1 - ((8 * 8 * 2) / (resolution[1] * resolution[2])))
          # f_mort[, ,] = f_mort / (1 - ((16 * 16) / (resolution[1] * resolution[2])))
        }

        # create MPA
        # f_mort[25:26, 25:26, ] <- 0 # size 2x2
        # f_mort[24:27, 24:27, ] <- 0 # size 4x4
        # f_mort[22:29, 22:29, ] <- 0 # size 8x8
        # f_mort[17:32, 17:32, ] <- 0 # size 16x16
        # f_mort[21:24, 24:27, ] <- 0 # size 4x4, spacing 2
        # f_mort[27:30, 24:27, ] <- 0 # size 4x4, spacing 2
        # f_mort[20:23, 24:27, ] <- 0 # size 4x4, spacing 4
        # f_mort[28:31, 24:27, ] <- 0 # size 4x4, spacing 4
        # f_mort[18:21, 24:27, ] <- 0 # size 4x4, spacing 8
        # f_mort[30:33, 24:27, ] <- 0 # size 4x4, spacing 8
        # f_mort[14:17, 24:27, ] <- 0 # size 4x4, spacing 16
        # f_mort[34:37, 24:27, ] <- 0 # size 4x4, spacing 16
        # f_mort[17:24, 22:29, ] <- 0 # size 8x8, spacing 2
        # f_mort[27:34, 22:29, ] <- 0 # size 8x8, spacing 2
        # f_mort[16:23, 22:29, ] <- 0 # size 8x8, spacing 4
        # f_mort[28:35, 22:29, ] <- 0 # size 8x8, spacing 4
        f_mort[14:21, 22:29, ] <- 0 # size 8x8, spacing 8
        f_mort[30:37, 22:29, ] <- 0 # size 8x8, spacing 8
        # f_mort[10:17, 22:29, ] <- 0 # size 8x8, spacing 16
        # f_mort[34:41, 22:29, ] <- 0 # size 8x8, spacing 16
      }

      # fishing mortality
      if (t > 20) {
        fished_array[, , , t] <- pop[, , 2, ] * (f_mort)
        pop[, , 2, ] <- pop[, , 2, ] * (1 - f_mort)
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

    # creates dataframe from fishing array data
    for (a in 1:years) {
      for (b in 1:sexes) {
        fishing <- fished_array[, , b, a] %>%
          as.data.frame()
        fishing$generation <- paste0(a)
        fishing$sex <- paste0(b)
        fishing$lat <- c(1:resolution[2])
        fishing$adult <- adult_diffusion
        fishing$larval <- recruit_diffusion
        fished_df <- bind_rows(fished_df, fishing)
      }
    }

    rm(adult_movement_matrix, recruit_movement_matrix) # remove movement matrices
    gc() # clear memory
  }
  gc()

  end_time <- Sys.time()
  end_time - start_time

  names(output_df) <- c(1:resolution[1], "generation", "sex", "age", "lat", "adult", "larval")
  names(fished_df) <- c(1:resolution[1], "generation", "sex", "lat", "adult", "larval")

  fished_df <- fished_df %>%
    pivot_longer(1:resolution[1],
      names_to = "lon",
      values_to = "fished"
    ) %>%
    mutate(sex = case_when( # assign real values to sex
      sex == 1 ~ "female",
      sex == 2 ~ "male"
    )) %>%
    mutate(sex = as.factor(sex)) %>% # turn sex into factor
    mutate(adult = as.factor(adult)) %>%
    mutate(larval = as.factor(larval)) %>%
    mutate(lat = as.numeric(lat)) %>%
    mutate(lon = as.numeric(lon)) %>%
    mutate(generation = as.numeric(generation)) %>%
    group_by(lat, lon, generation, adult, larval) %>%
    summarize(fished = sum(fished))

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
    mutate(age = as.factor(age)) %>% # age as factor
    mutate(adult = as.factor(adult)) %>%
    mutate(larval = as.factor(larval)) %>%
    mutate(lat = as.numeric(lat)) %>%
    mutate(lon = as.numeric(lon)) %>%
    mutate(generation = as.numeric(generation)) %>%
    mutate(fishing_pressure = calc_fmsy(num_eggs, h)) %>% 
    mutate(reproductive_output = num_eggs) %>% 
    mutate(dd_strength = h) %>% 
    group_by(lat, lon, age, generation, adult, larval, fishing_pressure, reproductive_output, dd_strength) %>%
    summarize(pop = sum(pop))

  output_df <- output_df %>%
    full_join(fished_df)

  write_csv(output_df, here::here("outputs", paste0("8x8_8_", reproductive_output_c, "R0.csv")))

  rm(fished_df, output_df)
  gc()
}

rm(list = ls())
