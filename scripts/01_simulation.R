source(here::here("scripts", "funs", "movement_matrix.R"))

addTaskCallback(function(...) {
  set.seed(42)
  TRUE
})
options(warn = -1)
options(dplyr.summarise.inform = FALSE)

# simulation variables
reps <- 1
resolution <- c(50, 50)
years <- 100 # eventually set up for pre fishing, pre MPA
seasons <- 1

time_step <- 1 / seasons

steps <- years * seasons

patch_area <- 1

age_classes <- 2 # babies, adult
sexes <- 2 # female, male

# fish variables
adult_diffusion <- 2 # km2/year
recruit_diffusion <- 4 # km2/year
num_eggs <- 5 # number of eggs per female fish
dd <- 0.005 # density dependence for larval mortality
n_mort <- 1 - 0.3 # natural mortality
f_mort <- 1 - array(0.5, c(resolution, age_classes)) # fishing mortality (same dimension as simulation)
opt.temp <- 25 # optimal temperature of species
temp.range <- 4 # thermal breath of species

initial <- 100

max_hab_mult <- 2

# habitats
adult_habitat <- expand_grid(x = 1:resolution[1], y = 1:resolution[2]) %>%
  mutate(
    habitat = 1,
    habitat = habitat / max(habitat) * adult_diffusion
  ) %>%
  pivot_wider(names_from = y, values_from = habitat) %>%
  select(-x) %>%
  as.matrix()

recruit_habitat <- expand_grid(x = 1:resolution[1], y = 1:resolution[2]) %>%
  mutate(
    habitat = 1,
    habitat = habitat / max(habitat) * recruit_diffusion
  ) %>%
  pivot_wider(names_from = y, values_from = habitat) %>%
  select(-x) %>%
  as.matrix()

# adult_mm <- movement_matrix(time_step, resolution, adult_habitat)

# save(adult_mm, file = here::here("outputs", "adult_diffusion_2.rda"))

load(here::here("outputs", "adult_diffusion_2.rda"))

adult_movement_matrix <- array(0, dim = c(resolution[1], resolution[2], nrow(adult_mm)))

for (i in 1:nrow(adult_mm)) {
  adult_movement_matrix[, , i] <- t(Reshape(adult_mm[, i], resolution[1], resolution[2]))
}
#
# recruit_mm <- movement_matrix(time_step, resolution, recruit_habitat)

# save(recruit_mm, file = here::here("outputs", "recruit_diffusion_4.rda"))

load(here::here("outputs", "recruit_diffusion_4.rda"))

recruit_movement_matrix <- array(0, dim = c(resolution[1], resolution[2], nrow(recruit_mm)))

for (i in 1:nrow(recruit_mm)) {
  recruit_movement_matrix[, , i] <- t(Reshape(recruit_mm[, i], resolution[1], resolution[2]))
}

pop <- array(0, c(resolution, age_classes, sexes)) # initialize grid
pop[, , 2, ] <- initial / 2 # add initial adults

output <- array(0, c(resolution, age_classes, sexes, years, reps)) # create array to hold outputs

start_time <- Sys.time()

for (rep in 1:reps) {
  print(rep)
  for (t in 1:years) {
    print(t)
    output[, , , , t, rep] <- pop
    # births
    pop[, , 1, ] <- pop[, , 2, 1] * num_eggs / 2
    # larvae move
    pop[, , 1, 1] <- rowSums(recruit_movement_matrix * array(rep(pop[, , 1, 1], each = resolution[1] * resolution[2]), c(resolution, resolution[1] * resolution[2])), dims = 2)
    pop[, , 1, 2] <- rowSums(recruit_movement_matrix * array(rep(pop[, , 1, 2], each = resolution[1] * resolution[2]), c(resolution, resolution[1] * resolution[2])), dims = 2)
    # juvenile mortality with density dependence
    pop[, , 1, ] <- pop[, , 1, ] * array((n_mort / (1 + dd * rowSums(pop[, , 1, ], dim = 2))), c(resolution, 2))
    # adult natural mortality
    pop[, , 2, ] <- pop[, , 2, ] * n_mort
    # add MPA
    if (t > 30) {
      # f_mort[25:26, 25:26, ] <- 1 # size 2x2
      f_mort[24:27, 24:27, ] <- 1 # size 4x4
      # f_mort[22:29, 22:29, ] <- 1 # size 8x8
      # f_mort[17:32, 17:32, ] <- 1 # size 16x16
      # f_mort[24:27, 21:24, ] <- 1 # size 4x4, spacing 2
      # f_mort[24:27, 27:30, ] <- 1 # size 4x4, spacing 2
      # f_mort[24:27, 19:23, ] <- 1 # size 4x4, spacing 4
      # f_mort[24:27, 28:31, ] <- 1 # size 4x4, spacing 4
      # f_mort[24:27, 18:21, ] <- 1 # size 4x4, spacing 8
      # f_mort[24:27, 30:33, ] <- 1 # size 4x4, spacing 8
      # f_mort[24:27, 13:17, ] <- 1 # size 4x4, spacing 16
      # f_mort[24:27, 34:37, ] <- 1 # size 4x4, spacing 16
      # f_mort[22:29, 17:24, ] <- 1 # size 8x8, spacing 2
      # f_mort[22:29, 27:34, ] <- 1 # size 8x8, spacing 2
      # f_mort[22:29, 18:23, ] <- 1 # size 8x8, spacing 4
      # f_mort[22:29, 28:25, ] <- 1 # size 8x8, spacing 4
      # f_mort[22:29, 14:21, ] <- 1 # size 8x8, spacing 8
      # f_mort[22:29, 30:37, ] <- 1 # size 8x8, spacing 8
      # f_mort[22:29, 9:17, ] <- 1 # size 8x8, spacing 16
      # f_mort[22:29, 34:41, ] <- 1 # size 8x8, spacing 16
    }
    # fishing mortality
    if (t > 20) {
      pop[, , 2, ] <- pop[, , 2, ] * f_mort
    }
    # adult move
    pop[, , 2, 1] <- rowSums(adult_movement_matrix * array(rep(pop[, , 2, 1], each = resolution[1] * resolution[2]), c(resolution, resolution[1] * resolution[2])), dims = 2)
    pop[, , 2, 2] <- rowSums(adult_movement_matrix * array(rep(pop[, , 2, 2], each = resolution[1] * resolution[2]), c(resolution, resolution[1] * resolution[2])), dims = 2)
    # larvae become adults
    pop[, , 2, ] <- pop[, , 1, ] + pop[, , 2, ]

    output[, , 1, , t, rep] <- pop[, , 1, ]
    pop[, , 1, ] <- 0
  }
  gc() # clear memory
}
gc()

end_time <- Sys.time()
end_time - start_time

# Output results into a dataframe
output_df <- data.frame() # create dataframe to hold results

# creates dataframe from array data
for (a in 1:reps) {
  for (b in 1:years) {
    for (c in 1:sexes) {
      for (d in 1:age_classes) {
        world_sub <- output[, , d, c, b, a] %>%
          as.data.frame()
        world_sub$rep <- paste0(a)
        world_sub$generation <- paste0(b)
        world_sub$sex <- paste0(c)
        world_sub$age <- paste0(d)
        world_sub$lat <- c(1:resolution[2])
        output_df <- bind_rows(output_df, world_sub)
      }
    }
  }
}

names(output_df) <- c(1:resolution[1], "rep", "generation", "sex", "age", "lat")

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
  mutate(lat = as.numeric(lat)) %>%
  mutate(lon = as.numeric(lon)) %>%
  mutate(rep = as.numeric(rep)) %>%
  mutate(generation = as.numeric(generation)) %>%
  group_by(lat, lon, rep, age, generation) %>%
  summarize(pop = sum(pop))

write_csv(output_df, here::here("outputs", "base_model_4x4.csv"))
