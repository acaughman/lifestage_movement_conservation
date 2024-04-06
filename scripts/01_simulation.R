source(here::here("scripts", "funs", "movement_matrix.R"))
source(here::here("scripts", "funs", "spawn.R"))


addTaskCallback(function(...) {
  set.seed(42)
  TRUE
})
options(warn = -1)
options(dplyr.summarise.inform = FALSE)

# set up variables
reps = 1
resolution <- c(10, 10)
years <- 100 # eventually set up for pre fishing, pre MPA
seasons <- 1

time_step <- 1 / seasons

steps <- years * seasons

patch_area <- 1

initial <- 100

age_classes <- 2 # babies, adult
sexes <- 2 # female, male

max_hab_mult = 2

# fish variables
adult_diffusion <- 1.5 # km2/year
recruit_diffusion <- 3 # km2/year
num_eggs <- 2
dd <- 0.0001
n_mort <- 0.3
maturity_age <- 3
f_mort <- array(0.5, resolution)
opt.temp <- 25 # optimal temperature of species
temp.range <- 4 # thermal breath of species

# habitats
adult_habitat <- expand_grid(x = 1:resolution[1], y = 1:resolution[2]) %>%
  mutate(
    habitat = 0.01,
    habitat = habitat / max(habitat) * adult_diffusion
  ) %>%
  pivot_wider(names_from = y, values_from = habitat) %>%
  select(-x) %>%
  as.matrix()

recruit_habitat <- expand_grid(x = 1:resolution[1], y = 1:resolution[2]) %>%
  mutate(
    habitat = 0.01,
    habitat = habitat / max(habitat) * recruit_diffusion
  ) %>%
  pivot_wider(names_from = y, values_from = habitat) %>%
  select(-x) %>%
  as.matrix()

adult_movement_matrix <- movement_matrix(time_step, resolution, adult_habitat)
recruit_movement_matrix <- movement_matrix(time_step, resolution, recruit_habitat)

# add MPA (no fishing redistribution right now)
f_mort[5, 5] <- 0
f_mort[5, 6] <- 0
f_mort[6, 5] <- 0
f_mort[6, 6] <- 0

# initialize grid
pop <-array(0, c(resolution, age_classes, sexes))
pop[, , 2, ] <- initial # add initial adults

output <- array(0, c(resolution, age_classes, sexes, years, reps))

for (rep in 1:reps) {
  print(rep)
  for (t in 1:years) {
    output[, , ,  , t, rep] <- pop
    pop[,,1,] = pop[, , 2, 1] * num_eggs
    pop[,,1,] = pop[,,1,] * (1-(n_mort / (1 + dd * pop[,,1,])))
    #add juveniles move 
    pop[,,2,] = pop[,,2,] + pop[,,1,] 
    pop[,,2,1] = pop[,,2,1] * (1-n_mort) * (1-f_mort)
    pop[,,2,2] = pop[,,2,2] * (1-n_mort) * (1-f_mort)
    #adults move
  }
  gc() # clear memory
}
gc()

end_time <- Sys.time()
end_time - start_time

# output array
save(output.array, file = here::here("model1.rda"))
