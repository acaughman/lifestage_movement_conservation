source(here::here("scripts", "funs", "movement_matrix.R"))

reps <- 1
resolution <- c(50, 50)
years <- 100 # eventually set up for pre fishing, pre MPA
seasons <- 1

time_step <- 1 / seasons

steps <- years * seasons

patch_area <- 1


adult_diffusion <- 32
recruit_diffusion <- 32
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

adult_mm <- movement_matrix(time_step, resolution, adult_habitat)

save(adult_mm, file = here::here("outputs", "adult_diffusion_32.rda"))

adult_movement_matrix <- array(0, dim = c(resolution[1], resolution[2], nrow(adult_mm)))

for (i in 1:nrow(adult_mm)) {
  adult_movement_matrix[, , i] <- Reshape(adult_mm[, i], resolution[1], resolution[2])
}

save(adult_movement_matrix, file = here::here("outputs", "adult_movement_matrix_32.rda"))

recruit_mm <- movement_matrix(time_step, resolution, recruit_habitat)

save(recruit_mm, file = here::here("outputs", "recruit_diffusion_32.rda"))

recruit_movement_matrix <- array(0, dim = c(resolution[1], resolution[2], nrow(recruit_mm)))

for (i in 1:nrow(recruit_mm)) {
  recruit_movement_matrix[, , i] <- Reshape(recruit_mm[, i], resolution[1], resolution[2])
}

save(recruit_movement_matrix, file = here::here("outputs", "recruit_movement_matrix_32.rda"))
