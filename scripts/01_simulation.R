source(here::here("scripts", "funs", "movement_matrix.R"))

addTaskCallback(function(...) {
  set.seed(42)
  TRUE
})
options(warn = -1)
options(dplyr.summarise.inform = FALSE)

# simulation variables
reps <- 1
resolution <- c(10, 10)
years <- 100 # eventually set up for pre fishing, pre MPA
seasons <- 1

time_step <- 1 / seasons

steps <- years * seasons

patch_area <- 1

age_classes <- 2 # babies, adult
sexes <- 2 # female, male

# fish variables
adult_diffusion <- 1.5 # km2/year
recruit_diffusion <- 3 # km2/year
num_eggs <- 3 # number of eggs per female fish
dd <- 0.002 # density dependence for larval mortality
n_mort <- 1 - 0.3 # natural mortality
f_mort <- 1 - array(0.5, c(resolution, age_classes)) # fishing mortality (same dimension as simulation)
opt.temp <- 25 # optimal temperature of species
temp.range <- 4 # thermal breath of species

initial <- 100

max_hab_mult <- 2

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
f_mort[5, 5, ] <- 1
f_mort[5, 6, ] <- 1
f_mort[6, 5, ] <- 1
f_mort[6, 6, ] <- 1

pop <- array(0, c(resolution, age_classes, sexes)) # initialize grid
pop[, , 2, ] <- initial # add initial adults

output <- array(0, c(resolution, age_classes, sexes, years, reps)) # create array to hold outputs

start_time <- Sys.time()

for (rep in 1:reps) {
  print(rep)
  for (t in 1:years) {
    output[, , , , t, rep] <- pop
    # births
    pop[, , 1, ] <- pop[, , 2, 1] * num_eggs / 2
    # larvae move
    pop[, , 1, ] = rowSums(recruit_movement_matrix * as.vector(Reshape(pop[, , 1, ], 100)), dims = 3)
    # natural mortality
    pop[, , 1, ] <- pop[, , 1, ] * array((n_mort / (1 + dd * rowSums(pop[, , 1, ], dim = 2))), c(resolution, 2))
    pop[, , 2, ] <- pop[, , 2, ] *  n_mort
    # fishing mortality
    pop[, , 2, ] <- pop[, , 2, ] *  f_mort
    # adult move
    pop[, , 2, ] = rowSums(adult_movement_matrix * as.vector(Reshape(pop[, , 2, ], 100)), dims = 3)
    # larvae become adults
    pop[, , 2, ] = pop[, , 1, ] + pop[, , 2, ]
    
    output[, , 1 , , t, rep] <- pop[, , 1, ]
    pop[, , 1, ] = 0
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

names(output_df) = c(1:resolution[1], "rep", "generation", "sex", "age", "lat")

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
  group_by(lat,lon,rep,age,generation) %>% 
  summarize(pop = sum(pop))

write_csv(output_df, here::here("outputs", "base_model"))

# summary -----------------------------------------------------------------

mpa_df = output_df %>% 
  filter(lat %in% c(5,6))%>% 
  filter(lon %in% c(5,6)) %>% 
  group_by(rep, age, generation) %>% 
  summarize(pop = sum(pop))

overall_df = output_df%>% 
  group_by(rep, age, generation) %>% 
  summarize(pop = sum(pop))

ggplot(overall_df) +
  geom_line(aes(generation, pop, color = age, group = age)) +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x= element_blank())
