adult_habitat <- expand_grid(x = 1:resolution[1], y = 1:resolution[2]) %>%
  mutate(habitat =  .01 * (resolution[1] - x),
         habitat = habitat / max(habitat) * adult_diffusion) %>% 
  pivot_wider(names_from = y, values_from = habitat) %>% 
  select(-x) %>% 
  as.matrix()

recruit_habitat <- expand_grid(x = 1:resolution[1], y = 1:resolution[2]) %>%
  mutate(habitat =  .01 * x,
         habitat = habitat / max(habitat) * adult_diffusion) %>% 
  pivot_wider(names_from = y, values_from = habitat) %>% 
  select(-x) %>% 
  as.matrix()