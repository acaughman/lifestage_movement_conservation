library(tidyverse)
library(patchwork)

# output_df1 = read_csv(here::here("outputs", "base_model_2x2.csv"))
# output_df2 = read_csv(here::here("outputs", "base_model_4x4.csv"))
# output_df3 = read_csv(here::here("outputs", "base_model_8x8.csv"))
# output_df4 = read_csv(here::here("outputs", "base_model_16x16.csv"))

# output_df1 = read_csv(here::here("outputs", "base_model_4x4_2.csv"))
# output_df2 = read_csv(here::here("outputs", "base_model_4x4_4.csv"))
# output_df3 = read_csv(here::here("outputs", "base_model_4x4_8.csv"))
# output_df4 = read_csv(here::here("outputs", "base_model_4x4_16.csv"))

# output_df1 = read_csv(here::here("outputs", "base_model_8x8_2.csv"))
# output_df2 = read_csv(here::here("outputs", "base_model_8x8_4.csv"))
# output_df3 = read_csv(here::here("outputs", "base_model_8x8_8.csv"))
output_df4 = read_csv(here::here("outputs", "base_model_8x8_16.csv"))

output_df1 = read_csv(here::here("outputs", "base_model_8x8_16_movetest.csv"))

# summary -----------------------------------------------------------------

# f_mort[25:26, 25:26, ] <- 1 # size 2x2
# f_mort[24:27, 24:27, ] <- 1 # size 4x4
# f_mort[22:29, 22:29, ] <- 1 # size 8x8
# f_mort[17:32, 17:32, ] <- 1 # size 16x16

p1 = ggplot(output_df1 %>% filter(generation == 100) %>% filter(age == "adult")) +
  geom_tile(aes(lon, lat, fill = pop, color = pop)) +
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  # geom_rect(xmin = 25, ymin = 25, xmax=26, ymax=26, fill = NA, color= "red") +
  theme_bw()
p2 = ggplot(output_df2 %>% filter(generation == 100) %>% filter(age == "adult")) +
  geom_tile(aes(lon, lat, fill = pop, color = pop)) +
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  # geom_rect(xmin = 24, ymin = 24, xmax=27, ymax=27, fill = NA, color= "red") +
  theme_bw()
p3 = ggplot(output_df3 %>% filter(generation == 100) %>% filter(age == "adult")) +
  geom_tile(aes(lon, lat, fill = pop, color = pop)) +
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  # geom_rect(xmin = 22, ymin = 22, xmax=29, ymax=29, fill = NA, color= "red") +
  theme_bw()
p4 = ggplot(output_df4 %>% filter(generation == 100) %>% filter(age == "adult")) +
  geom_tile(aes(lon, lat, fill = pop, color = pop)) +
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  # geom_rect(xmin = 17, ymin = 17, xmax=32, ymax=32, fill = NA, color= "red") +
  theme_bw()

# (p1 + p2) / (p3 + p4)

p1  + p4

# Other Plots -------------------------------------------------------------

mpa_df = output_df2 %>% 
  filter(lat %in% c(24, 25,26, 27))%>% 
  filter(lon %in% c(24, 25,26, 27)) %>% 
  group_by(rep, age, generation) %>% 
  summarize(pop = sum(pop))

ggplot(mpa_df) +
  geom_line(aes(generation, pop, color = age, group = age)) +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x= element_blank())

overall_df = output_df4%>% 
  group_by(rep, age, generation) %>% 
  summarize(pop = sum(pop))

ggplot(overall_df) +
  geom_line(aes(generation, pop, color = age, group = age)) +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x= element_blank())

