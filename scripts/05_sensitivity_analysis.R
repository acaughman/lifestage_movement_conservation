library(tidyverse)
library(patchwork)

output <- read_csv(here::here("data", "processed_data", "sensitivity_results.csv"))
connect_full <- read_csv(here::here("data", "processed_data", "sensitivity_results_connectivity.csv"))

mpa <- output %>%
  filter(mpa != "Non-MPA") %>%
  group_by(mpa, mpa_size, mpa_spacing, larval, adult, generation, age, fp, eggs) %>%
  summarize(mean_pop = mean(pop, na.rm = TRUE)) %>%
  filter(mpa == "MPA 1") %>%
  mutate(adult_cat = case_when(
    adult %in% c(1, 2) ~ "low",
    adult %in% c(4, 8) ~ "medium",
    adult %in% c(16, 32) ~ "high"
  )) %>%
  mutate(larval_cat = case_when(
    larval %in% c(4, 2) ~ "low",
    larval %in% c(16, 8) ~ "medium",
    larval %in% c(190, 32) ~ "high"
  )) %>%
  mutate(
    movement = c(paste0(adult, " / ", larval)),
    move_cat = c(paste0(adult_cat, " / ", larval_cat)),
    move_ratio = adult / larval
  ) %>%
  mutate(ratio_cat = case_when(
    move_ratio < 1 ~ "Larval Greater",
    move_ratio == 1 ~ "Equal",
    move_ratio > 1 ~ "Adult Greater"
  )) %>% 
  mutate(mpa_spacing = as.factor(mpa_spacing)) %>% 
  mutate(eggs = as.factor(eggs),
         fp = as.factor(fp)) %>% 
  mutate(eggs = fct_relevel(eggs, c("low", "med", "high")),
         fp = fct_relevel(fp, c("low", "med", "high")))

rm(output)

mpa <- mpa %>%
  mutate(movement = fct_relevel(movement, c(
    "1 / 2", "2 / 2", "4 / 2", "8 / 2", "16 / 2", "32 / 2",
    "1 / 4", "1 / 8", "1 / 16", "1 / 32", "1 / 190",
    "2 / 4", "4 / 4", "8 / 4", "16 / 4", "32 / 4",
    "2 / 8", "2 / 16", "2 / 32", "2 / 190",
    "4 / 8", "8 / 8", "16 / 8", "32 / 8",
    "4 / 16", "4 / 32", "4 / 190",
    "8 / 16", "16 / 16", "32 / 16",
    "8 / 32", "8 / 190",
    "16 / 32", "32 / 32",
    "16 / 190", "32 / 190"
  )))

connect <- connect_full %>%
  mutate(
    adult_fished_c_1 = adult_RS_fished + adult_IS_fished,
    adult_mpa_c_1 = adult_RS_mpa + adult_IS_mpa,
    larvae_fished_c_1 = larvae_RS_fished + larvae_IS_fished,
    larvae_mpa_c_1 = larvae_RS_mpa + larvae_IS_mpa,
    adult_c_2 = adult_SR + adult_II,
    larvae_c_2 = larvae_SR + larvae_II,
    movement = c(paste0(adult, " / ", larval))
  ) %>%
  mutate(
    fished_c1 = adult_fished_c_1 + larvae_fished_c_1,
    mpa_c1 = adult_mpa_c_1 + larvae_mpa_c_1,
    c2 = adult_c_2 + larvae_c_2,
    fished_c3 = adult_ES_fished + larvae_ES_fished,
    mpa_c3 = adult_ES_mpa + larvae_ES_mpa,
    relative_mpa_c1 = adult_mpa_c_1 / larvae_mpa_c_1,
    relative_fished_c1 = adult_fished_c_1 / larvae_fished_c_1,
    relative_c2 = adult_c_2 / larvae_c_2,
    relative_mpa_R = adult_RS_mpa / larvae_RS_mpa,
    relative_fished_R = adult_RS_fished / larvae_RS_fished,
    relative_mpa_I = adult_IS_mpa / larvae_IS_mpa,
    relative_fished_I = adult_IS_fished / larvae_IS_fished,
    relative_mpa_E =  adult_ES_mpa / larvae_ES_mpa,
    relative_fished_E = adult_ES_fished / larvae_ES_fished
  ) %>%
  mutate(adult_cat = case_when(
    adult %in% c(1, 2) ~ "low",
    adult %in% c(4, 8) ~ "high",
    adult %in% c(16, 32) ~ "high"
  )) %>%
  mutate(larval_cat = case_when(
    larval %in% c(4, 2) ~ "low",
    larval %in% c(16, 8) ~ "high",
    larval %in% c(190, 32) ~ "high"
  )) %>%
  mutate(
    movement = c(paste0(adult, " / ", larval)),
    move_cat = c(paste0(adult_cat, " / ", larval_cat)),
    move_ratio = adult / larval
  ) %>%
  mutate(movement = fct_relevel(movement, c(
    "1 / 2", "2 / 2", "4 / 2", "8 / 2", "16 / 2", "32 / 2",
    "1 / 4", "1 / 8", "1 / 16", "1 / 32", "1 / 190",
    "2 / 4", "4 / 4", "8 / 4", "16 / 4", "32 / 4",
    "2 / 8", "2 / 16", "2 / 32", "2 / 190",
    "4 / 8", "8 / 8", "16 / 8", "32 / 8",
    "4 / 16", "4 / 32", "4 / 190",
    "8 / 16", "16 / 16", "32 / 16",
    "8 / 32", "8 / 190",
    "16 / 32", "32 / 32",
    "16 / 190", "32 / 190"
  ))) %>%
  mutate(ratio_cat = case_when(
    move_ratio < 1 ~ "Larval Greater",
    move_ratio == 1 ~ "Equal",
    move_ratio > 1 ~ "Adult Greater"
  )) %>% 
  mutate(mpa_spacing = as.factor(mpa_spacing)) %>% 
  mutate(eggs = as.factor(eggs),
         fp = as.factor(fp)) %>% 
  mutate(eggs = fct_relevel(eggs, c("low", "med", "high")),
         fp = fct_relevel(fp, c("low", "med", "high")))

mpa_connect <- connect %>%
  filter(adult %in% c(2, 8, 16)) %>% 
  filter(larval %in% c(2, 8, 16)) %>%
  mutate(movement = fct_relevel(movement, c(
    "8 / 2", "16 / 2", "16 / 8", 
    "2 / 2",  "8 / 8","16 / 16", 
    "2 / 8", "2 / 16", "8 / 16"
  ))) 

eq_pop_size = mpa %>% 
  filter(age == "adult") %>% 
  filter(adult %in% c(2, 8, 16)) %>% 
  filter(larval %in% c(2, 8, 16)) %>%
  mutate(movement = fct_relevel(movement, c(
    "8 / 2", "16 / 2", "16 / 8", 
    "2 / 2",  "8 / 8","16 / 16", 
    "2 / 8", "2 / 16", "8 / 16"
  ))) %>% 
  filter(mpa == "MPA 1") %>% 
  filter(generation == 90)

# Biomass Figs ------------------------------------------------------------

ggplot(eq_pop_size) +
  geom_point(aes(movement, mean_pop, color = mpa_spacing)) +
  facet_grid(fp~eggs, labeller = label_both, scales = "free_y") +
  theme_bw() +
  theme(strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "white")) +
  labs(x = "Relative Movement",
       y = "Population",
       color = "MPA Spacing") +
  scale_color_viridis_d(end = 0.9)

p1 = ggplot(eq_pop_size %>% filter(mpa_spacing == 0)) +
  geom_point(aes(eggs, mean_pop, color = movement)) +
  facet_wrap(~fp) +
  theme_bw() +
  theme(strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "white")) +
  labs(x = "Eggs Produced",
       y = "Population",
       color = "Movement \n (adult / larvae)") +
  scale_color_viridis_d(end = 0.9)
p1

ggsave(p1, path = here::here("figs"), file = paste0("fig3.pdf"), height = 6, width = 10, limitsize = FALSE)

# Connect Figs ------------------------------------------------------------

p1 <- ggplot(mpa_connect) +
  geom_hline(aes(yintercept = 1), linetype = "dashed", linewidth = 1) +
  geom_point(aes(movement, relative_mpa_R, color = "Retention", shape = mpa_spacing), size = 3) +
  # geom_point(aes(movement, relative_mpa_I, color = "Import"), size = 5) +
  theme_bw(base_size = 20) +
  facet_grid(fp~eggs, labeller = label_both) +
  labs(
    x = "Relative Movement",
    y = "Relative Contribution",
    color = "",
    shape = "MPA Spacing"
  ) +
  scale_color_manual(values = c("Retention" = "green", "Import" = "blue", "Export" = "purple")) +
  theme(strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_y_log10() 

p2 <- ggplot(mpa_connect) +
  geom_hline(aes(yintercept = 1), linetype = "dashed", linewidth = 1) +
  geom_point(aes(movement, relative_mpa_I, color = "Import", shape = mpa_spacing), size = 3) +
  theme_bw(base_size = 20) +
  facet_grid(fp~eggs, labeller = label_both) +
  labs(
    x = "Relative Movement",
    y = "Relative Contribution",
    color = "",
    shape = "MPA Spacing"
  ) +
  scale_color_manual(values = c("Retention" = "green", "Import" = "blue", "Export" = "purple")) +
  theme(strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_y_log10() 

p3 <- ggplot(mpa_connect) +
  geom_hline(aes(yintercept = 1), linetype = "dashed", linewidth = 1) +
  geom_point(aes(movement, relative_mpa_E, color = "Export", shape = mpa_spacing), size = 3) +
  theme_bw(base_size = 20) +
  facet_grid(fp~eggs, labeller = label_both) +
  labs(
    x = "Relative Movement",
    y = "Relative Contribution",
    color = "",
    shape = "MPA Spacing"
  ) +
  scale_color_manual(values = c("Retention" = "green", "Import" = "blue", "Export" = "purple")) +
  theme(strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_y_log10()

plot <- p1 + p2 + p3 + plot_annotation(tag_levels = "A") + plot_layout(guides ="collect")

ggsave(plot, path = here::here("figs"), file = paste0("fig4.pdf"), height = 6, width = 25, limitsize = FALSE)

# OLD ---------------------------------------------------------------------

# 
# p1 <- ggplot(mpa_connect) +
#   geom_hline(aes(yintercept = 1), color = "red", linetype = "dashed", linewidth = 1) +
#   geom_point(aes(adult, relative_c2, color = as.factor(larval)), size = 5) +
#   theme_bw(base_size = 20) +
#   scale_color_viridis_d(end = 0.9) +
#   labs(
#     x = "Adult Movement",
#     y = "Relative Adult / Larval Connectivity",
#     color = "Larval Movement",
#     linetype = "Larval Movement",
#     title = "Theoretical Connectivity"
#   ) +
#   geom_smooth(aes(adult, relative_c2, linetype = as.factor(larval)), se = FALSE, alpha = 0.3, color = "black", linewidth = 1) +
#   ylim(c(0.2, 4.5))
# 
# ggsave(p1, path = here::here("figs", "pres"), file = paste0("theoretical.pdf"), height = 8, width = 8, limitsize = FALSE)
# 
# p2 <- ggplot(mpa_connect) +
#   # geom_abline(aes()) +
#   geom_hline(aes(yintercept = 1), color = "red", linetype = "dashed", linewidth = 1) +
#   geom_point(aes(adult, relative_mpa_c1, color = as.factor(larval), shape = mpa_spacing), size = 5) +
#   theme_bw(base_size = 20) +
#   scale_color_viridis_d(end = 0.9) +
#   facet_grid(fp~eggs, labeller = label_both, scales = "free_y") +
#   labs(
#     x = "Adult Movement",
#     y = "",
#     color = "Larval Movement",
#     linetype = "Larval Movement",
#     title = "Realized Connectivity",
#     shape = "MPA Spacing"
#   ) +
#   geom_smooth(aes(adult, relative_mpa_c1, linetype = as.factor(larval)), se = FALSE, alpha = 0.3, color = "black", linewidth = 1) +
#   theme(strip.text = element_text(face = "bold"),
#         strip.background = element_rect(fill = "white"))
# # ylim(c(0.2, 4.5))
# 
# plot <- p1 + p2 + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect")
# 
# # ggsave(plot, path = here::here("figs", "pres"), file = paste0("theoretical_v_realized.pdf"), height = 8, width = 12, limitsize = FALSE)
# 
# p3 <- ggplot(mpa_connect) +
#   # geom_abline(aes()) +
#   geom_hline(aes(yintercept = 1), color = "red", linetype = "dashed", linewidth = 1) +
#   geom_point(aes(adult, relative_fished_c1, color = as.factor(larval), shape = mpa_spacing), size = 5) +
#   theme_bw(base_size = 20) +
#   scale_color_viridis_d(end = 0.9) +
#   facet_grid(fp~eggs, labeller = label_both, scales = "free_y") +
#   labs(
#     x = "Adult Movement",
#     y = "",
#     color = "Larval Movement",
#     linetype = "Larval Movement",
#     title = "Realized Connectivity \n without MPA",
#     shape = "MPA spacing"
#   ) +
#   geom_smooth(aes(adult, relative_fished_c1, linetype = as.factor(larval)), se = FALSE, alpha = 0.3, color = "black", linewidth = 1) +
#   theme(strip.text = element_text(face = "bold"),
#         strip.background = element_rect(fill = "white"))
# 
# plot <- p1 + p3 + p2 + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect")
# 
# ggsave(plot, path = here::here("figs", "pres"), file = paste0("theoretical_v_realized2.pdf"), height = 8, width = 16, limitsize = FALSE)
# 


# p1 <- ggplot(mpa_connect) +
#   geom_hline(aes(yintercept = 1), linetype = "dashed", linewidth = 1) +
#   geom_point(aes(movement, relative_mpa_c1, color = mpa_spacing), size = 5) +
#   # geom_point(aes(movement, relative_mpa_I, color = "Import"), size = 5) +
#   theme_bw(base_size = 20) +
#   facet_grid(fp~eggs, labeller = label_both) +
#   labs(
#     x = "Relative Movement",
#     y = "Relative Contribution Adult vs. Larvae",
#     color = "",
#     shape = "MPA Spacing"
#   ) +
#   theme(strip.text = element_text(face = "bold"),
#         strip.background = element_rect(fill = "white"),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
#   scale_y_log10() +
#   scale_color_viridis_d(end = 0.9)

