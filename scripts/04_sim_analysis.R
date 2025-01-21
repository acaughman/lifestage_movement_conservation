library(tidyverse)
library(patchwork)
library(sensitivity)
library(sensobol)

# Data Input --------------------------------------------------------------

output <- read_csv(here::here("data", "processed_data", "model_results.csv"))
connect_full <- read_csv(here::here("data", "processed_data", "connectivity_results.csv"))

mpa <- output %>%
  group_by(mpa, mpa_size, mpa_spacing, larval, adult, generation, age, fp, eggs) %>%
  summarize(mean_pop = mean(pop, na.rm = TRUE)) %>%
  filter(mpa != "MPA 2") 

rm(output)

mpa <- mpa %>% 
  mutate(adult_cat = case_when(
    adult %in% c(2) ~ "low",
    adult %in% c(8) ~ "medium",
    adult %in% c(16) ~ "high"
  )) %>%
  mutate(larval_cat = case_when(
    larval %in% c(4) ~ "low",
    larval %in% c(8) ~ "medium",
    larval %in% c(32) ~ "high"
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
         fp = fct_relevel(fp, c("low", "med", "high"))) %>%
  mutate(movement = fct_relevel(movement, c(
    "2 / 4",  "2 / 8", "2 / 32", 
    "8 / 4", "8 / 8", "8 / 32",
    "16 / 4", "16 / 8", "16 / 32"
    ))) %>% 
  pivot_wider(names_from = mpa, values_from = mean_pop) 

mpa <- mpa %>% 
  janitor::clean_names() %>% 
  mutate(in_out = log(mpa_1 / reference))

connect <- connect_full %>%
  mutate(
    adult_fished_abs = adult_RS_fished + adult_IS_fished,
    adult_mpa_abs = adult_RS_mpa + adult_IS_mpa,
    larvae_fished_abs = larvae_RS_fished + larvae_IS_fished,
    larvae_mpa_abs = larvae_RS_mpa + larvae_IS_mpa,
    adult_rel = adult_SR + adult_II,
    larvae_rel = larvae_SR + larvae_II,
    fished_abs = adult_fished_abs + larvae_fished_abs,
    mpa_abs = adult_mpa_abs + larvae_mpa_abs,
    rel = adult_rel + larvae_rel,
    fished_export = adult_ES_fished + larvae_ES_fished,
    mpa_export = adult_ES_mpa + larvae_ES_mpa,
    fished_import = adult_IS_fished + larvae_IS_fished,
    mpa_import = adult_IS_mpa + larvae_IS_mpa,
    fished_ret = adult_RS_fished + larvae_RS_fished,
    mpa_ret = adult_RS_mpa + larvae_RS_mpa,
    movement = c(paste0(adult, " / ", larval))
  ) %>%
  mutate(
    relative_mpa_abs = adult_mpa_abs / larvae_mpa_abs,
    relative_fished_abs = adult_fished_abs / larvae_fished_abs,
    relative_rel = adult_rel / larvae_rel,
    relative_mpa_R = adult_RS_mpa / larvae_RS_mpa,
    relative_fished_R = adult_RS_fished / larvae_RS_fished,
    relative_mpa_I = adult_IS_mpa / larvae_IS_mpa,
    relative_fished_I = adult_IS_fished / larvae_IS_fished,
    relative_mpa_E =  adult_ES_mpa / larvae_ES_mpa,
    relative_fished_E = adult_ES_fished / larvae_ES_fished,
    adult_relative_mpa = adult_RS_mpa / adult_IS_mpa,
    adult_relative_fished = adult_RS_fished / adult_IS_fished,
    larvae_relative_mpa = larvae_RS_mpa / larvae_IS_mpa,
    larvae_relative_fished = larvae_RS_fished / larvae_IS_fished,
    relative_ret_imp = mpa_ret / mpa_import
  ) %>%
  mutate(adult_cat = case_when(
    adult %in% c(2) ~ "low",
    adult %in% c(8) ~ "medium",
    adult %in% c(16) ~ "high"
  )) %>%
  mutate(larval_cat = case_when(
    larval %in% c(4) ~ "low",
    larval %in% c(8) ~ "medium",
    larval %in% c(32) ~ "high"
  )) %>%
  mutate(
    movement = c(paste0(adult, " / ", larval)),
    move_cat = c(paste0(adult_cat, " / ", larval_cat)),
    move_ratio = adult / larval
  ) %>%
  mutate(movement = fct_relevel(movement, c(
    "2 / 4",  "2 / 8", "2 / 32", 
    "8 / 4", "8 / 8", "8 / 32",
    "16 / 4", "16 / 8", "16 / 32"
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

eq_pop_size = mpa %>% 
  filter(age == "adult") %>% 
  filter(generation == 90)

colors = c("Retention" = "#5ec962", "Import" = "#3b528b", "Export" = "#440154", "Retention + Import" = "#21918c")

# Sobol Sensitivity --------------------------------------------------------

# sobol_data = mpa %>% 
#   ungroup() %>% 
#   filter(generation == 90) %>% 
#   filter(mpa == "MPA 1") %>% 
#   mutate(movement = c(paste0(adult, " / ", larval))) %>% 
#   select(eggs, fp, adult, larval, movement, mpa_size, mpa_spacing, mean_pop)
# 
# input_data = sobol_data %>% 
#   select(-mean_pop)
# output_data = sobol_data %>% 
#   select(mean_pop) %>% 
#   pull()
# 
# x = as.matrix(sobol_data)
# 
# 
# sobol_indices <- sobol(model = NULL,
#                        X1 = ?,
#                        X2 = ?,
#                        order = 1, nboot = 100)
# 
# sobol_summary <- data.frame(Parameter = rownames(sobol_indices$S),
#                             First_Order = sobol_indices$S[,1],
#                             Total_Order = sobol_indices$ST)

# Theoretical vs Realized Connectivity ------------------------------------

sub_connect = connect %>% 
  filter(mpa_size %in% c(4, 8)) %>% 
  filter(mpa_spacing == 8) %>% 
  filter(eggs == "low") %>% 
  filter(fp == "high")

p1 = ggplot(sub_connect) +
  geom_hline(aes(yintercept = 1), color = "red", alpha = 0.5, linetype = "dashed") +
  geom_point(aes(adult, relative_mpa_abs, color = as.factor(larval), shape = as.factor(mpa_size)), size = 3) +
  geom_line(aes(adult, relative_mpa_abs, color = as.factor(larval), linetype = as.factor(mpa_size)), linewidth = 1) +
  theme_bw(base_size = 16) +
  scale_color_viridis_d(end = 0.9) +
  labs(x = "Adult Movement",
       y = "Relative Number of Settlers (Adult / Larval)",
       color = "Larval Movement",
       shape = "MPA Size",
       linetype = "MPA Size") 

p2 = ggplot(sub_connect) +
  geom_hline(aes(yintercept = 1), color = "red", alpha = 0.5, linetype = "dashed") +
  geom_point(aes(adult, relative_ret_imp, color = as.factor(larval), shape = as.factor(mpa_size)), size = 3) +
  geom_line(aes(adult, relative_ret_imp, color = as.factor(larval), linetype = as.factor(mpa_size)), linewidth = 1) +
  theme_bw(base_size = 16) +
  scale_color_viridis_d(end = 0.9) +
  labs(x = "Adult Movement",
       y = "Relative Number of Settlers (Retention / Import)",
       color = "Larval Movement",
       shape = "MPA Size",
       linetype = "MPA Size") 

p = p1 + p2 + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect")

ggsave(p, path = here::here("figs"), file = paste0("fig1.pdf"), height = 8, width = 15)
  
p3 = ggplot(sub_connect) +
  geom_hline(aes(yintercept = 1), color = "red", alpha = 0.5, linetype = "dashed") +
  geom_point(aes(adult, relative_rel, color = as.factor(larval), shape = as.factor(mpa_size)), size = 3) +
  geom_line(aes(adult, relative_rel, color = as.factor(larval), linetype = as.factor(mpa_size)), linewidth = 1) +
  theme_bw(base_size = 16) +
  scale_color_viridis_d(end = 0.9) +
  labs(x = "Adult Movement",
       y = "Number of Settlers (Adult / Larval)",
       color = "Larval Movement",
       shape = "MPA Size",
       linetype = "MPA Size") +
  ylim(c(0.2, 8.5))

p4 = ggplot(sub_connect) +
  geom_hline(aes(yintercept = 1), color = "red", alpha = 0.5, linetype = "dashed") +
  geom_point(aes(adult, relative_fished_abs, color = as.factor(larval), shape = as.factor(mpa_size)), size = 3) +
  geom_line(aes(adult, relative_fished_abs, color = as.factor(larval), linetype = as.factor(mpa_size)), linewidth = 1) +
  theme_bw(base_size = 16) +
  scale_color_viridis_d(end = 0.9) +
  labs(x = "Adult Movement",
       y = "",
       color = "Larval Movement",
       shape = "MPA Size",
       linetype = "MPA Size") +
  ylim(c(0.2, 8.5))

p5 = ggplot(sub_connect) +
  geom_hline(aes(yintercept = 1), color = "red", alpha = 0.5, linetype = "dashed") +
  geom_point(aes(adult, relative_mpa_abs, color = as.factor(larval), shape = as.factor(mpa_size)), size = 3) +
  geom_line(aes(adult, relative_mpa_abs, color = as.factor(larval), linetype = as.factor(mpa_size)), linewidth = 1) +
  theme_bw(base_size = 16) +
  scale_color_viridis_d(end = 0.9) +
  labs(x = "Adult Movement",
       y = "",
       color = "Larval Movement",
       shape = "MPA Size",
       linetype = "MPA Size") +
  ylim(c(0.2, 8.5))

# May Drop the Size bit 
p = p3 + p4 + p5 + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect")

ggsave(p, path = here::here("figs"), file = paste0("figS1.pdf"), height = 8, width = 20)

# Connectivity ---------------------------------------

sub_connect = connect %>% 
  filter(mpa_size == 8) %>% 
  filter(mpa_spacing == 8) %>% 
  filter(eggs == "med") %>% 
  filter(fp == "med") %>% 
  mutate(movement = fct_reorder(movement, relative_mpa_abs, .desc = TRUE))

p1 = ggplot(sub_connect) +
  geom_hline(aes(yintercept = 1), color = "red", alpha = 0.5, linetype = "dashed") +
  # geom_rect(aes(xmin = 0.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  # geom_rect(aes(xmin = 4.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  # geom_rect(aes(xmin = 6.5, xmax = 9.5, ymin = 0, ymax = 16, color = "Retention"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  geom_point(aes(movement, relative_mpa_abs, color = "Retention + Import"), size = 3) +
  # geom_point(aes(movement, relative_mpa_R, color = "Retention"), size = 3) +
  # geom_point(aes(movement, relative_mpa_I, color = "Import"), size = 3) +
  # geom_point(aes(movement, relative_mpa_E, color = "Export"), size = 3) +
  theme_bw(base_size = 16) +
  # facet_wrap(~fp) +
  # theme(strip.text = element_text(face = "bold"),
  #       strip.background = element_rect(fill = "white"),
  #       axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "Movement (Adult / Larval)", 
       y = "Relative Number of Settlers (Adult / Larval)",
       color = "") +
  # ylim(c(NA, 11)) +
  scale_color_manual(values = colors)

p2 = ggplot(sub_connect) +
  geom_hline(aes(yintercept = 1), color = "red", alpha = 0.5, linetype = "dashed") +
  # geom_rect(aes(xmin = 0.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  # geom_rect(aes(xmin = 4.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  # geom_rect(aes(xmin = 6.5, xmax = 9.5, ymin = 0, ymax = 16, color = "Retention"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  # geom_point(aes(fct_reorder(movement, relative_mpa_abs, .desc = TRUE), relative_mpa_abs, color = "Retention + Import"), size = 3.5) +
  geom_point(aes(movement, relative_mpa_R, color = "Retention"), size = 3) +
  # geom_point(aes(movement, relative_mpa_I, color = "Import"), size = 3) +
  # geom_point(aes(movement, relative_mpa_E, color = "Export"), size = 3) +
  theme_bw(base_size = 16) +
  # facet_wrap(~fp) +
  # theme(strip.text = element_text(face = "bold"),
  #       strip.background = element_rect(fill = "white"),
  #       axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "Movement (Adult / Larval)", 
       y = "Relative Contribution to Retention (Adult / Larval)",
       color = "") +
  # ylim(c(NA, 11)) +
  scale_color_manual(values = colors)

p3 = ggplot(sub_connect) +
  geom_hline(aes(yintercept = 1), color = "red", alpha = 0.5, linetype = "dashed") +
  # geom_rect(aes(xmin = 0.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  # geom_rect(aes(xmin = 4.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  # geom_rect(aes(xmin = 6.5, xmax = 9.5, ymin = 0, ymax = 16, color = "Retention"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  # geom_point(aes(fct_reorder(movement, relative_mpa_abs, .desc = TRUE), relative_mpa_abs, color = "Retention + Import"), size = 3.5) +
  # geom_point(aes(movement, relative_mpa_R, color = "Retention"), size = 3) +
  geom_point(aes(movement, relative_mpa_I, color = "Import"), size = 3) +
  # geom_point(aes(movement, relative_mpa_E, color = "Export"), size = 3) +
  theme_bw(base_size = 16) +
  # facet_wrap(~fp) +
  # theme(strip.text = element_text(face = "bold"),
  #       strip.background = element_rect(fill = "white"),
  #       axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "Movement (Adult / Larval)", 
       y = "Relative Contribution to Import (Adult / Larval)",
       color = "") +
  # ylim(c(NA, 11)) +
  scale_color_manual(values = colors)

p4 = ggplot(sub_connect) +
  geom_hline(aes(yintercept = 1), color = "red", alpha = 0.5, linetype = "dashed") +
  # geom_rect(aes(xmin = 0.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  # geom_rect(aes(xmin = 4.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  # geom_rect(aes(xmin = 6.5, xmax = 9.5, ymin = 0, ymax = 16, color = "Retention"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  # geom_point(aes(fct_reorder(movement, relative_mpa_abs, .desc = TRUE), relative_mpa_abs, color = "Retention + Import"), size = 3.5) +
  # geom_point(aes(movement, relative_mpa_R, color = "Retention"), size = 3) +
  # geom_point(aes(movement, relative_mpa_I, color = "Import"), size = 3) +
  geom_point(aes(movement, relative_mpa_E, color = "Export"), size = 3) +
  theme_bw(base_size = 16) +
  # facet_wrap(~fp) +
  # theme(strip.text = element_text(face = "bold"),
  #       strip.background = element_rect(fill = "white"),
  #       axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "Movement (Adult / Larval)", 
       y = "Relative Contribution to Export (Adult / Larval)",
       color = "") +
  # ylim(c(NA, 11)) +
  scale_color_manual(values = colors)

p = (p2 + p3 + p4) + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect")

ggsave(p, path = here::here("figs"), file = paste0("fig2.pdf"), height = 6, width = 20)

# Fishing Pressure and Connectivity ---------------------------------------

sub_connect = connect %>% 
  filter(mpa_size == 8) %>% 
  filter(mpa_spacing == 8) %>% 
  filter(eggs == "med") %>% 
  filter(move_cat %in% c("high / low",  "high / medium", "medium / low")) %>% 
  mutate(movement = fct_relevel(movement, c("8 / 4", "16 / 8", "16 / 4"))) %>% 
  mutate(fp = case_when(
    fp == "med" ~ "medium",
    TRUE ~ fp
  )) %>% 
  mutate(fp = fct_relevel(fp, c("low", "medium", "high")))

p1 = ggplot(sub_connect) +
  geom_hline(aes(yintercept = 1), color = "red", alpha = 0.5, linetype = "dashed") +
  geom_point(aes(fct_reorder(movement, relative_mpa_R, .desc = TRUE), relative_mpa_R, color = "Retention"), size = 3) +
  geom_point(aes(movement, relative_mpa_I, color = "Import"), size = 3) +
  geom_point(aes(movement, relative_mpa_E, color = "Export"), size = 3) +
  theme_bw(base_size = 16) +
  labs(x = "Movement (Adult / Larval)", 
       y = "Number of Settlers (Adult / Larval)",
       color = "") +
  facet_wrap(~fp,ncol = 3) +
  scale_color_manual(values = colors) +
  ylim(c(NA, 16)) +
  theme(strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave(p1, path = here::here("figs"), file = paste0("fig3.pdf"), height = 6, width = 10)

# MPA Design --------------------------------------------------------------

sub_connect = connect %>% 
  filter(mpa_size %in% c(8)) %>% 
  filter(eggs == "low") %>% 
  filter(fp == "high")

p1 = ggplot(sub_connect) + 
  geom_hline(aes(yintercept = 1), color = "red", alpha = 0.5, linetype = "dashed") +
  geom_point(aes(mpa_spacing, relative_mpa_abs, color = movement), size = 3) +
  geom_line(aes(mpa_spacing, relative_mpa_abs, color = movement, group = as.factor(movement))) +
  theme_bw(base_size = 16) +
  # facet_wrap(~mpa_size)
  labs(x = "MPA Spacing",
       y = "Relative Number of Settlers (Adult / Larval)",
       color = "Movement (Adult / Larval)") +
  scale_color_viridis_d(end = 0.9) 

sub_connect = connect %>% 
  filter(mpa_spacing %in% c(0)) %>% 
  filter(eggs == "low") %>% 
  filter(fp == "high") 

p2 = ggplot(sub_connect) + 
  geom_hline(aes(yintercept = 1), color = "red", alpha = 0.5, linetype = "dashed") +
  geom_point(aes(mpa_size, relative_mpa_abs, color = movement), size = 3) +
  geom_line(aes(mpa_size, relative_mpa_abs, color = movement, group = as.factor(movement))) +
  theme_bw(base_size = 16) +
  # facet_wrap(~mpa_size)
  labs(x = "MPA Size",
       y = "Relative Number of Settlers (Adult / Larval)",
       color = "Movement (Adult / Larval)") +
  scale_color_viridis_d(end = 0.9) 

plot = p1 / p2 + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect")

ggsave(plot, path = here::here("figs"), file = paste0("fig4.pdf"), height = 12, width = 10)

eq_pop_size_sub = eq_pop_size %>% 
  filter(mpa_size %in% c(8)) %>% 
  filter(fp == "high") %>% 
  filter(eggs == "low")

p3 = ggplot(eq_pop_size_sub) +
  geom_hline(aes(yintercept = 0), color = "red", alpha = 0.5, linetype = "dashed") +
  # geom_hline(aes(yintercept = 0.7), color = "red", alpha = 0.5, linetype = "dashed") +
  geom_point(aes(mpa_spacing, in_out, color = movement), size = 3) +
  geom_line(aes(mpa_spacing, in_out, color = movement, group = movement)) +
  # facet_wrap(~mpa_size) +
  theme_bw(base_size = 16) +
  theme(strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "white")) +
  labs(x = "MPA Spacing",
       y = "Log(Inside MPA Population / Outside MPA Population)",
       color = "Movement (Adult / Larval)",
       shape = "MPA Size",
       linetype = "MPA Size") +
  scale_color_viridis_d(end = 0.9)

eq_pop_size_sub_a = eq_pop_size %>% 
  filter(mpa_size == 8) %>% 
  filter(mpa_spacing == 8) %>% 
  filter(fp == "high") %>% 
  filter(eggs == "low") %>% 
  group_by(adult) %>% 
  summarise(mean_adult = mean(in_out))

eq_pop_size_sub_l= eq_pop_size %>% 
  filter(mpa_size == 8) %>% 
  filter(mpa_spacing == 8) %>% 
  filter(fp == "high") %>% 
  filter(eggs == "low") %>% 
  group_by(larval) %>% 
  summarise(mean_larval = mean(in_out))

p4 = ggplot() +
  geom_point(data = eq_pop_size_sub_a, aes(adult, mean_adult, color = "Adult"), size = 3) +
  geom_point(data = eq_pop_size_sub_l, aes(larval, mean_larval, color = "Larval"), size = 3) +
  geom_line(data = eq_pop_size_sub_a, aes(adult, mean_adult, color = "Adult")) +
  geom_line(data = eq_pop_size_sub_l, aes(larval, mean_larval, color = "Larval")) +
  theme_bw(base_size = 16) +
  labs(x = "Movement Extent",
       y = "Log(Inside MPA Population / Outside MPA Population)",
       color = "") +
  scale_color_manual(values = c("Adult" = "#5ec962", "Larval" =  "#440154"))

plot = (p3 / p4) + plot_annotation(tag_levels = "A")

ggsave(plot, path = here::here("figs"), file = paste0("fig5.pdf"), height = 12, width = 8)

# In/0ut across eggs -------------------------------------------------------

eq_pop_size_sub = eq_pop_size %>% 
  filter(mpa_size == 8) %>% 
  filter(fp == "high") %>% 
  mutate(eggs = case_when(
    eggs == "med" ~ "medium",
    TRUE ~ eggs
  )) %>% 
  mutate(eggs = fct_relevel(eggs, c("low", "medium", "high")))

p1 = ggplot(eq_pop_size_sub) +
  geom_hline(aes(yintercept = 0), color = "red", alpha = 0.5, linetype = "dashed") +
  # geom_hline(aes(yintercept = 0.7), color = "red", alpha = 0.5, linetype = "dashed") +
  geom_point(aes(mpa_spacing, in_out, color = movement), size = 2) +
  geom_line(aes(mpa_spacing, in_out, color = movement, group = movement)) +
  facet_wrap(~eggs) +
  theme_bw(base_size = 16) +
  theme(strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "white")) +
  labs(x = "MPA Spacing",
       y = "Log(Inside MPA Population / Outside MPA Population)",
       color = "Movement (Adult / Larval)",
       shape = "Movement (Adult / Larval)") +
  scale_color_viridis_d(end = 0.9) +
  scale_shape_manual(values = c(19, 1, 1, 19, 19, 1, 19, 19, 19))

ggsave(p1, path = here::here("figs"), file = paste0("figS14.pdf"), height = 6, width = 8)

# Connectivity Across MPA sizes -------------------------------------------

sub_connect = connect %>% 
  filter(eggs == "med") %>% 
  filter(mpa_size == 2) %>% 
  filter(mpa_spacing == 0) %>% 
  mutate(fp = case_when(
    fp == "med" ~ "medium",
    TRUE ~ fp
  )) %>% 
  mutate(fp = fct_relevel(fp, c("low", "medium", "high")))

p1 = ggplot(sub_connect) +
  geom_hline(aes(yintercept = 1), color = "red", alpha = 0.5, linetype = "dashed") +
  # geom_rect(aes(xmin = 0.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +  
  # geom_rect(aes(xmin = 4.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  # geom_rect(aes(xmin = 6.5, xmax = 9.5, ymin = 0, ymax = 16, color = "Retention"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  geom_point(aes(fct_reorder(movement, relative_mpa_abs, .desc = TRUE), relative_mpa_abs, color = "Retention + Import"), size = 3.5) +
  geom_point(aes(movement, relative_mpa_R, color = "Retention"), size = 3) +
  geom_point(aes(movement, relative_mpa_I, color = "Import"), size = 3) +
  geom_point(aes(movement, relative_mpa_E, color = "Export"), size = 3) +
  theme_bw(base_size = 16) +
  facet_grid(~fp) +
  theme(strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "Movement (Adult / Larval)", 
       y = "Relative Number of Settlers (Adult / Larval)",
       color = "") +
  ylim(c(NA, 16)) +
  scale_color_manual(values = colors)

ggsave(p1, path = here::here("figs"), file = paste0("figS2.pdf"), height = 6, width = 8)

sub_connect = connect %>% 
  filter(eggs == "med") %>% 
  filter(mpa_size == 4) %>% 
  filter(mpa_spacing == 0) %>% 
  mutate(fp = case_when(
    fp == "med" ~ "medium",
    TRUE ~ fp
  )) %>% 
  mutate(fp = fct_relevel(fp, c("low", "medium", "high")))

p1 = ggplot(sub_connect) +
  geom_hline(aes(yintercept = 1), color = "red", alpha = 0.5, linetype = "dashed") +
  # geom_rect(aes(xmin = 0.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +  
  # geom_rect(aes(xmin = 4.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  # geom_rect(aes(xmin = 6.5, xmax = 9.5, ymin = 0, ymax = 16, color = "Retention"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  geom_point(aes(fct_reorder(movement, relative_mpa_abs, .desc = TRUE), relative_mpa_abs, color = "Retention + Import"), size = 3.5) +
  geom_point(aes(movement, relative_mpa_R, color = "Retention"), size = 3) +
  geom_point(aes(movement, relative_mpa_I, color = "Import"), size = 3) +
  geom_point(aes(movement, relative_mpa_E, color = "Export"), size = 3) +
  theme_bw(base_size = 16) +
  facet_grid(~fp) +
  theme(strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "Movement (Adult / Larval)", 
       y = "Relative Number of Settlers (Adult / Larval)",
       color = "") +
  ylim(c(NA, 16)) +
  scale_color_manual(values = colors)

ggsave(p1, path = here::here("figs"), file = paste0("figS3.pdf"), height = 6, width = 8)

sub_connect = connect %>% 
  filter(eggs == "med") %>% 
  filter(mpa_size == 4) %>% 
  filter(mpa_spacing == 2) %>% 
  mutate(fp = case_when(
    fp == "med" ~ "medium",
    TRUE ~ fp
  )) %>% 
  mutate(fp = fct_relevel(fp, c("low", "medium", "high")))

p1 = ggplot(sub_connect) +
  geom_hline(aes(yintercept = 1), color = "red", alpha = 0.5, linetype = "dashed") +
  # geom_rect(aes(xmin = 0.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +  
  # geom_rect(aes(xmin = 4.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  # geom_rect(aes(xmin = 6.5, xmax = 9.5, ymin = 0, ymax = 16, color = "Retention"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  geom_point(aes(fct_reorder(movement, relative_mpa_abs, .desc = TRUE), relative_mpa_abs, color = "Retention + Import"), size = 3.5) +
  geom_point(aes(movement, relative_mpa_R, color = "Retention"), size = 3) +
  geom_point(aes(movement, relative_mpa_I, color = "Import"), size = 3) +
  geom_point(aes(movement, relative_mpa_E, color = "Export"), size = 3) +
  theme_bw(base_size = 16) +
  facet_grid(~fp) +
  theme(strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "Movement (Adult / Larval)", 
       y = "Relative Number of Settlers (Adult / Larval)",
       color = "") +
  ylim(c(NA, 16)) +
  scale_color_manual(values = colors)

ggsave(p1, path = here::here("figs"), file = paste0("figS4.pdf"), height = 6, width = 8)

sub_connect = connect %>% 
  filter(eggs == "med") %>% 
  filter(mpa_size == 4) %>% 
  filter(mpa_spacing == 4) %>% 
  mutate(fp = case_when(
    fp == "med" ~ "medium",
    TRUE ~ fp
  )) %>% 
  mutate(fp = fct_relevel(fp, c("low", "medium", "high")))

p1 = ggplot(sub_connect) +
  geom_hline(aes(yintercept = 1), color = "red", alpha = 0.5, linetype = "dashed") +
  # geom_rect(aes(xmin = 0.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +  
  # geom_rect(aes(xmin = 4.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  # geom_rect(aes(xmin = 6.5, xmax = 9.5, ymin = 0, ymax = 16, color = "Retention"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  geom_point(aes(fct_reorder(movement, relative_mpa_abs, .desc = TRUE), relative_mpa_abs, color = "Retention + Import"), size = 3.5) +
  geom_point(aes(movement, relative_mpa_R, color = "Retention"), size = 3) +
  geom_point(aes(movement, relative_mpa_I, color = "Import"), size = 3) +
  geom_point(aes(movement, relative_mpa_E, color = "Export"), size = 3) +
  theme_bw(base_size = 16) +
  facet_grid(~fp) +
  theme(strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "Movement (Adult / Larval)", 
       y = "Relative Number of Settlers (Adult / Larval)",
       color = "") +
  ylim(c(NA, 16)) +
  scale_color_manual(values = colors)

ggsave(p1, path = here::here("figs"), file = paste0("figS5.pdf"), height = 6, width = 8)

sub_connect = connect %>% 
  filter(eggs == "med") %>% 
  filter(mpa_size == 4) %>% 
  filter(mpa_spacing == 8) %>% 
  mutate(fp = case_when(
    fp == "med" ~ "medium",
    TRUE ~ fp
  )) %>% 
  mutate(fp = fct_relevel(fp, c("low", "medium", "high")))

p1 = ggplot(sub_connect) +
  geom_hline(aes(yintercept = 1), color = "red", alpha = 0.5, linetype = "dashed") +
  # geom_rect(aes(xmin = 0.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +  
  # geom_rect(aes(xmin = 4.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  # geom_rect(aes(xmin = 6.5, xmax = 9.5, ymin = 0, ymax = 16, color = "Retention"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  geom_point(aes(fct_reorder(movement, relative_mpa_abs, .desc = TRUE), relative_mpa_abs, color = "Retention + Import"), size = 3.5) +
  geom_point(aes(movement, relative_mpa_R, color = "Retention"), size = 3) +
  geom_point(aes(movement, relative_mpa_I, color = "Import"), size = 3) +
  geom_point(aes(movement, relative_mpa_E, color = "Export"), size = 3) +
  theme_bw(base_size = 16) +
  facet_grid(~fp) +
  theme(strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "Movement (Adult / Larval)", 
       y = "Relative Number of Settlers (Adult / Larval)",
       color = "") +
  ylim(c(NA, 16)) +
  scale_color_manual(values = colors)

ggsave(p1, path = here::here("figs"), file = paste0("figS6.pdf"), height = 6, width = 8)

sub_connect = connect %>% 
  filter(eggs == "med") %>% 
  filter(mpa_size == 4) %>% 
  filter(mpa_spacing == 16) %>% 
  mutate(fp = case_when(
    fp == "med" ~ "medium",
    TRUE ~ fp
  )) %>% 
  mutate(fp = fct_relevel(fp, c("low", "medium", "high")))

p1 = ggplot(sub_connect) +
  geom_hline(aes(yintercept = 1), color = "red", alpha = 0.5, linetype = "dashed") +
  # geom_rect(aes(xmin = 0.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +  
  # geom_rect(aes(xmin = 4.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  # geom_rect(aes(xmin = 6.5, xmax = 9.5, ymin = 0, ymax = 16, color = "Retention"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  geom_point(aes(fct_reorder(movement, relative_mpa_abs, .desc = TRUE), relative_mpa_abs, color = "Retention + Import"), size = 3.5) +
  geom_point(aes(movement, relative_mpa_R, color = "Retention"), size = 3) +
  geom_point(aes(movement, relative_mpa_I, color = "Import"), size = 3) +
  geom_point(aes(movement, relative_mpa_E, color = "Export"), size = 3) +
  theme_bw(base_size = 16) +
  facet_grid(~fp) +
  theme(strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "Movement (Adult / Larval)", 
       y = "Relative Number of Settlers (Adult / Larval)",
       color = "") +
  ylim(c(NA, 16)) +
  scale_color_manual(values = colors)

ggsave(p1, path = here::here("figs"), file = paste0("figS7.pdf"), height = 6, width = 8)

sub_connect = connect %>% 
  filter(eggs == "med") %>% 
  filter(mpa_size == 8) %>% 
  filter(mpa_spacing == 0) %>% 
  mutate(fp = case_when(
    fp == "med" ~ "medium",
    TRUE ~ fp
  )) %>% 
  mutate(fp = fct_relevel(fp, c("low", "medium", "high")))

p1 = ggplot(sub_connect) +
  geom_hline(aes(yintercept = 1), color = "red", alpha = 0.5, linetype = "dashed") +
  # geom_rect(aes(xmin = 0.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +  
  # geom_rect(aes(xmin = 4.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  # geom_rect(aes(xmin = 6.5, xmax = 9.5, ymin = 0, ymax = 16, color = "Retention"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  geom_point(aes(fct_reorder(movement, relative_mpa_abs, .desc = TRUE), relative_mpa_abs, color = "Retention + Import"), size = 3.5) +
  geom_point(aes(movement, relative_mpa_R, color = "Retention"), size = 3) +
  geom_point(aes(movement, relative_mpa_I, color = "Import"), size = 3) +
  geom_point(aes(movement, relative_mpa_E, color = "Export"), size = 3) +
  theme_bw(base_size = 16) +
  facet_grid(~fp) +
  theme(strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "Movement (Adult / Larval)", 
       y = "Relative Number of Settlers (Adult / Larval)",
       color = "") +
  ylim(c(NA, 16)) +
  scale_color_manual(values = colors)

ggsave(p1, path = here::here("figs"), file = paste0("figS8.pdf"), height = 6, width = 8)

sub_connect = connect %>% 
  filter(eggs == "med") %>% 
  filter(mpa_size == 8) %>% 
  filter(mpa_spacing == 2) %>% 
  mutate(fp = case_when(
    fp == "med" ~ "medium",
    TRUE ~ fp
  )) %>% 
  mutate(fp = fct_relevel(fp, c("low", "medium", "high")))

p1 = ggplot(sub_connect) +
  geom_hline(aes(yintercept = 1), color = "red", alpha = 0.5, linetype = "dashed") +
  # geom_rect(aes(xmin = 0.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +  
  # geom_rect(aes(xmin = 4.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  # geom_rect(aes(xmin = 6.5, xmax = 9.5, ymin = 0, ymax = 16, color = "Retention"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  geom_point(aes(fct_reorder(movement, relative_mpa_abs, .desc = TRUE), relative_mpa_abs, color = "Retention + Import"), size = 3.5) +
  geom_point(aes(movement, relative_mpa_R, color = "Retention"), size = 3) +
  geom_point(aes(movement, relative_mpa_I, color = "Import"), size = 3) +
  geom_point(aes(movement, relative_mpa_E, color = "Export"), size = 3) +
  theme_bw(base_size = 16) +
  facet_grid(~fp) +
  theme(strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "Movement (Adult / Larval)", 
       y = "Relative Number of Settlers (Adult / Larval)",
       color = "") +
  ylim(c(NA, 16)) +
  scale_color_manual(values = colors)

ggsave(p1, path = here::here("figs"), file = paste0("figS9.pdf"), height = 6, width = 8)

sub_connect = connect %>% 
  filter(eggs == "med") %>% 
  filter(mpa_size == 8) %>% 
  filter(mpa_spacing == 4) %>% 
  mutate(fp = case_when(
    fp == "med" ~ "medium",
    TRUE ~ fp
  )) %>% 
  mutate(fp = fct_relevel(fp, c("low", "medium", "high")))

p1 = ggplot(sub_connect) +
  geom_hline(aes(yintercept = 1), color = "red", alpha = 0.5, linetype = "dashed") +
  # geom_rect(aes(xmin = 0.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +  
  # geom_rect(aes(xmin = 4.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  # geom_rect(aes(xmin = 6.5, xmax = 9.5, ymin = 0, ymax = 16, color = "Retention"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  geom_point(aes(fct_reorder(movement, relative_mpa_abs, .desc = TRUE), relative_mpa_abs, color = "Retention + Import"), size = 3.5) +
  geom_point(aes(movement, relative_mpa_R, color = "Retention"), size = 3) +
  geom_point(aes(movement, relative_mpa_I, color = "Import"), size = 3) +
  geom_point(aes(movement, relative_mpa_E, color = "Export"), size = 3) +
  theme_bw(base_size = 16) +
  facet_grid(~fp) +
  theme(strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "Movement (Adult / Larval)", 
       y = "Relative Number of Settlers (Adult / Larval)",
       color = "") +
  ylim(c(NA, 16)) +
  scale_color_manual(values = colors)

ggsave(p1, path = here::here("figs"), file = paste0("figS10.pdf"), height = 6, width = 8)

sub_connect = connect %>% 
  filter(eggs == "med") %>% 
  filter(mpa_size == 8) %>% 
  filter(mpa_spacing == 8) %>% 
  mutate(fp = case_when(
    fp == "med" ~ "medium",
    TRUE ~ fp
  )) %>% 
  mutate(fp = fct_relevel(fp, c("low", "medium", "high")))

p1 = ggplot(sub_connect) +
  geom_hline(aes(yintercept = 1), color = "red", alpha = 0.5, linetype = "dashed") +
  # geom_rect(aes(xmin = 0.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +  
  # geom_rect(aes(xmin = 4.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  # geom_rect(aes(xmin = 6.5, xmax = 9.5, ymin = 0, ymax = 16, color = "Retention"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  geom_point(aes(fct_reorder(movement, relative_mpa_abs, .desc = TRUE), relative_mpa_abs, color = "Retention + Import"), size = 3.5) +
  geom_point(aes(movement, relative_mpa_R, color = "Retention"), size = 3) +
  geom_point(aes(movement, relative_mpa_I, color = "Import"), size = 3) +
  geom_point(aes(movement, relative_mpa_E, color = "Export"), size = 3) +
  theme_bw(base_size = 16) +
  facet_grid(~fp) +
  theme(strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "Movement (Adult / Larval)", 
       y = "Relative Number of Settlers (Adult / Larval)",
       color = "") +
  ylim(c(NA, 16)) +
  scale_color_manual(values = colors)

ggsave(p1, path = here::here("figs"), file = paste0("figS11.pdf"), height = 6, width = 8)

sub_connect = connect %>% 
  filter(eggs == "med") %>% 
  filter(mpa_size == 8) %>% 
  filter(mpa_spacing == 16) %>% 
  mutate(fp = case_when(
    fp == "med" ~ "medium",
    TRUE ~ fp
  )) %>% 
  mutate(fp = fct_relevel(fp, c("low", "medium", "high")))

p1 = ggplot(sub_connect) +
  geom_hline(aes(yintercept = 1), color = "red", alpha = 0.5, linetype = "dashed") +
  # geom_rect(aes(xmin = 0.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +  
  # geom_rect(aes(xmin = 4.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  # geom_rect(aes(xmin = 6.5, xmax = 9.5, ymin = 0, ymax = 16, color = "Retention"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  geom_point(aes(fct_reorder(movement, relative_mpa_abs, .desc = TRUE), relative_mpa_abs, color = "Retention + Import"), size = 3.5) +
  geom_point(aes(movement, relative_mpa_R, color = "Retention"), size = 3) +
  geom_point(aes(movement, relative_mpa_I, color = "Import"), size = 3) +
  geom_point(aes(movement, relative_mpa_E, color = "Export"), size = 3) +
  theme_bw(base_size = 16) +
  facet_grid(~fp) +
  theme(strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "Movement (Adult / Larval)", 
       y = "Relative Number of Settlers (Adult / Larval)",
       color = "") +
  ylim(c(NA, 16)) +
  scale_color_manual(values = colors)

ggsave(p1, path = here::here("figs"), file = paste0("figS12.pdf"), height = 6, width = 8)

sub_connect = connect %>% 
  filter(eggs == "med") %>% 
  filter(mpa_size == 16) %>% 
  filter(mpa_spacing == 0) %>% 
  mutate(fp = case_when(
    fp == "med" ~ "medium",
    TRUE ~ fp
  )) %>% 
  mutate(fp = fct_relevel(fp, c("low", "medium", "high")))

p1 = ggplot(sub_connect) +
  geom_hline(aes(yintercept = 1), color = "red", alpha = 0.5, linetype = "dashed") +
  # geom_rect(aes(xmin = 0.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +  
  # geom_rect(aes(xmin = 4.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  # geom_rect(aes(xmin = 6.5, xmax = 9.5, ymin = 0, ymax = 16, color = "Retention"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  geom_point(aes(fct_reorder(movement, relative_mpa_abs, .desc = TRUE), relative_mpa_abs, color = "Retention + Import"), size = 3.5) +
  geom_point(aes(movement, relative_mpa_R, color = "Retention"), size = 3) +
  geom_point(aes(movement, relative_mpa_I, color = "Import"), size = 3) +
  geom_point(aes(movement, relative_mpa_E, color = "Export"), size = 3) +
  theme_bw(base_size = 16) +
  facet_grid(~fp) +
  theme(strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "Movement (Adult / Larval)", 
       y = "Relative Number of Settlers (Adult / Larval)",
       color = "") +
  ylim(c(NA, 16)) +
  scale_color_manual(values = colors)

ggsave(p1, path = here::here("figs"), file = paste0("figS13.pdf"), height = 6, width = 8)

