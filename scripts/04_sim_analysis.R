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
    movement = c(paste0(adult, " / ", larval))
  ) %>%
  mutate(
    fished_abs = adult_fished_abs + larvae_fished_abs,
    mpa_abs = adult_mpa_abs + larvae_mpa_abs,
    rel = adult_rel + larvae_rel,
    fished_export = adult_ES_fished + larvae_ES_fished,
    mpa_export = adult_ES_mpa + larvae_ES_mpa,
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


# Sobol Sensitivity --------------------------------------------------------

sobol_data = mpa %>% 
  ungroup() %>% 
  filter(generation == 90) %>% 
  filter(mpa == "MPA 1") %>% 
  mutate(movement = c(paste0(adult, " / ", larval))) %>% 
  select(eggs, fp, adult, larval, movement, mpa_size, mpa_spacing, mean_pop)

input_data = sobol_data %>% 
  select(-mean_pop)
output_data = sobol_data %>% 
  select(mean_pop) %>% 
  pull()

x = as.matrix(sobol_data)


sobol_indices <- sobol(model = NULL,
                       X1 = ?,
                       X2 = ?,
                       order = 1, nboot = 100)

sobol_summary <- data.frame(Parameter = rownames(sobol_indices$S),
                            First_Order = sobol_indices$S[,1],
                            Total_Order = sobol_indices$ST)

# Theoretical vs Realized Connectivity ------------------------------------

sub_connect = connect %>% 
  filter(mpa_size %in% c(4, 8)) %>% 
  filter(mpa_spacing == 8) %>% 
  filter(eggs == "low") %>% 
  filter(fp == "high")
  
p1 = ggplot(sub_connect) +
  geom_hline(aes(yintercept = 1), color = "red", alpha = 0.5, linetype = "dashed") +
  geom_point(aes(adult, relative_rel, color = as.factor(larval), shape = as.factor(mpa_size)), size = 3) +
  geom_line(aes(adult, relative_rel, color = as.factor(larval), linetype = as.factor(mpa_size)), linewidth = 1) +
  theme_bw() +
  scale_color_viridis_d(end = 0.9) +
  labs(x = "Adult Movement",
       y = "Relative Connectivity (Adult / Larval)",
       color = "Larval Movement",
       shape = "MPA Size",
       linetype = "MPA Size") +
  ylim(c(0.2, 8.5))

p2 = ggplot(sub_connect) +
  geom_hline(aes(yintercept = 1), color = "red", alpha = 0.5, linetype = "dashed") +
  geom_point(aes(adult, relative_fished_abs, color = as.factor(larval), shape = as.factor(mpa_size)), size = 3) +
  geom_line(aes(adult, relative_fished_abs, color = as.factor(larval), linetype = as.factor(mpa_size)), linewidth = 1) +
  theme_bw() +
  scale_color_viridis_d(end = 0.9) +
  labs(x = "Adult Movement",
       y = "",
       color = "Larval Movement",
       shape = "MPA Size",
       linetype = "MPA Size") +
  ylim(c(0.2, 8.5))

p3 = ggplot(sub_connect) +
  geom_hline(aes(yintercept = 1), color = "red", alpha = 0.5, linetype = "dashed") +
  geom_point(aes(adult, relative_mpa_abs, color = as.factor(larval), shape = as.factor(mpa_size)), size = 3) +
  geom_line(aes(adult, relative_mpa_abs, color = as.factor(larval), linetype = as.factor(mpa_size)), linewidth = 1) +
  theme_bw() +
  scale_color_viridis_d(end = 0.9) +
  labs(x = "Adult Movement",
       y = "",
       color = "Larval Movement",
       shape = "MPA Size",
       linetype = "MPA Size") +
  ylim(c(0.2, 8.5))

# May Drop the Size bit 
p = p1 + p2 + p3 + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect")

ggsave(p, path = here::here("figs"), file = paste0("fig1.pdf"), height = 8, width = 20)

# Fishing Pressure and Connectivity ---------------------------------------

sub_connect = connect %>% 
  filter(mpa_size == 8) %>% 
  filter(mpa_spacing %in% c(0,8)) %>% 
  filter(eggs == "med")

colors = c("Retention" = "#35b779", "Import" = "#440154", "Export" = "#31688e", "Retention + Import" = "#21918c")

p1 = ggplot(sub_connect %>% filter(mpa_spacing == 0)) +
  geom_hline(aes(yintercept = 1), color = "red", alpha = 0.5, linetype = "dashed") +
  geom_rect(aes(xmin = 0.5, xmax = 3.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +  
  geom_rect(aes(xmin = 6.5, xmax = 9.5, ymin = 0, ymax = 16, color = "Retention"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  geom_point(aes(fct_reorder(movement, relative_mpa_R, .desc = TRUE), relative_mpa_R, color = "Retention"), size = 3) +
  geom_point(aes(movement, relative_mpa_I, color = "Import"), size = 3) +
  geom_point(aes(movement, relative_mpa_E, color = "Export"), size = 3) +
  theme_bw() +
  facet_wrap(~fp) +
  theme(strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "", 
       y = "Relative Connectivity (Adult / Larval)",
       color = "") +
  ylim(c(NA, 16)) +
  scale_color_manual(values = colors)

p2 = ggplot(sub_connect %>% filter(mpa_spacing == 8)) +
  geom_hline(aes(yintercept = 1), color = "red", alpha = 0.5, linetype = "dashed") +
  geom_rect(aes(xmin = 0.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +  
  # geom_rect(aes(xmin = 4.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) + 
  geom_rect(aes(xmin = 6.5, xmax = 9.5, ymin = 0, ymax = 16, color = "Retention"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  geom_point(aes(fct_reorder(movement, relative_mpa_R, .desc = TRUE), relative_mpa_R, color = "Retention"), size = 3) +
  geom_point(aes(movement, relative_mpa_I, color = "Import"), size = 3) +
  geom_point(aes(movement, relative_mpa_E, color = "Export"), size = 3) +
  theme_bw() +
  facet_wrap(~fp) +
  theme(strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "Movement (Adult / Larval)", 
       y = "Relative Connectivity (Adult / Larval)",
       color = "") +
  ylim(c(NA, 16)) +
  scale_color_manual(values = colors) +
  theme(legend.position = "none")

# may drop to just spacing == 8 if patterns are the same
p = p1 / p2 + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect")

ggsave(p, path = here::here("figs"), file = paste0("fig2.pdf"), height = 15, width = 8)

# Shifts in Connectivity Driver -------------------------------------------

sub_connect = connect %>% 
  filter(eggs == "med") %>% 
  filter(mpa_size %in% c(4, 8)) %>% 
  filter(move_cat %in% c("high / low",  "high / medium", "medium / low")) %>% 
  pivot_longer(cols = c(relative_mpa_abs, relative_mpa_R), names_to = "measure", values_to = "connectivity") %>% 
  mutate(measure = case_when(
    measure == "relative_mpa_R" ~ "Retention",
    measure == "relative_mpa_abs" ~ "Retention + Import"
  )) %>% 
  mutate(movement = fct_relevel(movement, c("8 / 4", "16 / 8", "16 / 4")))

p1 = ggplot(sub_connect %>% filter(connectivity < 50)) +
  geom_hline(aes(yintercept = 1), color = "red", alpha = 0.5, linetype = "dashed") +
  geom_point(aes(movement, connectivity, color = as.factor(mpa_spacing), shape = as.factor(mpa_size)), size = 2) +
  theme_bw() +
  labs(x = "Movement (Adult / Larval)", 
       y = "Relative Connectivity (Adult / Larval)",
       color = "MPA Spacing",
       shape = "MPA Size") +
  facet_wrap(fp~measure, scales = "free", ncol = 2) +
  scale_color_viridis_d(end = 0.9) +
  theme(strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave(p1, path = here::here("figs"), file = paste0("fig3.pdf"), height = 8, width = 8)

# In/Out Ratio ------------------------------------------------------------

eq_pop_size_sub = eq_pop_size %>% 
  filter(mpa_size %in% c(4, 8)) %>% 
  filter(fp == "high") %>% 
  filter(eggs == "low")

p1 = ggplot(eq_pop_size_sub) +
  geom_hline(aes(yintercept = 0), color = "red", alpha = 0.5, linetype = "dashed") +
  # geom_hline(aes(yintercept = 0.7), color = "red", alpha = 0.5, linetype = "dashed") +
  geom_point(aes(mpa_spacing, in_out, color = movement, shape = movement), size = 2) +
  facet_wrap(~mpa_size) +
  theme_bw() +
  theme(strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "white")) +
  labs(x = "MPA Spacing",
       y = "Log(Inside MPA Population / Outside MPA Population)",
       color = "Movement (Adult / Larval)",
       shape = "Movement (Adult / Larval)") +
  scale_color_viridis_d(end = 0.9) +
  scale_shape_manual(values = c(19, 1, 1, 19, 19, 1, 19, 19, 19))

ggsave(p1, path = here::here("figs"), file = paste0("fig4.pdf"), height = 6, width = 8)

# OLD ---------------------------------------------------------------------


# ggplot(eq_pop_size) +
#   geom_point(aes(movement, in_out, color = mpa_spacing)) +
#   facet_grid(fp~eggs, labeller = label_both, scales = "free_y") +
#   theme_bw() +
#   theme(strip.text = element_text(face = "bold"),
#         strip.background = element_rect(fill = "white")) +
#   labs(x = "Relative Movement",
#        y = "Population",
#        color = "MPA Spacing") +
#   scale_color_viridis_d(end = 0.9)
# 
# p1 = ggplot(eq_pop_size %>% filter(mpa_spacing == 0)) +
#   geom_point(aes(eggs, in_out, color = movement)) +
#   facet_wrap(~fp) +
#   theme_bw() +
#   theme(strip.text = element_text(face = "bold"),
#         strip.background = element_rect(fill = "white")) +
#   labs(x = "Eggs Produced",
#        y = "Inside Outside Ratio",
#        color = "Movement \n (adult / larvae)") +
#   scale_color_viridis_d(end = 0.9)
# p1
# 
# ggsave(p1, path = here::here("figs"), file = paste0("fig3.pdf"), height = 6, width = 10, limitsize = FALSE)
# 
# # Connect Figs ------------------------------------------------------------
# 
# p1 <- ggplot(connect) +
#   geom_hline(aes(yintercept = 1), linetype = "dashed", linewidth = 1) +
#   geom_point(aes(movement, relative_mpa_R, color = "Retention", shape = mpa_spacing), size = 3) +
#   # geom_point(aes(movement, relative_mpa_I, color = "Import"), size = 5) +
#   theme_bw(base_size = 20) +
#   facet_grid(fp~eggs, labeller = label_both) +
#   labs(
#     x = "Relative Movement",
#     y = "Relative Contribution",
#     color = "",
#     shape = "MPA Spacing"
#   ) +
#   scale_color_manual(values = c("Retention" = "green", "Import" = "blue", "Export" = "purple")) +
#   theme(strip.text = element_text(face = "bold"),
#         strip.background = element_rect(fill = "white"),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
#   scale_y_log10() 
# 
# p2 <- ggplot(connect) +
#   geom_hline(aes(yintercept = 1), linetype = "dashed", linewidth = 1) +
#   geom_point(aes(movement, relative_mpa_I, color = "Import", shape = mpa_spacing), size = 3) +
#   theme_bw(base_size = 20) +
#   facet_grid(fp~eggs, labeller = label_both) +
#   labs(
#     x = "Relative Movement",
#     y = "Relative Contribution",
#     color = "",
#     shape = "MPA Spacing"
#   ) +
#   scale_color_manual(values = c("Retention" = "green", "Import" = "blue", "Export" = "purple")) +
#   theme(strip.text = element_text(face = "bold"),
#         strip.background = element_rect(fill = "white"),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
#   scale_y_log10() 
# 
# p3 <- ggplot(connect) +
#   geom_hline(aes(yintercept = 1), linetype = "dashed", linewidth = 1) +
#   geom_point(aes(movement, relative_mpa_E, color = "Export", shape = mpa_spacing), size = 3) +
#   theme_bw(base_size = 20) +
#   facet_grid(fp~eggs, labeller = label_both) +
#   labs(
#     x = "Relative Movement",
#     y = "Relative Contribution",
#     color = "",
#     shape = "MPA Spacing"
#   ) +
#   scale_color_manual(values = c("Retention" = "green", "Import" = "blue", "Export" = "purple")) +
#   theme(strip.text = element_text(face = "bold"),
#         strip.background = element_rect(fill = "white"),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
#   scale_y_log10()
# 
# plot <- p1 + p2 + p3 + plot_annotation(tag_levels = "A") + plot_layout(guides ="collect")
# 
# ggsave(plot, path = here::here("figs"), file = paste0("fig4.pdf"), height = 6, width = 25, limitsize = FALSE)
# 
# # OLD ---------------------------------------------------------------------
# 
# # 
# # p1 <- ggplot(mpa_connect) +
# #   geom_hline(aes(yintercept = 1), color = "red", linetype = "dashed", linewidth = 1) +
# #   geom_point(aes(adult, relative_c2, color = as.factor(larval)), size = 5) +
# #   theme_bw(base_size = 20) +
# #   scale_color_viridis_d(end = 0.9) +
# #   labs(
# #     x = "Adult Movement",
# #     y = "Relative Adult / Larval Connectivity",
# #     color = "Larval Movement",
# #     linetype = "Larval Movement",
# #     title = "Theoretical Connectivity"
# #   ) +
# #   geom_smooth(aes(adult, relative_c2, linetype = as.factor(larval)), se = FALSE, alpha = 0.3, color = "black", linewidth = 1) +
# #   ylim(c(0.2, 4.5))
# # 
# # ggsave(p1, path = here::here("figs", "pres"), file = paste0("theoretical.pdf"), height = 8, width = 8, limitsize = FALSE)
# # 
# # p2 <- ggplot(mpa_connect) +
# #   # geom_abline(aes()) +
# #   geom_hline(aes(yintercept = 1), color = "red", linetype = "dashed", linewidth = 1) +
# #   geom_point(aes(adult, relative_mpa_c1, color = as.factor(larval), shape = mpa_spacing), size = 5) +
# #   theme_bw(base_size = 20) +
# #   scale_color_viridis_d(end = 0.9) +
# #   facet_grid(fp~eggs, labeller = label_both, scales = "free_y") +
# #   labs(
# #     x = "Adult Movement",
# #     y = "",
# #     color = "Larval Movement",
# #     linetype = "Larval Movement",
# #     title = "Realized Connectivity",
# #     shape = "MPA Spacing"
# #   ) +
# #   geom_smooth(aes(adult, relative_mpa_c1, linetype = as.factor(larval)), se = FALSE, alpha = 0.3, color = "black", linewidth = 1) +
# #   theme(strip.text = element_text(face = "bold"),
# #         strip.background = element_rect(fill = "white"))
# # # ylim(c(0.2, 4.5))
# # 
# # plot <- p1 + p2 + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect")
# # 
# # # ggsave(plot, path = here::here("figs", "pres"), file = paste0("theoretical_v_realized.pdf"), height = 8, width = 12, limitsize = FALSE)
# # 
# # p3 <- ggplot(mpa_connect) +
# #   # geom_abline(aes()) +
# #   geom_hline(aes(yintercept = 1), color = "red", linetype = "dashed", linewidth = 1) +
# #   geom_point(aes(adult, relative_fished_c1, color = as.factor(larval), shape = mpa_spacing), size = 5) +
# #   theme_bw(base_size = 20) +
# #   scale_color_viridis_d(end = 0.9) +
# #   facet_grid(fp~eggs, labeller = label_both, scales = "free_y") +
# #   labs(
# #     x = "Adult Movement",
# #     y = "",
# #     color = "Larval Movement",
# #     linetype = "Larval Movement",
# #     title = "Realized Connectivity \n without MPA",
# #     shape = "MPA spacing"
# #   ) +
# #   geom_smooth(aes(adult, relative_fished_c1, linetype = as.factor(larval)), se = FALSE, alpha = 0.3, color = "black", linewidth = 1) +
# #   theme(strip.text = element_text(face = "bold"),
# #         strip.background = element_rect(fill = "white"))
# # 
# # plot <- p1 + p3 + p2 + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect")
# # 
# # ggsave(plot, path = here::here("figs", "pres"), file = paste0("theoretical_v_realized2.pdf"), height = 8, width = 16, limitsize = FALSE)
# # 
# 
# 
# # p1 <- ggplot(mpa_connect) +
# #   geom_hline(aes(yintercept = 1), linetype = "dashed", linewidth = 1) +
# #   geom_point(aes(movement, relative_mpa_c1, color = mpa_spacing), size = 5) +
# #   # geom_point(aes(movement, relative_mpa_I, color = "Import"), size = 5) +
# #   theme_bw(base_size = 20) +
# #   facet_grid(fp~eggs, labeller = label_both) +
# #   labs(
# #     x = "Relative Movement",
# #     y = "Relative Contribution Adult vs. Larvae",
# #     color = "",
# #     shape = "MPA Spacing"
# #   ) +
# #   theme(strip.text = element_text(face = "bold"),
# #         strip.background = element_rect(fill = "white"),
# #         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
# #   scale_y_log10() +
# #   scale_color_viridis_d(end = 0.9)
# 
