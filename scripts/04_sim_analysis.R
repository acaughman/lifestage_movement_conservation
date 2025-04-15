library(tidyverse)
library(patchwork)

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
    adult %in% c(0.5) ~ "low",
    adult %in% c(8) ~ "medium",
    adult %in% c(32) ~ "high"
  )) %>%
  mutate(larval_cat = case_when(
    larval %in% c(8) ~ "low",
    larval %in% c(32) ~ "medium",
    larval %in% c(192) ~ "high"
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
  mutate(
    eggs = as.factor(eggs),
    fp = as.factor(fp)
  ) %>%
  mutate(
    eggs = fct_relevel(eggs, c("low", "med", "high")),
    fp = fct_relevel(fp, c("low", "med", "high"))
  ) %>%
  mutate(movement = fct_relevel(movement, c(
    "0.5 / 8", "0.5 / 32", "0.5 / 192",
    "8 / 8", "8 / 32", "8 / 192",
    "32 / 8", "32 / 32", "32 / 192"
  ))) %>%
  pivot_wider(names_from = mpa, values_from = mean_pop) %>%
  mutate(move_cat = fct_relevel(
    move_cat,
    "low / low", "low / medium", "low / high",
    "medium / low", "medium / medium", "medium / high",
    "high / low", "high / medium", "high / high"
  ))

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
    relative_mpa_abs = adult_mpa_abs / mpa_abs,
    relative_fished_abs = adult_fished_abs / fished_abs,
    relative_rel = adult_rel / rel,
    relative_mpa_R = adult_RS_mpa / mpa_ret,
    relative_fished_R = adult_RS_fished / fished_ret,
    relative_mpa_I = adult_IS_mpa / mpa_import,
    relative_fished_I = adult_IS_fished / fished_import,
    relative_mpa_E = adult_ES_mpa / mpa_export,
    relative_fished_E = adult_ES_fished / fished_export,
    adult_relative_mpa = adult_RS_mpa / adult_mpa_abs,
    adult_relative_fished = adult_RS_fished / adult_fished_abs,
    larvae_relative_mpa = larvae_RS_mpa / larvae_mpa_abs,
    larvae_relative_fished = larvae_RS_fished / larvae_fished_abs,
    relative_ret_imp = mpa_ret / (mpa_import + mpa_ret)
  ) %>%
  mutate(adult_cat = case_when(
    adult %in% c(0.5) ~ "low",
    adult %in% c(8) ~ "medium",
    adult %in% c(32) ~ "high"
  )) %>%
  mutate(larval_cat = case_when(
    larval %in% c(8) ~ "low",
    larval %in% c(32) ~ "medium",
    larval %in% c(192) ~ "high"
  )) %>%
  mutate(
    movement = c(paste0(adult, " / ", larval)),
    move_cat = c(paste0(adult_cat, " / ", larval_cat)),
    move_ratio = adult / larval
  ) %>%
  mutate(movement = fct_relevel(movement, c(
    "0.5 / 8", "0.5 / 32", "0.5 / 192",
    "8 / 8", "8 / 32", "8 / 192",
    "32 / 8", "32 / 32", "32 / 192"
  ))) %>%
  mutate(ratio_cat = case_when(
    move_ratio < 1 ~ "Larval Greater",
    move_ratio == 1 ~ "Equal",
    move_ratio > 1 ~ "Adult Greater"
  )) %>%
  mutate(mpa_spacing = as.factor(mpa_spacing)) %>%
  mutate(
    eggs = as.factor(eggs),
    fp = as.factor(fp)
  ) %>%
  mutate(
    eggs = fct_relevel(eggs, c("low", "med", "high")),
    fp = fct_relevel(fp, c("low", "med", "high"))
  ) %>%
  mutate(move_cat = fct_relevel(
    move_cat,
    "low / low", "low / medium", "low / high",
    "medium / low", "medium / medium", "medium / high",
    "high / low", "high / medium", "high / high"
  ))

eq_pop_size <- mpa %>%
  filter(age == "adult") %>%
  filter(generation == 90)

colors <- c("Retention" = "#95d840", "Import" = "#238a8d", "Export" = "#440154", "Retention + Import" = "#3dbc74")

# Theoretical vs Realized Connectivity ------------------------------------

sub_connect <- connect %>%
  filter(mpa_size %in% c(4, 8)) %>%
  filter(mpa_spacing == 16) %>%
  filter(eggs == "low") %>%
  filter(fp == "high")

p1 <- ggplot(sub_connect) +
  geom_hline(aes(yintercept = 0.5), color = "red", alpha = 0.5, linetype = "dashed", linewidth = 1) +
  geom_point(aes(adult, relative_mpa_abs, color = as.factor(larval), shape = as.factor(mpa_size)), size = 3) +
  geom_line(aes(adult, relative_mpa_abs, color = as.factor(larval), linetype = as.factor(mpa_size)), linewidth = 1) +
  theme_bw(base_size = 16) +
  scale_color_viridis_d(end = 0.9) +
  labs(
    x = "Adult Movement",
    y = "Adult Proportion of Total Settlers ",
    color = "Larval Movement",
    shape = "MPA Size",
    linetype = "MPA Size"
  )

p2 <- ggplot(sub_connect) +
  #   geom_hline(aes(yintercept = 0.5), color = "red", alpha = 0.5, linetype = "dashed", linewidth = 1) +
  geom_point(aes(adult, relative_ret_imp, color = as.factor(larval), shape = as.factor(mpa_size)), size = 3) +
  geom_line(aes(adult, relative_ret_imp, color = as.factor(larval), linetype = as.factor(mpa_size)), linewidth = 1) +
  theme_bw(base_size = 16) +
  scale_color_viridis_d(end = 0.9) +
  labs(
    x = "Adult Movement",
    y = "Retention Proportion of Total Settlers",
    color = "Larval Movement",
    shape = "MPA Size",
    linetype = "MPA Size"
  )

p <- p1 + p2 + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect")

ggsave(p, path = here::here("figs"), file = paste0("fig2.pdf"), height = 8, width = 15)

sub_connect <- connect %>%
  filter(mpa_size %in% c(8)) %>%
  filter(mpa_spacing == 16) %>%
  filter(eggs == "low") %>%
  filter(fp == "high")

linetype <- c("Theoretical" = "solid", "MPA Implementation" = "dashed", "MPA Equilibrium" = "dotted")

p3 <- ggplot(sub_connect) +
  geom_hline(aes(yintercept = 0.5), color = "red", alpha = 0.5, linetype = "dashed", linewidth = 1) +
  geom_point(aes(adult, relative_rel, color = as.factor(larval)), size = 3) +
  geom_line(aes(adult, relative_rel, color = as.factor(larval), linetype = "Theoretical"), linewidth = 1) +
  geom_point(aes(adult, relative_fished_abs, color = as.factor(larval)), size = 3) +
  geom_line(aes(adult, relative_fished_abs, color = as.factor(larval), linetype = "MPA Implementation"), linewidth = 1) +
  geom_point(aes(adult, relative_mpa_abs, color = as.factor(larval)), size = 3) +
  geom_line(aes(adult, relative_mpa_abs, color = as.factor(larval), linetype = "MPA Equilibrium"), linewidth = 1) +
  theme_bw(base_size = 16) +
  scale_color_viridis_d(end = 0.9) +
  labs(
    x = "Adult Movement",
    y = "Adult Proportion of Total Settlers",
    color = "Larval Movement",
    linetype = ""
  ) +
  scale_linetype_manual(values = linetype)

ggsave(p3, path = here::here("figs"), file = paste0("figS1.pdf"), height = 8, width = 8)

# Connectivity ---------------------------------------

sub_connect <- connect %>%
  filter(mpa_size == 8) %>%
  filter(mpa_spacing == 16) %>%
  filter(fp == "high") %>%
  filter(eggs == "low")

p1 <- ggplot(sub_connect) +
  geom_hline(aes(yintercept = 0.5), color = "red", alpha = 0.5, linetype = "dashed", linewidth = 1) +
  # geom_rect(aes(xmin = 0.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  # geom_rect(aes(xmin = 4.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  # geom_rect(aes(xmin = 6.5, xmax = 9.5, ymin = 0, ymax = 16, color = "Retention"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  geom_point(aes(move_cat, relative_mpa_abs, color = "Retention + Import"), size = 3) +
  # geom_point(aes(move_cat, relative_mpa_R, color = "Retention"), size = 3) +
  # geom_point(aes(move_cat, relative_mpa_I, color = "Import"), size = 3) +
  # geom_point(aes(move_cat, relative_mpa_E, color = "Export"), size = 3) +
  theme_bw(base_size = 16) +
  # facet_wrap(~fp) +
  # theme(strip.text = element_text(face = "bold"),
  #       strip.background = element_rect(fill = "white"),
  #       axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(
    x = "Movement (Adult / Larval)",
    y = "Adult Proportion of Total Settlers ",
    color = ""
  ) +
  # ylim(c(NA, 11)) +
  scale_color_manual(values = colors) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

p2 <- ggplot(sub_connect) +
  geom_hline(aes(yintercept = 0.5), color = "red", alpha = 0.5, linetype = "dashed", linewidth = 1) +
  # geom_rect(aes(xmin = 0.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  # geom_rect(aes(xmin = 4.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  # geom_rect(aes(xmin = 6.5, xmax = 9.5, ymin = 0, ymax = 16, color = "Retention"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  # geom_point(aes(fct_reorder(move_cat, relative_mpa_abs, .desc = TRUE), relative_mpa_abs, color = "Retention + Import"), size = 3.5) +
  geom_point(aes(move_cat, relative_mpa_R, color = "Retention"), size = 3) +
  # geom_point(aes(move_cat, relative_mpa_I, color = "Import"), size = 3) +
  # geom_point(aes(move_cat, relative_mpa_E, color = "Export"), size = 3) +
  theme_bw(base_size = 16) +
  # facet_wrap(~fp) +
  # theme(strip.text = element_text(face = "bold"),
  #       strip.background = element_rect(fill = "white"),
  #       axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(
    x = "Movement (Adult / Larval)",
    y = "Adult Proportion of Total Retention",
    color = ""
  ) +
  # ylim(c(NA, 11)) +
  scale_color_manual(values = colors) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

p3 <- ggplot(sub_connect) +
  geom_hline(aes(yintercept = 0.5), color = "red", alpha = 0.5, linetype = "dashed", linewidth = 1) +
  # geom_rect(aes(xmin = 0.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  # geom_rect(aes(xmin = 4.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  # geom_rect(aes(xmin = 6.5, xmax = 9.5, ymin = 0, ymax = 16, color = "Retention"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  # geom_point(aes(fct_reorder(move_cat, relative_mpa_abs, .desc = TRUE), relative_mpa_abs, color = "Retention + Import"), size = 3.5) +
  # geom_point(aes(move_cat, relative_mpa_R, color = "Retention"), size = 3) +
  geom_point(aes(move_cat, relative_mpa_I, color = "Import"), size = 3) +
  # geom_point(aes(move_cat, relative_mpa_E, color = "Export"), size = 3) +
  theme_bw(base_size = 16) +
  # facet_wrap(~fp) +
  # theme(strip.text = element_text(face = "bold"),
  #       strip.background = element_rect(fill = "white"),
  #       axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(
    x = "Movement (Adult / Larval)",
    y = "Adult Proportion of Total Import",
    color = ""
  ) +
  # ylim(c(NA, 11)) +
  scale_color_manual(values = colors) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

p4 <- ggplot(sub_connect) +
  geom_hline(aes(yintercept = 0.5), color = "red", alpha = 0.5, linetype = "dashed", linewidth = 1) +
  # geom_rect(aes(xmin = 0.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  # geom_rect(aes(xmin = 4.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  # geom_rect(aes(xmin = 6.5, xmax = 9.5, ymin = 0, ymax = 16, color = "Retention"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  # geom_point(aes(fct_reorder(move_cat, relative_mpa_abs, .desc = TRUE), relative_mpa_abs, color = "Retention + Import"), size = 3.5) +
  # geom_point(aes(move_cat, relative_mpa_R, color = "Retention"), size = 3) +
  # geom_point(aes(move_cat, relative_mpa_I, color = "Import"), size = 3) +
  geom_point(aes(move_cat, relative_mpa_E, color = "Export"), size = 3) +
  theme_bw(base_size = 16) +
  # facet_wrap(~fp) +
  # theme(strip.text = element_text(face = "bold"),
  #       strip.background = element_rect(fill = "white"),
  #       axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(
    x = "Movement (Adult / Larval)",
    y = "Adult Proportion of Total Export",
    color = ""
  ) +
  # ylim(c(NA, 11)) +
  scale_color_manual(values = colors) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

p <- (p2 + p3 + p4) + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect")

ggsave(p, path = here::here("figs"), file = paste0("fig3.pdf"), height = 8, width = 20)

# Fishing Pressure and Connectivity ---------------------------------------

sub_connect <- connect %>%
  filter(mpa_size == 8) %>%
  filter(mpa_spacing == 16) %>%
  filter(eggs == "low") %>%
  mutate(fp = case_when(
    fp == "med" ~ "medium",
    TRUE ~ fp
  )) %>%
  mutate(fp = fct_relevel(fp, c("low", "medium", "high")))

p1 <- ggplot(sub_connect) +
  geom_hline(aes(yintercept = 0.5), color = "red", alpha = 0.5, linetype = "dashed", linewidth = 1) +
  geom_jitter(aes(move_cat, relative_mpa_R, color = "Retention"), size = 3, width = 0.25) +
  # geom_jitter(aes(move_cat, relative_mpa_I, color = "Import"), size = 3, width = 0.25) +
  # geom_jitter(aes(move_cat, relative_mpa_E, color = "Export"), size = 3, width = 0.25) +
  theme_bw(base_size = 16) +
  labs(
    x = "",
    y = "",
    color = ""
  ) +
  facet_wrap(~fp, ncol = 3) +
  scale_color_manual(values = colors) +
  theme(
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(fill = "white"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )
    
p2 <- ggplot(sub_connect) +
  geom_hline(aes(yintercept = 0.5), color = "red", alpha = 0.5, linetype = "dashed", linewidth = 1) +
  # geom_jitter(aes(move_cat, relative_mpa_R, color = "Retention"), size = 3, width = 0.25) +
  geom_jitter(aes(move_cat, relative_mpa_I, color = "Import"), size = 3, width = 0.25) +
  # geom_jitter(aes(move_cat, relative_mpa_E, color = "Export"), size = 3, width = 0.25) +
  theme_bw(base_size = 16) +
  labs(
    x = "",
    y = "Adult Proportion of Total Settlers",
    color = ""
  ) +
  facet_wrap(~fp, ncol = 3) +
  scale_color_manual(values = colors) +
  theme(
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(fill = "white"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

p3 <- ggplot(sub_connect) +
  geom_hline(aes(yintercept = 0.5), color = "red", alpha = 0.5, linetype = "dashed", linewidth = 1) +
  # geom_jitter(aes(move_cat, relative_mpa_R, color = "Retention"), size = 3, width = 0.25) +
  # geom_jitter(aes(move_cat, relative_mpa_I, color = "Import"), size = 3, width = 0.25) +
  geom_jitter(aes(move_cat, relative_mpa_E, color = "Export"), size = 3, width = 0.25) +
  theme_bw(base_size = 16) +
  labs(
    x = "Movement (Adult / Larval)",
    y = "",
    color = ""
  ) +
  facet_wrap(~fp, ncol = 3) +
  scale_color_manual(values = colors) +
  theme(
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(fill = "white"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

ggsave(p1 / p2 / p3 + plot_annotation(tag_levels = "A"), path = here::here("figs"), file = paste0("fig4.pdf"), height = 15, width = 10)

# MPA Design Connectivity --------------------------------------------------------------

sub_connect <- connect %>%
  filter(mpa_size %in% c(8)) %>%
  filter(eggs == "low") %>%
  filter(fp == "high")

p1 <- ggplot(sub_connect) +
  geom_hline(aes(yintercept = 0.5), color = "red", alpha = 0.5, linetype = "dashed", linewidth = 1) +
  geom_point(aes(mpa_spacing, relative_mpa_abs, color = move_cat), size = 3) +
  geom_line(aes(mpa_spacing, relative_mpa_abs, color = move_cat, group = as.factor(move_cat), linetype = ratio_cat), linewidth = 1) +
  theme_bw(base_size = 16) +
  # facet_wrap(~mpa_size)
  labs(
    x = "MPA Spacing (# grid cells)",
    y = "Adult Proportion of Total Settlers ",
    color = "Movement (Adult / Larval)",
    linetype = ""
  ) +
  scale_color_viridis_d(end = 0.9) +
  scale_linetype_manual(values = c("solid", "dotted", "dashed"))

sub_connect <- connect %>%
  filter(mpa_spacing %in% c(16)) %>%
  filter(eggs == "low") %>%
  filter(fp == "high")

p2 <- ggplot(sub_connect) +
  geom_hline(aes(yintercept = 0.5), color = "red", alpha = 0.5, linetype = "dashed", linewidth = 1) +
  geom_point(aes(mpa_size * mpa_size, relative_mpa_abs, color = move_cat), size = 3) +
  geom_line(aes(mpa_size * mpa_size, relative_mpa_abs, color = move_cat, group = as.factor(move_cat), linetype = ratio_cat), linewidth = 1) +
  theme_bw(base_size = 16) +
  # facet_wrap(~mpa_size)
  labs(
    x = "MPA Size (# grid cells)",
    y = "Adult Proportion of Total Settlers ",
    color = "Movement (Adult / Larval)",
    linetype = ""
  ) +
  scale_color_viridis_d(end = 0.9) +
  scale_linetype_manual(values = c("solid", "dotted", "dashed"))

plot <- p2 / p1 + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect")

ggsave(plot, path = here::here("figs"), file = paste0("fig5.pdf"), height = 12, width = 10)

# MPA Design In/out --------------------------------------------------------------

eq_pop_size_sub <- eq_pop_size %>%
  filter(mpa_size %in% c(8)) %>%
  filter(fp == "high") %>%
  filter(eggs == "low") %>%
  mutate(move_cat = fct_relevel(
    move_cat,
    "low / low", "low / medium", "low / high",
    "medium / low", "medium / medium", "medium / high",
    "high / low", "high / medium", "high / high"
  )) 

p3 <- ggplot(eq_pop_size_sub) +
  # geom_vline(aes(xintercept = 8), linetype = "dashed", alpha = 0.5) +
  # geom_vline(aes(xintercept = 0.5), linetype = "dashed", alpha = 0.5) +
  geom_hline(aes(yintercept = 0), color = "red", alpha = 0.5, linetype = "dashed") +
  geom_point(aes(mpa_spacing, in_out, color = move_cat), size = 3) +
  geom_line(aes(mpa_spacing, in_out, color = move_cat, group = move_cat), linewidth = 1) +
  # facet_wrap(~mpa_size) +
  theme_bw(base_size = 16) +
  theme(
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(fill = "white")
  ) +
  labs(
    x = "MPA Spacing (# grid cells)",
    y = "Log(In/Out)",
    color = "Movement (Adult / Larval)",
    shape = "MPA Size",
    linetype = "MPA Size"
  ) +
  scale_color_viridis_d(end = 0.9)

eq_pop_size_sub <- eq_pop_size %>%
  filter(mpa_spacing %in% c(0)) %>%
  filter(fp == "high") %>%
  filter(eggs == "low") %>%
  mutate(move_cat = fct_relevel(
    move_cat,
    "low / low", "low / medium", "low / high",
    "medium / low", "medium / medium", "medium / high",
    "high / low", "high / medium", "high / high"
  )) 

p4 <- ggplot(eq_pop_size_sub) +
  # geom_vline(aes(xintercept = 0.5 * 0.5 * 2), linetype = "dashed", alpha = 0.5) +
  # geom_vline(aes(xintercept = 8 * 8 * 2), linetype = "dashed", alpha = 0.5) +
  # geom_vline(aes(xintercept = 32 * 32 * 2), linetype = "dashed", alpha = 0.5) +
  geom_hline(aes(yintercept = 0), color = "red", alpha = 0.5, linetype = "dashed") +
  # geom_hline(aes(yintercept = 0.7), color = "red", alpha = 0.5, linetype = "dashed") +
  geom_point(aes(mpa_size * mpa_size, in_out, color = move_cat), size = 3) +
  geom_line(aes(mpa_size * mpa_size, in_out, color = move_cat, group = move_cat), linewidth = 1) +
  # facet_wrap(~mpa_size) +
  theme_bw(base_size = 16) +
  theme(
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(fill = "white"),
    legend.position = "none"
  ) +
  labs(
    x = "MPA Size (# grid cells)",
    y = "Log(In/Out)",
    color = "Movement (Adult / Larval)",
    shape = "MPA Size",
    linetype = "MPA Size"
  ) +
  scale_color_viridis_d(end = 0.9)

eq_pop_size_sub_a <- eq_pop_size %>%
  filter(mpa_size == 8) %>%
  filter(mpa_spacing == 16) %>%
  filter(fp == "high") %>%
  filter(eggs == "low") %>%
  group_by(adult) %>%
  summarise(mean_adult = mean(in_out))

eq_pop_size_sub_l <- eq_pop_size %>%
  filter(mpa_size == 8) %>%
  filter(mpa_spacing == 16) %>%
  filter(fp == "high") %>%
  filter(eggs == "low") %>%
  group_by(larval) %>%
  summarise(mean_larval = mean(in_out))

p5 <- ggplot() +
  geom_point(data = eq_pop_size_sub_a, aes(adult, mean_adult, color = "Adult"), size = 3) +
  geom_point(data = eq_pop_size_sub_l, aes(larval, mean_larval, color = "Larval"), size = 3) +
  geom_line(data = eq_pop_size_sub_a, aes(adult, mean_adult, color = "Adult"), linewidth = 1) +
  geom_line(data = eq_pop_size_sub_l, aes(larval, mean_larval, color = "Larval"), linewidth = 1) +
  theme_bw(base_size = 16) +
  labs(
    x = "Movement Extent",
    y = "Log(In/Out)",
    color = ""
  ) +
  scale_color_manual(values = c("Adult" = "#5ec962", "Larval" = "#440154")) +
  theme(legend.position = "bottom")

plot <- (p4 + p3) / (p5 + p6) + plot_annotation(tag_levels = "A")

ggsave(plot, path = here::here("figs"), file = paste0("fig6.pdf"), height = 12, width = 15)

# Percent Increase calc ---------------------------------------------------

# adult high to medium
(eq_pop_size_sub_a$mean_adult[2] - eq_pop_size_sub_a$mean_adult[3]) / abs(eq_pop_size_sub_a$mean_adult[3]) * 100

# adult medium to low
(eq_pop_size_sub_a$mean_adult[1] - eq_pop_size_sub_a$mean_adult[2]) / abs(eq_pop_size_sub_a$mean_adult[2]) * 100

# larval high to medium
(eq_pop_size_sub_l$mean_larval[2] - eq_pop_size_sub_l$mean_larval[3]) / abs(eq_pop_size_sub_l$mean_larval[3]) * 100

# adult medium to low
(eq_pop_size_sub_l$mean_larval[1] - eq_pop_size_sub_l$mean_larval[2]) / abs(eq_pop_size_sub_l$mean_larval[2]) * 100

# across eggs -------------------------------------------------------

eq_pop_size_sub <- eq_pop_size %>%
  filter(mpa_size == 8) %>%
  filter(fp == "high") %>%
  mutate(eggs = case_when(
    eggs == "med" ~ "medium",
    TRUE ~ eggs
  )) %>%
  mutate(eggs = fct_relevel(eggs, c("low", "medium", "high"))) %>%
  mutate(move_cat = fct_relevel(
    move_cat,
    "low / low", "low / medium", "low / high",
    "medium / low", "medium / medium", "medium / high",
    "high / low", "high / medium", "high / high"
  ))

sub_connect <- connect %>%
  filter(mpa_size == 8) %>%
  filter(fp == "high") %>%
  mutate(eggs = case_when(
    eggs == "med" ~ "medium",
    TRUE ~ eggs
  )) %>%
  mutate(eggs = fct_relevel(eggs, c("low", "medium", "high")))

p1 <- ggplot(sub_connect) +
  geom_hline(aes(yintercept = 0.5), color = "red", alpha = 0.5, linetype = "dashed", linewidth = 1) +
  # geom_hline(aes(yintercept = 0.7), color = "red", alpha = 0.5, linetype = "dashed") +
  geom_point(aes(mpa_spacing, relative_mpa_R, color = move_cat), size = 2) +
  geom_line(aes(mpa_spacing, relative_mpa_R, color = move_cat, group = move_cat)) +
  facet_wrap(~eggs) +
  theme_bw(base_size = 16) +
  theme(
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(fill = "white")
  ) +
  labs(
    x = "MPA Spacing",
    y = "Adult Proportion of Total Retention",
    color = "Movement (Adult / Larval)",
    shape = "Movement (Adult / Larval)"
  ) +
  scale_color_viridis_d(end = 0.9)

p2 <- ggplot(sub_connect) +
  geom_hline(aes(yintercept = 0.5), color = "red", alpha = 0.5, linetype = "dashed", linewidth = 1) +
  # geom_hline(aes(yintercept = 0.7), color = "red", alpha = 0.5, linetype = "dashed") +
  geom_point(aes(mpa_spacing, relative_mpa_I, color = move_cat), size = 2) +
  geom_line(aes(mpa_spacing, relative_mpa_I, color = move_cat, group = move_cat)) +
  facet_wrap(~eggs) +
  theme_bw(base_size = 16) +
  theme(
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(fill = "white")
  ) +
  labs(
    x = "MPA Spacing",
    y = "Adult Proportion of Total Import",
    color = "Movement (Adult / Larval)",
    shape = "Movement (Adult / Larval)"
  ) +
  scale_color_viridis_d(end = 0.9)

p3 <- ggplot(eq_pop_size_sub) +
  geom_hline(aes(yintercept = 0), color = "red", alpha = 0.5, linetype = "dashed", linewidth = 1) +
  # geom_hline(aes(yintercept = 0.7), color = "red", alpha = 0.5, linetype = "dashed") +
  geom_point(aes(mpa_spacing, in_out, color = move_cat), size = 2) +
  geom_line(aes(mpa_spacing, in_out, color = move_cat, group = move_cat)) +
  facet_wrap(~eggs) +
  theme_bw(base_size = 16) +
  theme(
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(fill = "white")
  ) +
  labs(
    x = "MPA Spacing",
    y = "Log(Inside MPA Population / Outside MPA Population)",
    color = "Movement (Adult / Larval)",
    shape = "Movement (Adult / Larval)"
  ) +
  scale_color_viridis_d(end = 0.9)

p4 <- ggplot(eq_pop_size_sub) +
  geom_hline(aes(yintercept = 0), color = "red", alpha = 0.5, linetype = "dashed") +
  # geom_hline(aes(yintercept = 0.7), color = "red", alpha = 0.5, linetype = "dashed") +
  geom_point(aes(mpa_spacing, mpa_1, color = move_cat), size = 2) +
  geom_line(aes(mpa_spacing, mpa_1, color = move_cat, group = move_cat)) +
  facet_wrap(~eggs) +
  theme_bw(base_size = 16) +
  theme(
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(fill = "white")
  ) +
  labs(
    x = "MPA Spacing",
    y = "Population Abundance",
    color = "Movement (Adult / Larval)",
    shape = "Movement (Adult / Larval)"
  ) +
  scale_color_viridis_d(end = 0.9)

p <- (p1 + p2) / (p3 + p4) + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect")

ggsave(p, path = here::here("figs"), file = paste0("figS14.pdf"), height = 15, width = 15)

# Across MPA sizes -------------------------------------------

sub_connect <- connect %>%
  filter(eggs == "low") %>%
  filter(mpa_size == 2) %>%
  filter(mpa_spacing == 0) %>%
  mutate(fp = case_when(
    fp == "med" ~ "medium",
    TRUE ~ fp
  )) %>%
  mutate(fp = fct_relevel(fp, c("low", "medium", "high")))

eq_pop_size_sub <- eq_pop_size %>%
  filter(eggs == "low") %>%
  filter(mpa_size == 2) %>%
  filter(mpa_spacing == 0) %>%
  mutate(fp = case_when(
    fp == "med" ~ "medium",
    TRUE ~ fp
  )) %>%
  mutate(fp = as.factor(fp)) %>%
  mutate(fp = fct_relevel(fp, c("low", "medium", "high"))) %>%
  mutate(move_cat = fct_relevel(
    move_cat,
    "low / low", "low / medium", "low / high",
    "medium / low", "medium / medium", "medium / high",
    "high / low", "high / medium", "high / high"
  ))

p1 <- ggplot(sub_connect) +
  geom_hline(aes(yintercept = 0.5), color = "red", alpha = 0.5, linetype = "dashed", linewidth = 1) +
  # geom_rect(aes(xmin = 0.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  # geom_rect(aes(xmin = 4.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  # geom_rect(aes(xmin = 6.5, xmax = 9.5, ymin = 0, ymax = 16, color = "Retention"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  geom_point(aes(move_cat, relative_mpa_abs, color = "Retention + Import"), size = 4) +
  geom_point(aes(move_cat, relative_mpa_R, color = "Retention"), size = 3) +
  # geom_point(aes(move_cat, relative_mpa_I, color = "Import"), size = 3) +
  geom_point(aes(move_cat, relative_mpa_E, color = "Export"), size = 3) +
  theme_bw(base_size = 16) +
  facet_grid(~fp) +
  theme(
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(fill = "white"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  ) +
  labs(
    x = "",
    y = "Adult Proportion of Total Settlers ",
    color = ""
  ) +
  scale_color_manual(values = colors)

p2 <- ggplot(eq_pop_size_sub) +
  geom_point(aes(move_cat, in_out, color = fp), size = 3) +
  theme_bw(base_size = 16) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  ) +
  scale_color_viridis_d(end = 0.9) +
  labs(
    x = "Movement (Adult / Larval)",
    y = "Log(In/Out)",
    color = "Fishing"
  )

ggsave(p1 / p2, path = here::here("figs"), file = paste0("figS2.pdf"), height = 12, width = 8)

sub_connect <- connect %>%
  filter(eggs == "low") %>%
  filter(mpa_size == 4) %>%
  filter(mpa_spacing == 0) %>%
  mutate(fp = case_when(
    fp == "med" ~ "medium",
    TRUE ~ fp
  )) %>%
  mutate(fp = fct_relevel(fp, c("low", "medium", "high")))

eq_pop_size_sub <- eq_pop_size %>%
  filter(eggs == "low") %>%
  filter(mpa_size == 4) %>%
  filter(mpa_spacing == 0) %>%
  mutate(fp = case_when(
    fp == "med" ~ "medium",
    TRUE ~ fp
  )) %>%
  mutate(fp = as.factor(fp)) %>%
  mutate(fp = fct_relevel(fp, c("low", "medium", "high"))) %>%
  mutate(move_cat = fct_relevel(
    move_cat,
    "low / low", "low / medium", "low / high",
    "medium / low", "medium / medium", "medium / high",
    "high / low", "high / medium", "high / high"
  ))

p1 <- ggplot(sub_connect) +
  geom_hline(aes(yintercept = 0.5), color = "red", alpha = 0.5, linetype = "dashed", linewidth = 1) +
  # geom_rect(aes(xmin = 0.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  # geom_rect(aes(xmin = 4.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  # geom_rect(aes(xmin = 6.5, xmax = 9.5, ymin = 0, ymax = 16, color = "Retention"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  geom_point(aes(move_cat, relative_mpa_abs, color = "Retention + Import"), size = 4) +
  geom_point(aes(move_cat, relative_mpa_R, color = "Retention"), size = 3) +
  # geom_point(aes(move_cat, relative_mpa_I, color = "Import"), size = 3) +
  geom_point(aes(move_cat, relative_mpa_E, color = "Export"), size = 3) +
  theme_bw(base_size = 16) +
  facet_grid(~fp) +
  theme(
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(fill = "white"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  ) +
  labs(
    x = "Movement (Adult / Larval)",
    y = "Adult Proportion of Total Settlers ",
    color = ""
  ) +
  scale_color_manual(values = colors)

p2 <- ggplot(eq_pop_size_sub) +
  geom_point(aes(move_cat, in_out, color = fp), size = 3) +
  theme_bw(base_size = 16) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  ) +
  scale_color_viridis_d(end = 0.9) +
  labs(
    x = "Movement (Adult / Larval)",
    y = "Log(In/Out)",
    color = "Fishing"
  )

ggsave(p1 / p2, path = here::here("figs"), file = paste0("figS3.pdf"), height = 12, width = 8)

sub_connect <- connect %>%
  filter(eggs == "low") %>%
  filter(mpa_size == 4) %>%
  filter(mpa_spacing == 2) %>%
  mutate(fp = case_when(
    fp == "med" ~ "medium",
    TRUE ~ fp
  )) %>%
  mutate(fp = fct_relevel(fp, c("low", "medium", "high")))

eq_pop_size_sub <- eq_pop_size %>%
  filter(eggs == "low") %>%
  filter(mpa_size == 4) %>%
  filter(mpa_spacing == 2) %>%
  mutate(fp = case_when(
    fp == "med" ~ "medium",
    TRUE ~ fp
  )) %>%
  mutate(fp = as.factor(fp)) %>%
  mutate(fp = fct_relevel(fp, c("low", "medium", "high"))) %>%
  mutate(move_cat = fct_relevel(
    move_cat,
    "low / low", "low / medium", "low / high",
    "medium / low", "medium / medium", "medium / high",
    "high / low", "high / medium", "high / high"
  ))

p1 <- ggplot(sub_connect) +
  geom_hline(aes(yintercept = 0.5), color = "red", alpha = 0.5, linetype = "dashed", linewidth = 1) +
  # geom_rect(aes(xmin = 0.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  # geom_rect(aes(xmin = 4.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  # geom_rect(aes(xmin = 6.5, xmax = 9.5, ymin = 0, ymax = 16, color = "Retention"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  geom_point(aes(move_cat, relative_mpa_abs, color = "Retention + Import"), size = 4) +
  geom_point(aes(move_cat, relative_mpa_R, color = "Retention"), size = 3) +
  geom_point(aes(move_cat, relative_mpa_I, color = "Import"), size = 3) +
  geom_point(aes(move_cat, relative_mpa_E, color = "Export"), size = 3) +
  theme_bw(base_size = 16) +
  facet_grid(~fp) +
  theme(
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(fill = "white"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  ) +
  labs(
    x = "Movement (Adult / Larval)",
    y = "Adult Proportion of Total Settlers ",
    color = ""
  ) +
  scale_color_manual(values = colors)

p2 <- ggplot(eq_pop_size_sub) +
  geom_point(aes(move_cat, in_out, color = fp), size = 3) +
  theme_bw(base_size = 16) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  ) +
  scale_color_viridis_d(end = 0.9) +
  labs(
    x = "Movement (Adult / Larval)",
    y = "Log(In/Out)",
    color = "Fishing"
  )

ggsave(p1 / p2, path = here::here("figs"), file = paste0("figS4.pdf"), height = 12, width = 8)

sub_connect <- connect %>%
  filter(eggs == "low") %>%
  filter(mpa_size == 4) %>%
  filter(mpa_spacing == 4) %>%
  mutate(fp = case_when(
    fp == "med" ~ "medium",
    TRUE ~ fp
  )) %>%
  mutate(fp = fct_relevel(fp, c("low", "medium", "high")))

eq_pop_size_sub <- eq_pop_size %>%
  filter(eggs == "low") %>%
  filter(mpa_size == 4) %>%
  filter(mpa_spacing == 4) %>%
  mutate(fp = case_when(
    fp == "med" ~ "medium",
    TRUE ~ fp
  )) %>%
  mutate(fp = as.factor(fp)) %>%
  mutate(fp = fct_relevel(fp, c("low", "medium", "high"))) %>%
  mutate(move_cat = fct_relevel(
    move_cat,
    "low / low", "low / medium", "low / high",
    "medium / low", "medium / medium", "medium / high",
    "high / low", "high / medium", "high / high"
  ))

p1 <- ggplot(sub_connect) +
  geom_hline(aes(yintercept = 0.5), color = "red", alpha = 0.5, linetype = "dashed", linewidth = 1) +
  # geom_rect(aes(xmin = 0.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  # geom_rect(aes(xmin = 4.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  # geom_rect(aes(xmin = 6.5, xmax = 9.5, ymin = 0, ymax = 16, color = "Retention"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  geom_point(aes(move_cat, relative_mpa_abs, color = "Retention + Import"), size = 4) +
  geom_point(aes(move_cat, relative_mpa_R, color = "Retention"), size = 3) +
  geom_point(aes(move_cat, relative_mpa_I, color = "Import"), size = 3) +
  geom_point(aes(move_cat, relative_mpa_E, color = "Export"), size = 3) +
  theme_bw(base_size = 16) +
  facet_grid(~fp) +
  theme(
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(fill = "white"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  ) +
  labs(
    x = "Movement (Adult / Larval)",
    y = "Adult Proportion of Total Settlers ",
    color = ""
  ) +
  scale_color_manual(values = colors)

p2 <- ggplot(eq_pop_size_sub) +
  geom_point(aes(move_cat, in_out, color = fp), size = 3) +
  theme_bw(base_size = 16) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  ) +
  scale_color_viridis_d(end = 0.9) +
  labs(
    x = "Movement (Adult / Larval)",
    y = "Log(In/Out)",
    color = "Fishing"
  )

ggsave(p1 / p2, path = here::here("figs"), file = paste0("figS5.pdf"), height = 12, width = 8)

sub_connect <- connect %>%
  filter(eggs == "low") %>%
  filter(mpa_size == 4) %>%
  filter(mpa_spacing == 8) %>%
  mutate(fp = case_when(
    fp == "med" ~ "medium",
    TRUE ~ fp
  )) %>%
  mutate(fp = fct_relevel(fp, c("low", "medium", "high")))

eq_pop_size_sub <- eq_pop_size %>%
  filter(eggs == "low") %>%
  filter(mpa_size == 4) %>%
  filter(mpa_spacing == 8) %>%
  mutate(fp = case_when(
    fp == "med" ~ "medium",
    TRUE ~ fp
  )) %>%
  mutate(fp = as.factor(fp)) %>%
  mutate(fp = fct_relevel(fp, c("low", "medium", "high"))) %>%
  mutate(move_cat = fct_relevel(
    move_cat,
    "low / low", "low / medium", "low / high",
    "medium / low", "medium / medium", "medium / high",
    "high / low", "high / medium", "high / high"
  ))

p1 <- ggplot(sub_connect) +
  geom_hline(aes(yintercept = 0.5), color = "red", alpha = 0.5, linetype = "dashed", linewidth = 1) +
  # geom_rect(aes(xmin = 0.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  # geom_rect(aes(xmin = 4.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  # geom_rect(aes(xmin = 6.5, xmax = 9.5, ymin = 0, ymax = 16, color = "Retention"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  geom_point(aes(move_cat, relative_mpa_abs, color = "Retention + Import"), size = 4) +
  geom_point(aes(move_cat, relative_mpa_R, color = "Retention"), size = 3) +
  geom_point(aes(move_cat, relative_mpa_I, color = "Import"), size = 3) +
  geom_point(aes(move_cat, relative_mpa_E, color = "Export"), size = 3) +
  theme_bw(base_size = 16) +
  facet_grid(~fp) +
  theme(
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(fill = "white"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  ) +
  labs(
    x = "Movement (Adult / Larval)",
    y = "Adult Proportion of Total Settlers ",
    color = ""
  ) +
  scale_color_manual(values = colors)

p2 <- ggplot(eq_pop_size_sub) +
  geom_point(aes(move_cat, in_out, color = fp), size = 3) +
  theme_bw(base_size = 16) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  ) +
  scale_color_viridis_d(end = 0.9) +
  labs(
    x = "Movement (Adult / Larval)",
    y = "Log(In/Out)",
    color = "Fishing"
  )

ggsave(p1 / p2, path = here::here("figs"), file = paste0("figS6.pdf"), height = 12, width = 8)

sub_connect <- connect %>%
  filter(eggs == "low") %>%
  filter(mpa_size == 4) %>%
  filter(mpa_spacing == 16) %>%
  mutate(fp = case_when(
    fp == "med" ~ "medium",
    TRUE ~ fp
  )) %>%
  mutate(fp = fct_relevel(fp, c("low", "medium", "high")))

eq_pop_size_sub <- eq_pop_size %>%
  filter(eggs == "low") %>%
  filter(mpa_size == 4) %>%
  filter(mpa_spacing == 16) %>%
  mutate(fp = case_when(
    fp == "med" ~ "medium",
    TRUE ~ fp
  )) %>%
  mutate(fp = as.factor(fp)) %>%
  mutate(fp = fct_relevel(fp, c("low", "medium", "high"))) %>%
  mutate(move_cat = fct_relevel(
    move_cat,
    "low / low", "low / medium", "low / high",
    "medium / low", "medium / medium", "medium / high",
    "high / low", "high / medium", "high / high"
  ))

p1 <- ggplot(sub_connect) +
  geom_hline(aes(yintercept = 0.5), color = "red", alpha = 0.5, linetype = "dashed", linewidth = 1) +
  # geom_rect(aes(xmin = 0.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  # geom_rect(aes(xmin = 4.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  # geom_rect(aes(xmin = 6.5, xmax = 9.5, ymin = 0, ymax = 16, color = "Retention"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  geom_point(aes(move_cat, relative_mpa_abs, color = "Retention + Import"), size = 4) +
  geom_point(aes(move_cat, relative_mpa_R, color = "Retention"), size = 3) +
  geom_point(aes(move_cat, relative_mpa_I, color = "Import"), size = 3) +
  geom_point(aes(move_cat, relative_mpa_E, color = "Export"), size = 3) +
  theme_bw(base_size = 16) +
  facet_grid(~fp) +
  theme(
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(fill = "white"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  ) +
  labs(
    x = "Movement (Adult / Larval)",
    y = "Adult Proportion of Total Settlers ",
    color = ""
  ) +
  scale_color_manual(values = colors)

p2 <- ggplot(eq_pop_size_sub) +
  geom_point(aes(move_cat, in_out, color = fp), size = 3) +
  theme_bw(base_size = 16) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  ) +
  scale_color_viridis_d(end = 0.9) +
  labs(
    x = "Movement (Adult / Larval)",
    y = "Log(In/Out)",
    color = "Fishing"
  )

ggsave(p1 / p2, path = here::here("figs"), file = paste0("figS7.pdf"), height = 12, width = 8)

sub_connect <- connect %>%
  filter(eggs == "low") %>%
  filter(mpa_size == 8) %>%
  filter(mpa_spacing == 0) %>%
  mutate(fp = case_when(
    fp == "med" ~ "medium",
    TRUE ~ fp
  )) %>%
  mutate(fp = fct_relevel(fp, c("low", "medium", "high")))

eq_pop_size_sub <- eq_pop_size %>%
  filter(eggs == "low") %>%
  filter(mpa_size == 8) %>%
  filter(mpa_spacing == 0) %>%
  mutate(fp = case_when(
    fp == "med" ~ "medium",
    TRUE ~ fp
  )) %>%
  mutate(fp = as.factor(fp)) %>%
  mutate(fp = fct_relevel(fp, c("low", "medium", "high"))) %>%
  mutate(move_cat = fct_relevel(
    move_cat,
    "low / low", "low / medium", "low / high",
    "medium / low", "medium / medium", "medium / high",
    "high / low", "high / medium", "high / high"
  ))

p1 <- ggplot(sub_connect) +
  geom_hline(aes(yintercept = 0.5), color = "red", alpha = 0.5, linetype = "dashed", linewidth = 1) +
  # geom_rect(aes(xmin = 0.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  # geom_rect(aes(xmin = 4.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  # geom_rect(aes(xmin = 6.5, xmax = 9.5, ymin = 0, ymax = 16, color = "Retention"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  geom_point(aes(move_cat, relative_mpa_abs, color = "Retention + Import"), size = 4) +
  geom_point(aes(move_cat, relative_mpa_R, color = "Retention"), size = 3) +
  # geom_point(aes(move_cat, relative_mpa_I, color = "Import"), size = 3) +
  geom_point(aes(move_cat, relative_mpa_E, color = "Export"), size = 3) +
  theme_bw(base_size = 16) +
  facet_grid(~fp) +
  theme(
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(fill = "white"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  ) +
  labs(
    x = "Movement (Adult / Larval)",
    y = "Adult Proportion of Total Settlers ",
    color = ""
  ) +
  scale_color_manual(values = colors)

p2 <- ggplot(eq_pop_size_sub) +
  geom_point(aes(move_cat, in_out, color = fp), size = 3) +
  theme_bw(base_size = 16) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  ) +
  scale_color_viridis_d(end = 0.9) +
  labs(
    x = "Movement (Adult / Larval)",
    y = "Log(In/Out)",
    color = "Fishing"
  )

ggsave(p1 / p2, path = here::here("figs"), file = paste0("figS8.pdf"), height = 12, width = 8)

sub_connect <- connect %>%
  filter(eggs == "low") %>%
  filter(mpa_size == 8) %>%
  filter(mpa_spacing == 2) %>%
  mutate(fp = case_when(
    fp == "med" ~ "medium",
    TRUE ~ fp
  )) %>%
  mutate(fp = fct_relevel(fp, c("low", "medium", "high")))

eq_pop_size_sub <- eq_pop_size %>%
  filter(eggs == "low") %>%
  filter(mpa_size == 8) %>%
  filter(mpa_spacing == 2) %>%
  mutate(fp = case_when(
    fp == "med" ~ "medium",
    TRUE ~ fp
  )) %>%
  mutate(fp = as.factor(fp)) %>%
  mutate(fp = fct_relevel(fp, c("low", "medium", "high"))) %>%
  mutate(move_cat = fct_relevel(
    move_cat,
    "low / low", "low / medium", "low / high",
    "medium / low", "medium / medium", "medium / high",
    "high / low", "high / medium", "high / high"
  ))

p1 <- ggplot(sub_connect) +
  geom_hline(aes(yintercept = 0.5), color = "red", alpha = 0.5, linetype = "dashed", linewidth = 1) +
  # geom_rect(aes(xmin = 0.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  # geom_rect(aes(xmin = 4.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  # geom_rect(aes(xmin = 6.5, xmax = 9.5, ymin = 0, ymax = 16, color = "Retention"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  geom_point(aes(move_cat, relative_mpa_abs, color = "Retention + Import"), size = 4) +
  geom_point(aes(move_cat, relative_mpa_R, color = "Retention"), size = 3) +
  geom_point(aes(move_cat, relative_mpa_I, color = "Import"), size = 3) +
  geom_point(aes(move_cat, relative_mpa_E, color = "Export"), size = 3) +
  theme_bw(base_size = 16) +
  facet_grid(~fp) +
  theme(
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(fill = "white"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  ) +
  labs(
    x = "Movement (Adult / Larval)",
    y = "Adult Proportion of Total Settlers ",
    color = ""
  ) +
  scale_color_manual(values = colors)

p2 <- ggplot(eq_pop_size_sub) +
  geom_point(aes(move_cat, in_out, color = fp), size = 3) +
  theme_bw(base_size = 16) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  ) +
  scale_color_viridis_d(end = 0.9) +
  labs(
    x = "Movement (Adult / Larval)",
    y = "Log(In/Out)",
    color = "Fishing"
  )

ggsave(p1 / p2, path = here::here("figs"), file = paste0("figS9.pdf"), height = 12, width = 8)

sub_connect <- connect %>%
  filter(eggs == "low") %>%
  filter(mpa_size == 8) %>%
  filter(mpa_spacing == 4) %>%
  mutate(fp = case_when(
    fp == "med" ~ "medium",
    TRUE ~ fp
  )) %>%
  mutate(fp = fct_relevel(fp, c("low", "medium", "high")))

eq_pop_size_sub <- eq_pop_size %>%
  filter(eggs == "low") %>%
  filter(mpa_size == 8) %>%
  filter(mpa_spacing == 4) %>%
  mutate(fp = case_when(
    fp == "med" ~ "medium",
    TRUE ~ fp
  )) %>%
  mutate(fp = as.factor(fp)) %>%
  mutate(fp = fct_relevel(fp, c("low", "medium", "high"))) %>%
  mutate(move_cat = fct_relevel(
    move_cat,
    "low / low", "low / medium", "low / high",
    "medium / low", "medium / medium", "medium / high",
    "high / low", "high / medium", "high / high"
  ))

p1 <- ggplot(sub_connect) +
  geom_hline(aes(yintercept = 0.5), color = "red", alpha = 0.5, linetype = "dashed", linewidth = 1) +
  # geom_rect(aes(xmin = 0.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  # geom_rect(aes(xmin = 4.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  # geom_rect(aes(xmin = 6.5, xmax = 9.5, ymin = 0, ymax = 16, color = "Retention"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  geom_point(aes(move_cat, relative_mpa_abs, color = "Retention + Import"), size = 4) +
  geom_point(aes(move_cat, relative_mpa_R, color = "Retention"), size = 3) +
  geom_point(aes(move_cat, relative_mpa_I, color = "Import"), size = 3) +
  geom_point(aes(move_cat, relative_mpa_E, color = "Export"), size = 3) +
  theme_bw(base_size = 16) +
  facet_grid(~fp) +
  theme(
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(fill = "white"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  ) +
  labs(
    x = "Movement (Adult / Larval)",
    y = "Adult Proportion of Total Settlers ",
    color = ""
  ) +
  scale_color_manual(values = colors)

p2 <- ggplot(eq_pop_size_sub) +
  geom_point(aes(move_cat, in_out, color = fp), size = 3) +
  theme_bw(base_size = 16) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  ) +
  scale_color_viridis_d(end = 0.9) +
  labs(
    x = "Movement (Adult / Larval)",
    y = "Log(In/Out)",
    color = "Fishing"
  )

ggsave(p1 / p2, path = here::here("figs"), file = paste0("figS10.pdf"), height = 12, width = 8)

sub_connect <- connect %>%
  filter(eggs == "low") %>%
  filter(mpa_size == 8) %>%
  filter(mpa_spacing == 8) %>%
  mutate(fp = case_when(
    fp == "med" ~ "medium",
    TRUE ~ fp
  )) %>%
  mutate(fp = fct_relevel(fp, c("low", "medium", "high")))

eq_pop_size_sub <- eq_pop_size %>%
  filter(eggs == "low") %>%
  filter(mpa_size == 8) %>%
  filter(mpa_spacing == 8) %>%
  mutate(fp = case_when(
    fp == "med" ~ "medium",
    TRUE ~ fp
  )) %>%
  mutate(fp = as.factor(fp)) %>%
  mutate(fp = fct_relevel(fp, c("low", "medium", "high"))) %>%
  mutate(move_cat = fct_relevel(
    move_cat,
    "low / low", "low / medium", "low / high",
    "medium / low", "medium / medium", "medium / high",
    "high / low", "high / medium", "high / high"
  ))

p1 <- ggplot(sub_connect) +
  geom_hline(aes(yintercept = 0.5), color = "red", alpha = 0.5, linetype = "dashed", linewidth = 1) +
  # geom_rect(aes(xmin = 0.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  # geom_rect(aes(xmin = 4.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  # geom_rect(aes(xmin = 6.5, xmax = 9.5, ymin = 0, ymax = 16, color = "Retention"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  geom_point(aes(move_cat, relative_mpa_abs, color = "Retention + Import"), size = 4) +
  geom_point(aes(move_cat, relative_mpa_R, color = "Retention"), size = 3) +
  geom_point(aes(move_cat, relative_mpa_I, color = "Import"), size = 3) +
  geom_point(aes(move_cat, relative_mpa_E, color = "Export"), size = 3) +
  theme_bw(base_size = 16) +
  facet_grid(~fp) +
  theme(
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(fill = "white"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  ) +
  labs(
    x = "Movement (Adult / Larval)",
    y = "Adult Proportion of Total Settlers ",
    color = ""
  ) +
  scale_color_manual(values = colors)

p2 <- ggplot(eq_pop_size_sub) +
  geom_point(aes(move_cat, in_out, color = fp), size = 3) +
  theme_bw(base_size = 16) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  ) +
  scale_color_viridis_d(end = 0.9) +
  labs(
    x = "Movement (Adult / Larval)",
    y = "Log(In/Out)",
    color = "Fishing"
  )

ggsave(p1 / p2, path = here::here("figs"), file = paste0("figS11.pdf"), height = 12, width = 8)

sub_connect <- connect %>%
  filter(eggs == "low") %>%
  filter(mpa_size == 8) %>%
  filter(mpa_spacing == 16) %>%
  mutate(fp = case_when(
    fp == "med" ~ "medium",
    TRUE ~ fp
  )) %>%
  mutate(fp = fct_relevel(fp, c("low", "medium", "high")))

eq_pop_size_sub <- eq_pop_size %>%
  filter(eggs == "low") %>%
  filter(mpa_size == 8) %>%
  filter(mpa_spacing == 16) %>%
  mutate(fp = case_when(
    fp == "med" ~ "medium",
    TRUE ~ fp
  )) %>%
  mutate(fp = as.factor(fp)) %>%
  mutate(fp = fct_relevel(fp, c("low", "medium", "high"))) %>%
  mutate(move_cat = fct_relevel(
    move_cat,
    "low / low", "low / medium", "low / high",
    "medium / low", "medium / medium", "medium / high",
    "high / low", "high / medium", "high / high"
  ))

p1 <- ggplot(sub_connect) +
  geom_hline(aes(yintercept = 0.5), color = "red", alpha = 0.5, linetype = "dashed", linewidth = 1) +
  # geom_rect(aes(xmin = 0.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  # geom_rect(aes(xmin = 4.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  # geom_rect(aes(xmin = 6.5, xmax = 9.5, ymin = 0, ymax = 16, color = "Retention"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  geom_point(aes(move_cat, relative_mpa_abs, color = "Retention + Import"), size = 4) +
  geom_point(aes(move_cat, relative_mpa_R, color = "Retention"), size = 3) +
  geom_point(aes(move_cat, relative_mpa_I, color = "Import"), size = 3) +
  geom_point(aes(move_cat, relative_mpa_E, color = "Export"), size = 3) +
  theme_bw(base_size = 16) +
  facet_grid(~fp) +
  theme(
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(fill = "white"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  ) +
  labs(
    x = "Movement (Adult / Larval)",
    y = "Adult Proportion of Total Settlers ",
    color = ""
  ) +
  scale_color_manual(values = colors)

p2 <- ggplot(eq_pop_size_sub) +
  geom_point(aes(move_cat, in_out, color = fp), size = 3) +
  theme_bw(base_size = 16) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  ) +
  scale_color_viridis_d(end = 0.9) +
  labs(
    x = "Movement (Adult / Larval)",
    y = "Log(In/Out)",
    color = "Fishing"
  )

ggsave(p1 / p2, path = here::here("figs"), file = paste0("figS12.pdf"), height = 12, width = 8)

sub_connect <- connect %>%
  filter(eggs == "low") %>%
  filter(mpa_size == 16) %>%
  filter(mpa_spacing == 0) %>%
  mutate(fp = case_when(
    fp == "med" ~ "medium",
    TRUE ~ fp
  )) %>%
  mutate(fp = fct_relevel(fp, c("low", "medium", "high")))

eq_pop_size_sub <- eq_pop_size %>%
  filter(eggs == "low") %>%
  filter(mpa_size == 16) %>%
  filter(mpa_spacing == 0) %>%
  mutate(fp = case_when(
    fp == "med" ~ "medium",
    TRUE ~ fp
  )) %>%
  mutate(fp = as.factor(fp)) %>%
  mutate(fp = fct_relevel(fp, c("low", "medium", "high"))) %>%
  mutate(move_cat = fct_relevel(
    move_cat,
    "low / low", "low / medium", "low / high",
    "medium / low", "medium / medium", "medium / high",
    "high / low", "high / medium", "high / high"
  ))

p1 <- ggplot(sub_connect) +
  geom_hline(aes(yintercept = 0.5), color = "red", alpha = 0.5, linetype = "dashed", linewidth = 1) +
  # geom_rect(aes(xmin = 0.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  # geom_rect(aes(xmin = 4.5, xmax = 5.5, ymin = 0, ymax = 16, color = "Export"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  # geom_rect(aes(xmin = 6.5, xmax = 9.5, ymin = 0, ymax = 16, color = "Retention"), fill = "transparent", linetype = "dashed", linewidth = 0.2) +
  geom_point(aes(move_cat, relative_mpa_abs, color = "Retention + Import"), size = 4) +
  geom_point(aes(move_cat, relative_mpa_R, color = "Retention"), size = 3) +
  # geom_point(aes(move_cat, relative_mpa_I, color = "Import"), size = 3) +
  geom_point(aes(move_cat, relative_mpa_E, color = "Export"), size = 3) +
  theme_bw(base_size = 16) +
  facet_grid(~fp) +
  theme(
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(fill = "white"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  ) +
  labs(
    x = "Movement (Adult / Larval)",
    y = "Adult Proportion of Total Settlers ",
    color = ""
  ) +
  scale_color_manual(values = colors)

p2 <- ggplot(eq_pop_size_sub) +
  geom_point(aes(move_cat, in_out, color = fp), size = 3) +
  theme_bw(base_size = 16) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  ) +
  scale_color_viridis_d(end = 0.9) +
  labs(
    x = "Movement (Adult / Larval)",
    y = "Log(In/Out)",
    color = "Fishing"
  )

ggsave(p1 / p2, path = here::here("figs"), file = paste0("figS13.pdf"), height = 12, width = 8)
