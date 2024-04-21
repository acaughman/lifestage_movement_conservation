library(tidyverse)
library(patchwork)
library(pracma)
library(pheatmap)

resolution <- c(50, 50)

world = array(1:2500, c(resolution))

load(here::here("outputs", "adult_diffusion_32.rda")) 
ad = adult_mm
rm(adult_mm)

# pheatmap(ad, cluster_rows = FALSE, cluster_cols = FALSE)

ad_df <- array(0, dim = c(resolution[1], resolution[2], nrow(ad)))

for (i in 1:nrow(ad)) {
  ad_df[, , i] <- t(Reshape(ad[, i], resolution[1], resolution[2]))
}

load(here::here("outputs", "recruit_diffusion_32.rda")) 
ld = recruit_mm
rm(recruit_mm)

# pheatmap(ld, cluster_rows = FALSE, cluster_cols = FALSE)

ld_df <- array(0, dim = c(resolution[1], resolution[2], nrow(ld)))

for (i in 1:nrow(ld)) {
  ld_df[, , i] <- t(Reshape(ld[, i], resolution[1], resolution[2]))
}

# f_mort[22:29, 9:17, ] <- 1 # size 8x8, spacing 16
# f_mort[22:29, 34:41, ] <- 1 # size 8x8, spacing 16

map1 = as.matrix(ld_df[, , 1213])
map2 = as.matrix(ad_df[, , 1213])

pheatmap(map1, cluster_rows = FALSE, cluster_cols = FALSE)
pheatmap(map2, cluster_rows = FALSE, cluster_cols = FALSE)
