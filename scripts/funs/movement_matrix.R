library(tidyverse)
library(pracma)

source(here::here("scripts", "funs", "prep_movement.R"))

diffusion_prep <- function(x,y, time_step, patch_area){
  
  x[!is.na(x)] <- 1 # this is here to allow for barriers; set diffusion to zero if there's a physical barrier
  
  z <- x * y * (time_step / patch_area)
}

# function ----------------------------------------------------------------
#' Create connectivity matrix
#'
#' @param time_step time step in question
#' @param resolution spatial resolution
#' @param habitat 
#'
#' @return the connectivity matrix
#' @export
#'

movement_matrix <- function(time_step, resolution, habitat) {
  
  taxis_matrix <- list(habitat)
  
  # reshape to vector, for some reason doesn't work inside function
  for (i in seq_along(taxis_matrix)) {
    taxis_matrix[[i]] <- tidyr::pivot_longer(as.data.frame(taxis_matrix[[i]]), tidyr::everything()) # need to use pivot_longer to match patch order from expand_grid
    
    taxis_matrix[[i]] <- as.numeric(taxis_matrix[[i]]$value)
    
    taxis_matrix[[i]] <- pmin(exp((time_step * outer(taxis_matrix[[i]], taxis_matrix[[i]], "-")) / sqrt(patch_area)),max_hab_mult) # convert habitat gradient into diffusion multiplier
    
  }
  
  diffusion_foundation <- purrr::map2(taxis_matrix, adult_diffusion,diffusion_prep, time_step = time_step, patch_area = patch_area) # prepare adult diffusion matrix account for potential land
  
  diffusion_and_taxis <- purrr::map2(diffusion_foundation, taxis_matrix, ~ .x * .y)
  
  inst_movement_matrix <-  purrr::pmap(list(multiplier = diffusion_and_taxis),
                                       prep_movement,
                                       resolution = resolution)
  
  movement_matrix <-
    purrr::map(inst_movement_matrix,
               ~ as.matrix(expm::expm((.x))))
  
  movement_matrix = movement_matrix[[1]]
  
  movement_matrix_reshape = array(0, dim=c(resolution[1],resolution[2],nrow(movement_matrix)))
  
  for (i in 1:nrow(movement_matrix)) {
    movement_matrix_reshape[, ,i] = t(Reshape(movement_matrix[, i], resolution[1], resolution[2]))
  }
  
  return(movement_matrix_reshape)
}
