# Load necessary packages 
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyr,
               dplyr, 
               stats,
               vars, 
               ggplot2,
               readr,
               rio,
               lpirfs,
               panelvar) 

# `scale_irfs()`
# Scales input `lpirfs_lin_panel_obj` IRF
# point estimates and CIs by a scaling factor.
scale_irfs <- function(irf_obj, scale_factor){
  
  irf_obj[1][[1]] <- irf_obj[1][[1]] * scale_factor
  irf_obj[2][[1]] <- irf_obj[2][[1]] * scale_factor
  irf_obj[3][[1]] <- irf_obj[3][[1]] * scale_factor
  
  return(irf_obj)
}