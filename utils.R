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
               panelvar,
               svglite) 

# `scale_irfs()`
# Scales input `lpirfs_lin_panel_obj` IRF
# point estimates and CIs by a scaling factor.
scale_irfs <- function(irf_obj, scale_factor){
  
  irf_obj[1][[1]] <- irf_obj[1][[1]] * scale_factor
  irf_obj[2][[1]] <- irf_obj[2][[1]] * scale_factor
  irf_obj[3][[1]] <- irf_obj[3][[1]] * scale_factor
  
  return(irf_obj)
}

# `%!in%`
# Operator that returns TRUE when value
# does not match array of inputs
'%!in%' <- function(x,y)!('%in%'(x,y)) 

# `sims`
# Sims counterfactual
# given `lprifs_lin_panel_obj`
# sequence of counterfactual shocks
# and horizon length (+1)
sims <- function(fig, xpath, horizon){
  
  response_sims <- 0 * c(1:horizon)
  
  for(i in 1:horizon){
    if (i == 1){
      response_temp <- fig$irf_panel_mean |> as.vector() 
      response_sims <- xpath[i] * response_temp
    }else{
      response_temp <- fig$irf_panel_mean |> as.vector() |> lag(i-1)
      response_temp <- c(0*c(1:(i-1)),response_temp[i:h])
      response_sims <- xpath[i] * response_temp + response_sims
    }
  }
  
  return(response_sims)
}

# Create standard and cumulative IRF dataframe for easy plotting
create_irf_df <- function(reg_irf, horizon){
  
  temp_df <- data.frame(horizon = c(0:horizon),
                        irf = reg_irf |> as.vector(),
                        cirf = reg_irf |> as.vector() |> cumsum()) |> 
    pivot_longer(
      cols = c("irf","cirf"), 
      names_to = "response_type",
      values_to = "response"
    ) |>
    mutate(response_type = ifelse(response_type=="irf", "Standard IRF", "Cumulative IRF"))
  
  return(temp_df)
}