# Load packages & custom utils
source("utils.R")

#--------------------------------
# Prepare Data 

## Read csv
df <- read_csv("data/ctax_AEJM.csv")

## Keep 1985-2018 obs.
df <- df |> filter(year >= 1985,
                   year <= 2018)

## Create new variables
df <- df |>
  mutate(ctaxno = ifelse(is.na(ctaxyear), 1, 0)) |>
  mutate(ctaxpre = ifelse(year < ctaxyear, 1-ctaxno, 0)) |>
  mutate(rater_LCU_USD18sw = rater_LCU_USD18 * share19) |>
  mutate(ecp2018sw = ecp_ghg_tax_usd_2018) |>
  mutate(rater_LCU_USD18sw = ifelse(is.na(rater_LCU_USD18sw), 0, rater_LCU_USD18sw))

## Drop (set to `NA`) emission data prior to 1990
df <- df |>
  mutate(emission_ctsectors = ifelse(year < 1990, NA, emission_ctsectors)) |>
  mutate(lemission_ctsectors = ifelse(year < 1990, NA, lemission_ctsectors))

## Set base year for `pgdp` at 2018
## and generate log-diffed version of `pgdp`
df <- df |>
  group_by(country) |>
  mutate(pindex = ifelse(year == 2018, pgdp, 0)) |>
  mutate(pindex = max(pindex)) |>
  mutate(pindex = 100 * pgdp / pindex) |>
  mutate(dlpgdp = 100 * log(pindex/lag(pindex))) |>
  ungroup() |>
  dplyr::select(-pindex)

## Create differenced version of treatment
df <- df |>
  group_by(country) |>
  mutate(drater_LCU_USD18sw = c(diff(rater_LCU_USD18sw),NA))

## Select relevant variables for Fig 3 LP reg model
df_select <- df |>
  dplyr::transmute(country, 
                   year, 
                   dlrgdp, 
                   dlempman, 
                   dlemptot, 
                   dlpgdp, 
                   dlemission_ctsectors,
                   lrgdp, 
                   lempman, 
                   lemptot, 
                   pgdp,
                   lemission_ctsectors, 
                   rater_LCU_USD18sw, 
                   drater_LCU_USD18sw)

## Drop Liechtenstein
df_select <- df_select |>
  filter(country != "Liechtenstein")

#--------------------------------
# Set hyperparameters
conf95 = 1.96
conf68 = 1
h = 6 + 1 
lagnum = 4
gamma = 40 * 0.3 #irf scaling factor ($40 * 0.3 share)

#--------------------------------
#--------------------------------
# Fig A4
figa4_diff <- lp_lin_panel(data_set = df_select,
                      data_sample = "Full",
                      endog_data = "drater_LCU_USD18sw",
                      cumul_mult = FALSE,
                      shock = "drater_LCU_USD18sw",
                      diff_shock = FALSE,
                      panel_model = "within",
                      panel_effect = "time",
                      robust_cov = "vcovHC",
                      l_exog_data = c("drater_LCU_USD18sw","dlrgdp","dlemptot", "dlempman", "dlpgdp"),
                      lags_exog_data = lagnum,
                      confint = conf95,
                      hor = h)

plot(figa4_diff)

## Based on FigA4 estimates
## find sequence of carbon tax shocks 
## that cause a permanent increase in tax rate
col1 <- figa4_diff[1][[1]] |> 
  as.vector()
col2 <- figa4_diff[1][[1]] |> 
  as.vector() |> 
  lag(1)
col2 <- c(0,col2[2:h])
col3 <- figa4_diff[1][[1]] |> 
  as.vector() |> 
  lag(2)
col3 <- c(0*c(1:2),col3[3:h])
col4 <- figa4_diff[1][[1]] |> 
  as.vector() |> 
  lag(3)
col4 <- c(0*c(1:3),col4[4:h])
col5 <- figa4_diff[1][[1]] |> 
  as.vector() |> 
  lag(4)
col5 <- c(0*c(1:4),col5[5:h])
col6 <- figa4_diff[1][[1]] |> 
  as.vector() |> 
  lag(5)
col6 <- c(0*c(1:5),col6[6:h])
col7 <- figa4_diff[1][[1]] |> 
  as.vector() |> 
  lag(6)
col7 <- c(0*c(1:6),col7[7:h])
taxirf_mat <- matrix(c(col1,col2,col3,col4,col5,col6,col7), 7, 7)
xpath <- solve(taxirf_mat, c(1,rep(0,6))) # Solve for shock path

figa4_level <- lp_lin_panel(data_set = df_select,
                      data_sample = "Full",
                      endog_data = "rater_LCU_USD18sw",
                      cumul_mult = FALSE,
                      shock = "rater_LCU_USD18sw",
                      diff_shock = FALSE,
                      panel_model = "within",
                      panel_effect = "time",
                      robust_cov = "vcovHC",
                      l_exog_data = c("rater_LCU_USD18sw","dlrgdp","dlemptot", "dlempman", "dlpgdp"),
                      lags_exog_data = lagnum,
                      confint = conf95,
                      hor = h)

plot(figa4_level)

#--------------------------------
#--------------------------------
# Fig 3A: Effect of carbon tax on GDP growth (LP regression in Eq. (1); unrestricted)
fig3a <- lp_lin_panel(data_set = df_select,
                              data_sample = "Full",
                              endog_data = "dlrgdp",
                              cumul_mult = FALSE,
                              shock = "rater_LCU_USD18sw",
                              diff_shock = FALSE,
                              panel_model = "within",
                              panel_effect = "time",
                              robust_cov = "vcovHC",
                              l_exog_data = c("rater_LCU_USD18sw","dlrgdp","dlemptot", "dlempman", "dlpgdp"),
                              lags_exog_data = lagnum,
                              confint = conf95,
                              hor = h) |>
  scale_irfs(gamma)

plot(fig3a)

## Compute Sims point estimates
col1 <- fig3a[1][[1]] |> 
  as.vector()
col2 <- fig3a[1][[1]] |> 
  as.vector() |> 
  lag(1)
col2 <- c(0,col2[2:h])
col3 <- fig3a[1][[1]] |> 
  as.vector() |> 
  lag(2)
col3 <- c(0*c(1:2),col3[3:h])
col4 <- fig3a[1][[1]] |> 
  as.vector() |> 
  lag(3)
col4 <- c(0*c(1:3),col4[4:h])
col5 <- fig3a[1][[1]] |> 
  as.vector() |> 
  lag(4)
col5 <- c(0*c(1:4),col5[5:h])
col6 <- fig3a[1][[1]] |> 
  as.vector() |> 
  lag(5)
col6 <- c(0*c(1:5),col6[6:h])
col7 <- fig3a[1][[1]] |> 
  as.vector() |> 
  lag(6)
col7 <- c(0*c(1:6),col7[7:h])
response_sims <- xpath[1] * col1 +
  xpath[2] * col2 +
  xpath[3] * col3 +
  xpath[4] * col4 +
  xpath[5] * col5 +
  xpath[6] * col6 + 
  xpath[7] * col7

# Fig 3B: Effect of carbon tax on GDP growth (LP regression in Eq. (2); restricted)
fig3b <- lp_lin_panel(data_set = df_select,
                              data_sample = "Full",
                              endog_data = "dlrgdp",
                              cumul_mult = FALSE,
                              shock = "drater_LCU_USD18sw",
                              diff_shock = FALSE,
                              panel_model = "within",
                              panel_effect = "time",
                              robust_cov = "vcovHC",
                              l_exog_data = c("drater_LCU_USD18sw","dlrgdp","dlemptot", "dlempman", "dlpgdp"),
                              lags_exog_data = lagnum,
                              confint = conf95,
                              hor = h) |>
  scale_irfs(gamma)

plot(fig3b)

#--------------------------------
#--------------------------------
# Fig 4A: Effect of carbon tax on GDP growth (bivariate LP regression in Eq. (1); restricted)
fig4a <- lp_lin_panel(data_set = df_select,
                      data_sample = "Full",
                      endog_data = "dlrgdp",
                      cumul_mult = FALSE,
                      shock = "drater_LCU_USD18sw",
                      diff_shock = FALSE,
                      panel_model = "within",
                      panel_effect = "time",
                      robust_cov = "vcovHC",
                      l_exog_data = c("drater_LCU_USD18sw","dlrgdp"),
                      lags_exog_data = lagnum,
                      confint = conf95,
                      hor = h) |>
  scale_irfs(gamma)

plot(fig4a)

#--------------------------------
#--------------------------------
# Fig 5A: Effect of carbon tax on GDP level (Cumulative IRF; LP regression in Eq. (1); unrestricted)
fig5a <- lp_lin_panel(data_set = df_select,
                              data_sample = "Full",
                              endog_data = "lrgdp",
                              cumul_mult = FALSE,
                              shock = "rater_LCU_USD18sw",
                              diff_shock = FALSE,
                              panel_model = "within",
                              panel_effect = "time",
                              robust_cov = "vcovHC",
                              l_exog_data = c("rater_LCU_USD18sw","dlrgdp","dlemptot", "dlempman", "dlpgdp"),
                              lags_exog_data = lagnum,
                              confint = conf95,
                              hor = h) |>
  scale_irfs(gamma)

plot(fig5a)

# Fig 5B: Effect of carbon tax on GDP level (Cumulative IRF; LP regression in Eq. (2); restricted)
fig5b <- lp_lin_panel(data_set = df_select,
                              data_sample = "Full",
                              endog_data = "lrgdp",
                              cumul_mult = FALSE,
                              shock = "drater_LCU_USD18sw",
                              diff_shock = FALSE,
                              panel_model = "within",
                              panel_effect = "time",
                              robust_cov = "vcovHC",
                              l_exog_data = c("drater_LCU_USD18sw","dlrgdp","dlemptot", "dlempman", "dlpgdp"),
                              lags_exog_data = lagnum,
                              confint = conf95,
                              hor = h) |>
  scale_irfs(gamma)

plot(fig5b)

#--------------------------------
#--------------------------------
# Fig 6A: Effect of carbon tax on total employment growth (LP regression in Eq. (1); unrestricted)
fig6a <- lp_lin_panel(data_set = df_select,
                              data_sample = "Full",
                              endog_data = "dlemptot",
                              cumul_mult = FALSE,
                              shock = "rater_LCU_USD18sw",
                              diff_shock = FALSE,
                              panel_model = "within",
                              panel_effect = "time",
                              robust_cov = "vcovHC",
                              l_exog_data = c("rater_LCU_USD18sw","dlrgdp","dlemptot", "dlempman", "dlpgdp"),
                              lags_exog_data = lagnum,
                              confint = conf95,
                              hor = h) |>
  scale_irfs(gamma)

plot(fig6a)

# Fig 6B: Effect of carbon tax on total employment growth  (LP regression in Eq. (2); restricted)
fig6b <- lp_lin_panel(data_set = df_select,
                              data_sample = "Full",
                              endog_data = "dlemptot",
                              cumul_mult = FALSE,
                              shock = "drater_LCU_USD18sw",
                              diff_shock = TRUE,
                              panel_model = "within",
                              panel_effect = "time",
                              robust_cov = "vcovHC",
                              l_exog_data = c("drater_LCU_USD18sw","dlrgdp","dlemptot", "dlempman", "dlpgdp"),
                              lags_exog_data = lagnum,
                              confint = conf95,
                              hor = h) |>
  scale_irfs(gamma)

plot(fig6b)

#--------------------------------
#--------------------------------
# Fig 7A: Effect of carbon tax on total employment level (Cumulative IRF; LP regression in Eq. (1); unrestricted)
fig7a <- lp_lin_panel(data_set = df_select,
                      data_sample = "Full",
                      endog_data = "lemptot",
                      cumul_mult = FALSE,
                      shock = "rater_LCU_USD18sw",
                      diff_shock = FALSE,
                      panel_model = "within",
                      panel_effect = "time",
                      robust_cov = "vcovHC",
                      l_exog_data = c("rater_LCU_USD18sw","dlrgdp","dlemptot", "dlempman", "dlpgdp"),
                      lags_exog_data = lagnum,
                      confint = conf95,
                      hor = h) |>
  scale_irfs(gamma)

plot(fig7a)

# Fig 7B: Effect of carbon tax on total employment level (Cumulative IRF; LP regression in Eq. (2); restricted)
fig7b <- lp_lin_panel(data_set = df_select,
                      data_sample = "Full",
                      endog_data = "lemptot",
                      cumul_mult = FALSE,
                      shock = "drater_LCU_USD18sw",
                      diff_shock = FALSE,
                      panel_model = "within",
                      panel_effect = "time",
                      robust_cov = "vcovHC",
                      l_exog_data = c("drater_LCU_USD18sw","dlrgdp","dlemptot", "dlempman", "dlpgdp"),
                      lags_exog_data = lagnum,
                      confint = conf95,
                      hor = h) |>
  scale_irfs(gamma)

plot(fig7b)

#--------------------------------
#--------------------------------
# Fig 8A: Effect of carbon tax on manufacturing employment growth (LP regression in Eq. (1); unrestricted)
fig8a <- lp_lin_panel(data_set = df_select,
                      data_sample = "Full",
                      endog_data = "dlempman",
                      cumul_mult = FALSE,
                      shock = "rater_LCU_USD18sw",
                      diff_shock = FALSE,
                      panel_model = "within",
                      panel_effect = "time",
                      robust_cov = "vcovHC",
                      l_exog_data = c("rater_LCU_USD18sw","dlrgdp","dlemptot", "dlempman", "dlpgdp"),
                      lags_exog_data = lagnum,
                      confint = conf95,
                      hor = h) |>
  scale_irfs(gamma)

plot(fig8a)

# Fig 8B: Effect of carbon tax on manufacturing employment growth  (LP regression in Eq. (2); restricted)
fig8b <- lp_lin_panel(data_set = df_select,
                      data_sample = "Full",
                      endog_data = "dlempman",
                      cumul_mult = FALSE,
                      shock = "drater_LCU_USD18sw",
                      diff_shock = TRUE,
                      panel_model = "within",
                      panel_effect = "time",
                      robust_cov = "vcovHC",
                      l_exog_data = c("drater_LCU_USD18sw","dlrgdp","dlemptot", "dlempman", "dlpgdp"),
                      lags_exog_data = lagnum,
                      confint = conf95,
                      hor = h) |>
  scale_irfs(gamma)

plot(fig8b)

#--------------------------------
#--------------------------------
# Fig 9A: Effect of carbon tax on manufacturing employment level (Cumulative IRF; LP regression in Eq. (1); unrestricted)
fig9a <- lp_lin_panel(data_set = df_select,
                      data_sample = "Full",
                      endog_data = "lempman",
                      cumul_mult = FALSE,
                      shock = "rater_LCU_USD18sw",
                      diff_shock = FALSE,
                      panel_model = "within",
                      panel_effect = "time",
                      robust_cov = "vcovHC",
                      l_exog_data = c("rater_LCU_USD18sw","dlrgdp","dlemptot", "dlempman", "dlpgdp"),
                      lags_exog_data = lagnum,
                      confint = conf95,
                      hor = h) |>
  scale_irfs(gamma)

plot(fig9a)

# Fig 9B: Effect of carbon tax on manufacturing employment level (Cumulative IRF; LP regression in Eq. (2); restricted)
fig9b <- lp_lin_panel(data_set = df_select,
                      data_sample = "Full",
                      endog_data = "lempman",
                      cumul_mult = FALSE,
                      shock = "drater_LCU_USD18sw",
                      diff_shock = FALSE,
                      panel_model = "within",
                      panel_effect = "time",
                      robust_cov = "vcovHC",
                      l_exog_data = c("drater_LCU_USD18sw","dlrgdp","dlemptot", "dlempman", "dlpgdp"),
                      lags_exog_data = lagnum,
                      confint = conf95,
                      hor = h) |>
  scale_irfs(gamma)

plot(fig9b)

#--------------------------------
#--------------------------------
# Fig 10A: Effect of carbon tax on covered sector emission level (Cumulative IRF; LP regression in Eq. (1); unrestricted)
fig10a <- lp_lin_panel(data_set = df_select,
                      data_sample = "Full",
                      endog_data = "lemission_ctsectors",
                      cumul_mult = FALSE,
                      shock = "rater_LCU_USD18sw",
                      diff_shock = FALSE,
                      panel_model = "within",
                      panel_effect = "time",
                      robust_cov = "vcovHC",
                      l_exog_data = c("rater_LCU_USD18sw","dlemission_ctsectors","dlrgdp","dlemptot", "dlempman", "dlpgdp"),
                      lags_exog_data = lagnum,
                      confint = conf95,
                      hor = h) |>
  scale_irfs(gamma)

plot(fig10a)

# Fig 10B: Effect of carbon tax on covered sector emission level (Cumulative IRF; LP regression in Eq. (2); restricted)
fig10b <- lp_lin_panel(data_set = df_select,
                      data_sample = "Full",
                      endog_data = "lemission_ctsectors",
                      cumul_mult = FALSE,
                      shock = "drater_LCU_USD18sw",
                      diff_shock = FALSE,
                      panel_model = "within",
                      panel_effect = "time",
                      robust_cov = "vcovHC",
                      l_exog_data = c("drater_LCU_USD18sw","dlemission_ctsectors","dlrgdp","dlemptot", "dlempman", "dlpgdp"),
                      lags_exog_data = lagnum,
                      confint = conf95,
                      hor = h) |>
  scale_irfs(gamma)

plot(fig10b)

#--------------------------------
#--------------------------------
# Fig 10A (non-cumulative)
fig10a_noncum <- lp_lin_panel(data_set = df_select,
                       data_sample = "Full",
                       endog_data = "dlemission_ctsectors",
                       cumul_mult = FALSE,
                       shock = "rater_LCU_USD18sw",
                       diff_shock = FALSE,
                       panel_model = "within",
                       panel_effect = "time",
                       robust_cov = "vcovHC",
                       l_exog_data = c("rater_LCU_USD18sw","dlemission_ctsectors","dlrgdp","dlemptot", "dlempman", "dlpgdp"),
                       lags_exog_data = lagnum,
                       confint = conf95,
                       hor = h) |>
  scale_irfs(gamma)

plot(fig10a_noncum)

# Fig 10B: Effect of carbon tax on covered sector emission level (Cumulative IRF; LP regression in Eq. (2); restricted)
fig10b_noncum <- lp_lin_panel(data_set = df_select,
                       data_sample = "Full",
                       endog_data = "dlemission_ctsectors",
                       cumul_mult = FALSE,
                       shock = "drater_LCU_USD18sw",
                       diff_shock = FALSE,
                       panel_model = "within",
                       panel_effect = "time",
                       robust_cov = "vcovHC",
                       l_exog_data = c("drater_LCU_USD18sw","dlemission_ctsectors","dlrgdp","dlemptot", "dlempman", "dlpgdp"),
                       lags_exog_data = lagnum,
                       confint = conf95,
                       hor = h) |>
  scale_irfs(gamma)

plot(fig10b_noncum)


#--------------------------------
#--------------------------------
# Fig A9: Effect of carbon tax on GDP growth (LP regression in Eq. (1); restricted)
# excluding Scandinavian countries
scandinavian <- c("Denmark", "Finland", "Norway", "Sweden")

figa9 <- lp_lin_panel(data_set = df_select |> filter(country %!in% scandinavian),
                      data_sample = "Full",
                      endog_data = "dlrgdp",
                      cumul_mult = FALSE,
                      shock = "drater_LCU_USD18sw",
                      diff_shock = FALSE,
                      panel_model = "within",
                      panel_effect = "time",
                      robust_cov = "vcovHC",
                      l_exog_data = c("drater_LCU_USD18sw","dlrgdp","dlemptot", "dlempman", "dlpgdp"),
                      lags_exog_data = lagnum,
                      confint = conf95,
                      hor = h) |>
  scale_irfs(gamma)

plot(figa9)

# Fig A10: Effect of carbon tax on total employment growth (LP regression in Eq. (1); restricted)
# excluding Scandinavian countries
figa10 <- lp_lin_panel(data_set = df_select |> filter(country %!in% scandinavian),
                      data_sample = "Full",
                      endog_data = "dlemptot",
                      cumul_mult = FALSE,
                      shock = "drater_LCU_USD18sw",
                      diff_shock = FALSE,
                      panel_model = "within",
                      panel_effect = "time",
                      robust_cov = "vcovHC",
                      l_exog_data = c("drater_LCU_USD18sw","dlrgdp","dlemptot", "dlempman", "dlpgdp"),
                      lags_exog_data = lagnum,
                      confint = conf95,
                      hor = h) |>
  scale_irfs(gamma)

plot(figa10)

# Fig A11: Effect of carbon tax on GDP growth (LP regression in Eq. (1); restricted)
# including Scandinavian countries
figa11 <- lp_lin_panel(data_set = df_select |> filter(country %in% scandinavian),
                      data_sample = "Full",
                      endog_data = "dlrgdp",
                      cumul_mult = FALSE,
                      shock = "drater_LCU_USD18sw",
                      diff_shock = FALSE,
                      panel_model = "within",
                      panel_effect = "time",
                      robust_cov = "vcovHC",
                      l_exog_data = c("drater_LCU_USD18sw","dlrgdp","dlemptot", "dlempman", "dlpgdp"),
                      lags_exog_data = lagnum,
                      confint = conf95,
                      hor = h) |>
  scale_irfs(gamma)

plot(figa11)

# Fig A12: Effect of carbon tax on total employment growth (LP regression in Eq. (1); restricted)
# including Scandinavian countries
figa12 <- lp_lin_panel(data_set = df_select |> filter(country %in% scandinavian),
                       data_sample = "Full",
                       endog_data = "dlemptot",
                       cumul_mult = FALSE,
                       shock = "drater_LCU_USD18sw",
                       diff_shock = FALSE,
                       panel_model = "within",
                       panel_effect = "time",
                       robust_cov = "vcovHC",
                       l_exog_data = c("drater_LCU_USD18sw","dlrgdp","dlemptot", "dlempman", "dlpgdp"),
                       lags_exog_data = lagnum,
                       confint = conf95,
                       hor = h) |>
  scale_irfs(gamma)

plot(figa12)
