# Load packages & custom utils
source("utils.R")

#--------------------------------
#--------------------------------
#--------------------------------
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
                   drater_LCU_USD18sw,
                   IntRevRec)

## Drop Liechtenstein
df_select <- df_select |>
  filter(country != "Liechtenstein")

#--------------------------------
#--------------------------------
#--------------------------------
#--------------------------------
# Set analysis hyperparameters
conf95 = 1.96
conf68 = 1
h = 6 + 1 
h8 = h + 2
lagnum = 4
gamma = 40 * 0.3 #irf scaling factor ($40 * 0.3 share)

#--------------------------------
#--------------------------------
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
                      hor = h8)

plot(figa4_diff)

## Quickplot IRF and CIRF
plot(c(0:8), figa4_diff[1][[1]], type = "b", ylim=c(-0.5, 1.5))
lines(c(0:8), figa4_diff[2][[1]], type = "b")
lines(c(0:8), figa4_diff[3][[1]], type = "b")
lines(c(0:8), figa4_diff[1][[1]] |> cumsum(), type = "b")
abline(a=0, b=0)

## Based on FigA4 estimates
## find sequence of carbon tax shocks 
## that cause a permanent increase in tax rate
col1 <- figa4_diff[1][[1]] |> 
  as.vector()
col1 <- col1[1:h]
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
                      hor = h8)

plot(figa4_level)

## Quickplot level-IRF and CIRF
plot(c(0:8), figa4_level[1][[1]], type = "b", ylim=c(-0.5, 1.5))
lines(c(0:8), figa4_level[2][[1]], type = "b")
lines(c(0:8), figa4_level[3][[1]], type = "b")
lines(c(0:8), figa4_diff[1][[1]] |> cumsum(), type = "b")
abline(a=0, b=0)

#--------------------------------
#--------------------------------
#--------------------------------
#--------------------------------
# Custom Figure: Effect of carbon tax on carbon tax 

temp_df <- create_irf_df(figa4_diff$irf_panel_mean, 8)

response_plot <- ggplot(temp_df, aes(x=horizon, y=response, color=response_type)) +
  geom_line(size=2) +
  theme_classic() +
  theme(legend.position = "bottom") +
  coord_cartesian(expand = FALSE) + 
  theme(text=element_text(size=15))

new_response_plot <- response_plot + 
  ylab("$/ton") +
  xlab("Horizon (Years)") +
  geom_line(aes(y = 1), col = "black") +
  geom_line(aes(y = 0), col = "black") + 
  scale_color_discrete(name = "Response Type:")

new_response_plot

ggsave(filename = "figs/fig_a4.svg", 
       new_response_plot,
       width = 6, height = 4, dpi = 300, units = "in", device='svg')

#--------------------------------
#--------------------------------
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

## Quickplot Sims point estimates
plot(c(0:6), fig3a[1][[1]], type = "b", ylim = c(-4,4))
lines(c(0:6), sims(fig3a, xpath, 7), type = "b")
abline(a=0, b=0)

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

## Quickplot Sims point estimates
plot(c(0:6), fig3b[1][[1]], type = "b", ylim = c(-4,4))
lines(c(0:6), sims(fig3b, xpath, 7), type = "b")
abline(a=0, b=0)

#--------------------------------
#--------------------------------
#--------------------------------
#--------------------------------
# Custom Figures: Effect of permanent carbon tax change on GDP 

## Unrestricted model
temp_df <- create_irf_df(sims(fig3a, xpath, h), h-1)

new_response_plot <- create_irf_plot(temp_df, c(-4,4))

new_response_plot

ggsave(filename = "figs/fig_unrestricted_carbon-tax-on-gdp.svg", 
       new_response_plot,
       width = 6, height = 4, dpi = 300, units = "in", device='svg')

## Restricted model
temp_df <- create_irf_df(sims(fig3b, xpath, h), h-1)

new_response_plot <- create_irf_plot(temp_df, c(-4,4))

new_response_plot

ggsave(filename = "figs/fig_restricted_carbon-tax-on-gdp.svg", 
       new_response_plot,
       width = 6, height = 4, dpi = 300, units = "in", device='svg')

#--------------------------------
#--------------------------------
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

## Quickplot Sims point estimates
plot(c(0:6), fig4a[1][[1]], type = "b", ylim = c(-4,4))
lines(c(0:6), sims(fig4a, xpath, 7), type = "b")
abline(a=0, b=0)

#--------------------------------
#--------------------------------
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
#--------------------------------
#--------------------------------
# Custom Figures: Effect of permanent carbon tax change on total employment

## Unrestricted model
temp_df <- create_irf_df(sims(fig6a, xpath, h), h-1)

new_response_plot <- create_irf_plot(temp_df, c(-4,4))

new_response_plot

ggsave(filename = "figs/fig_unrestricted_carbon-tax-on-total-employment.svg", 
       new_response_plot,
       width = 6, height = 4, dpi = 300, units = "in", device='svg')

## Restricted model
temp_df <- create_irf_df(sims(fig6b, xpath, h), h-1)

new_response_plot <- create_irf_plot(temp_df, c(-4,4))

new_response_plot

ggsave(filename = "figs/fig_restricted_carbon-tax-on-total-employment.svg", 
       new_response_plot,
       width = 6, height = 4, dpi = 300, units = "in", device='svg')

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
#--------------------------------
#--------------------------------
# Custom Figures: Effect of permanent carbon tax change on manufacturing employment

## Unrestricted model
temp_df <- create_irf_df(sims(fig8a, xpath, h), h-1)

new_response_plot <- create_irf_plot(temp_df, c(-4,4))

new_response_plot

ggsave(filename = "figs/fig_unrestricted_carbon-tax-on-manufacturing-employment.svg", 
       new_response_plot,
       width = 6, height = 4, dpi = 300, units = "in", device='svg')

## Restricted model
temp_df <- create_irf_df(sims(fig8b, xpath, h), h-1)

new_response_plot <- create_irf_plot(temp_df, c(-4,4))

new_response_plot

ggsave(filename = "figs/fig_restricted_carbon-tax-on-manufacturing-employment.svg", 
       new_response_plot,
       width = 6, height = 4, dpi = 300, units = "in", device='svg')

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
plot(c(0:6), fig10a[1][[1]], type = "b")
lines(c(0:6), sims(fig10a, xpath, 7), type = "b")

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
plot(c(0:6), fig10a_noncum[1][[1]], type = "b")
lines(c(0:6), sims(fig10a_noncum, xpath, 7), type = "b")

# Fig 10B: (non-cumulative)
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
#--------------------------------
#--------------------------------
# Custom Figures: Effect of permanent carbon tax change on covered sector emissions

## Unrestricted model
temp_df <- create_irf_df(sims(fig10a_noncum, xpath, h), h-1)

new_response_plot <- create_irf_plot(temp_df, c(-10,10))

new_response_plot

ggsave(filename = "figs/fig_unrestricted_carbon-tax-on-covered-sector-emissions.svg", 
       new_response_plot,
       width = 6, height = 4, dpi = 300, units = "in", device='svg')

## Restricted model
temp_df <- create_irf_df(sims(fig10b_noncum, xpath, h), h-1)

new_response_plot <- create_irf_plot(temp_df, c(-10,10))

new_response_plot

ggsave(filename = "figs/fig_restricted_carbon-tax-on-covered-sector-emissions.svg", 
       new_response_plot,
       width = 6, height = 4, dpi = 300, units = "in", device='svg')

#--------------------------------
#--------------------------------
# Fig 11A: Effect on GDP growth; restricted; revenue recycling carbon tax countries only
fig11a <- lp_lin_panel(data_set = df_select |> filter(IntRevRec > 0),
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

plot(fig11a)

# Fig 11B: Effect on total employment growth; restricted; revenue recycling carbon tax countries only
fig11b <- lp_lin_panel(data_set = df_select |> filter(IntRevRec > 0),
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

plot(fig11b)

#--------------------------------
#--------------------------------
# Fig 12A: Effect on GDP growth; restricted; revenue recycling carbon tax countries only
fig12a <- lp_lin_panel(data_set = df_select |> 
                         mutate(IntRevRec = ifelse(is.na(IntRevRec) == TRUE, 0, IntRevRec)) |> 
                         filter(IntRevRec < 0.6),
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

plot(fig12a)

# Fig 12B: Effect on total employment growth; restricted; revenue recycling carbon tax countries only
fig12b <- lp_lin_panel(data_set = df_select |> 
                         mutate(IntRevRec = ifelse(is.na(IntRevRec) == TRUE, 0, IntRevRec)) |> 
                         filter(IntRevRec < 0.6),
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

plot(fig12b)

#--------------------------------
#--------------------------------
# Fig 13A: Effect of carbon tax on covered sector emission level in revenue recycling carbon tax countries
# (Cumulative IRF; LP regression in Eq. (2); restricted)
fig13a <- lp_lin_panel(data_set = df_select |> filter(IntRevRec > 0),
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

plot(fig13a)

# Fig 13B: Effect of carbon tax on covered sector emission level in revenue recycling carbon tax countries
# (Cumulative IRF; LP regression in Eq. (2); restricted)
fig13b <- lp_lin_panel(data_set = df_select |> 
                         mutate(IntRevRec = ifelse(is.na(IntRevRec) == TRUE, 0, IntRevRec)) |> 
                         filter(IntRevRec < 0.6),
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

plot(fig13b)

#--------------------------------
#--------------------------------
# Fig 13A Non-cumulative
fig13a_noncum <- lp_lin_panel(data_set = df_select |> filter(IntRevRec > 0),
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

plot(fig13a_noncum)

# Fig 13B Non-cumulative 
fig13b_noncum <- lp_lin_panel(data_set = df_select |> 
                         mutate(IntRevRec = ifelse(is.na(IntRevRec) == TRUE, 0, IntRevRec)) |> 
                         filter(IntRevRec < 0.6),
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

plot(fig13b_noncum)


#--------------------------------
#--------------------------------
# Fig 14A: Effect of carbon tax on GDP growth
# (LP regression in Eq. (2); restricted)
fig14a <- lp_lin_panel(data_set = df_select |> 
                         filter(country %in% c("Denmark",
                                               "Finland",
                                               "France",
                                               "Ireland",
                                               "Norway",
                                               "Sweden",
                                               "Switzerland")),
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

plot(fig14a)

# Fig 14B: Effect of carbon tax on covered sector emission level in large carbon tax countries
# (Cumulative IRF; LP regression in Eq. (2); restricted)
fig14b <- lp_lin_panel(data_set = df_select |> 
                         filter(country %in% c("Denmark",
                                               "Finland",
                                               "France",
                                               "Ireland",
                                               "Norway",
                                               "Sweden",
                                               "Switzerland")),
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

plot(fig14b)

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
