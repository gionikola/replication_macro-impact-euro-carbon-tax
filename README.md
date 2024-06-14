A replication of "The Macroeconomic Impact of Europe's Carbon Taxes" by Gilbert Metcalf and James Stock (AEJ Macro, 2023) using R.

Replication done by Thomas Ash and Giorgi Nikolaishvili.

---

# TO-DO

## Main Figures

[x] Fig A4: Effect of carbon tax on itself (standard and cumulative)

[x] Fig 3A: Effect of carbon tax on GDP growth (LP regression in Eq. (1); unrestricted)

[x] Fig 3B: Effect of carbon tax on GDP growth (LP regression in Eq. (1); restricted)

[x] Fig 4A: Effect of carbon tax on GDP growth (bivariate panel LP; restricted)

[] Fig 4B: Effect of carbon tax on GDP growth (SVAR; restricted)

[x] Fig 5A: Cumulated effect of carbon tax on GDP (LP regression in Eq. (1); unrestricted)

[x] Fig 5B: Cumulated effect of carbon tax on GDP (LP regression in Eq. (1); restricted)

[x] Fig 6A: Effect of carbon tax on total employment growth (LP regression in Eq. (1); unrestricted)

[x] Fig 6B: Effect of carbon tax on total employment growth (LP regression in Eq. (1); restricted)

[x] Fig 7A: Cumulated effect of carbon tax on total employment (LP regression in Eq. (1); unrestricted)

[x] Fig 7B: Cumulated effect of carbon tax on total employment (LP regression in Eq. (1); restricted)

[x] Fig 8A: Effect of carbon tax on manufacturing employment growth (LP regression in Eq. (1); unrestricted)

[x] Fig 8B: Effect of carbon tax on manufacturing employment growth (LP regression in Eq. (1); restricted)

[x] Fig 9A: Cumulated effect of carbon tax on manufacturing employment (LP regression in Eq. (1); unrestricted)

[x] Fig 9B: Cumulated effect of carbon tax on manufacturing employment (LP regression in Eq. (1); restricted)

[x] Fig 10A: Cumulated effect of carbon tax on covered sector output (LP regression in Eq. (1); unrestricted)

[x] Fig 10B: Cumulated effect of carbon tax on covered sector output (LP regression in Eq. (1); restricted)

## Robustness Figures

[] Fig 11A: Effect of carbon tax on GDP growth in revenue recycling carbon tax countries only (LP regression in Eq. (1); restricted)

[] Fig 11B: Effect of carbon tax on total employment growth in revenue recycling carbon tax countries only (LP regression in Eq. (1); restricted)

[] Fig 12A: Effect of carbon tax on GDP growth in non-revenue recycling carbon tax countries only (LP regression in Eq. (1); restricted)

[] Fig 12B: Effect of carbon tax on total employment growth in non-revenue recycling carbon tax countries only (LP regression in Eq. (1); restricted)

[] Fig 13A: Cumulated effect of carbon tax on covered sector output in revenue recycling carbon tax countries only(LP regression in Eq. (1); restricted)

[] Fig 13B: Cumulated effect of carbon tax on covered sector output in non-revenue recycling carbon tax countries only(LP regression in Eq. (1); restricted)

[] Fig 14A: Effect of carbon tax on GDP growth in large carbon tax countries only (LP regression in Eq. (1); restricted)

[] Fig 14B: Cumulated effect of carbon tax on total employment in large carbon tax countries only (LP regression in Eq. (1); restricted)

[x] Fig A9: Effect of carbon tax on GDP growth in sample without Denmark, Finland, Norway, and Sweden (LP regression in Eq. (1); restricted)

[x] Fig A10: Effect of carbon tax on total employment growth in sample without Denmark, Finland, Norway, and Sweden (LP regression in Eq. (1); restricted)

[x] Fig A11: Effect of carbon tax on GDP growth in sample with ONLY Denmark, Finland, Norway, and Sweden (LP regression in Eq. (1); restricted)

[x] Fig A12: Effect of carbon tax on total employment growth in sample with ONLY Denmark, Finland, Norway, and Sweden (LP regression in Eq. (1); restricted)

## Notes

- There are really only three model specifications in the paper, applied across a variety of contexts:
  1. Multivariate Lag-augmented panel LP with year- and country-fixed effects;
  2. Bivariate lag-augmented panel LP (I think this is basically the multivariate lag-augmented panel LP, but without controls) with year- and country-fixed effects;
  3. Bivariate panel VAR with year- and country-fixed effects.
- Model 1 is estimated without restrictions & with a no-long-run-effect restriction. Models 2 and 3 seem to only be estimated with the no-long-ru-effect restriction. Need to figure out how to apply this restriction.
- All LP standard errors are heteroskedasticity-consistent a la White (Eicker-Huber-White standard errors; `vcovHC` in `lpirfs` R package) -- following the recommendation of Plagborg-Moller and Montiel Olea (2021) in the context of lag-augmented LPs. The paper seems to incorrectly cite Plagborg-Moller and Wolf (2021).
- VAR IRF standard errors are bootstrapped. The paper is unclear about the exact type of bootstrap procedure used.
- `R` packages we can use for the replication: `lpirfs` (based on `plm`) and `panelvar`. 
