library(tidyverse)
library(menbayes)

## All have mean 2, so can use genotype 2, 2 for parents as null genotype
genofreq <- list(
  rep(0.2, 5),
  c(0.4, 0.1, 0, 0.1, 0.4)
)
n <- c(10, 100, 1000) #sample sizes
niter <- seq_len(100)

simdf <- expand_grid(n, genofreq, niter)

simdf$logbf <- NA_real_ #missing value indicator for log Bayes Factor

for (i in seq_len(nrow(simdf))) {
  ## Simulate genotypes here
  ogc <- c(stats::rmultinom(n = 1, size = simdf$n[[i]], prob = simdf$genofreq[[i]]))

  ## Fit Bayes test here
  trash <- capture.output(
    marg_null <- marg_f1_dr_pp_g4(x = ogc, g1 = 2, g2 = 2, chains = 1)
  )
  trash <- capture.output(
    marg_alt <- hwep:::ddirmult(x = ogc, alpha = rep(1, 5), lg = TRUE)
  )

  log_bf <- marg_null - marg_alt

  ## Assign the i-th value of logbf to be the log-BF
  simdf$logbf[i] <- log_bf
}

## write to rDS
saveRDS(simdf, "./output/sims/alt_sims_g.RDS")
