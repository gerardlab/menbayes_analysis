library(tidyverse)
library(menbayes)

## All have mean 2, so can use genotype 2, 2 for parents as null genotype
genofreq <- list(
  rep(0.2, 5),
  c(0.4, 0.1, 0, 0.1, 0.4)
)
n <- c(10, 100, 1000) #sample sizes
rd <- c(10, 100)
niter <- seq_len(100)

simdf <- expand_grid(n, genofreq, rd, niter)

simdf$logbf <- NA_real_ #missing value indicator for log Bayes Factor

for (i in seq_len(nrow(simdf))) {
  cat("Iteration:", i, "\n")

  ## Simulate genotype likelihoods here
  ogc <- c(stats::rmultinom(n = 1, size = simdf$n[[i]], prob = simdf$genofreq[[i]]))
  gl <- hwep::simgl(nvec = ogc, rdepth = simdf$rd[[i]], ret = "gl")

  ## Fit Bayes test here
  trash <- capture.output(
    marg_null <-  marg_f1_dr_pp_glpknown4(gl = gl, p1 = 2, p2 = 2, chains = 1)
  )
  trash <- capture.output(
    marg_alt <- marg_alt_gl(gl = gl, chains = 1)
  )

  log_bf <- marg_null - marg_alt

  ## Assign the i-th value of logbf to be the log-BF
  simdf$logbf[i] <- log_bf
}

## write to RDS
saveRDS(simdf, "./output/sims/alt_sims_gl.RDS")
