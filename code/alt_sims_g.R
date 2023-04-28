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
  marg_null <- marg_f1_dr_pp_g4(x = ogc, g1 = 2, g2 = 2, chains = 1)
  marg_alt <- hwep:::ddirmult(x = ogc, alpha = rep(1, 5), lg = TRUE)

  log_bf <- marg_null - marg_alt

  ## Assign the i-th value of logbf to be the log-BF
  simdf$logbf[i] <- log_bf
}

  ## write to csv
  saveRDS(simdf, "~/thesis/mira_proj/output/simdf_alt.RDS")

  ## looking at the bayes factors

simdf %>%
  mutate(xi = as.factor(xi),
         n = as.factor(n),
         alpha = as.factor(alpha)) %>%
  ggplot(mapping = aes(x = n, y = logbf, color = xi)) +
    geom_boxplot() +
    facet_grid(rows = vars(p1, p2), cols = vars(alpha)) +
    xlab("Sample Size") +
    ylab("Log Bayes Factor")

