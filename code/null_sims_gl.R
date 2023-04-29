## Load packages ----
library(tidyverse)
library(menbayes)
library(future)
library(foreach)
library(doFuture)
library(rngtools)
library(doRNG)

## Determine number of cores ----
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  nc <- 1
} else {
  eval(parse(text = args[[1]]))
}
cat(nc, "\n")

## Register workers ----
if (nc == 1) {
  registerDoSEQ()
  plan(sequential)
} else {
  registerDoFuture()
  registerDoRNG()
  plan(multisession, workers = nc)
  if (getDoParWorkers() == 1) {
    stop("nc > 1, but only one core registered")
  }
}

## Run simulations ----
p1 <- 0:2 #parent 1 genotypes
p2 <- 0:2 #parent 2 genotypes
alpha <- c(0, 1/12, 1/6) #different double reduction rates
xi <- c(0, 1/6, 1/3) #different preferential pairing rates
n <- c(10, 100, 1000) #sample sizes
rd <- c(10, 100) #read depths
niter <- seq_len(100) #number of iterations per simulation scenario
ploidy <- 4

simdf_null <- expand_grid(p1 = p1, p2 = p2, alpha = alpha, xi = xi, n = n, rd = rd, niter = niter) |>
  filter((p1 == 0 & p2 == 1) |
           (p1 == 0 & p2 == 2) |
           (p1 == 1 & p2 == 1) |
           (p1 == 1 & p2 == 2) |
           (p1 == 2 & p2 == 2))

simdf_null$logbf <- NA_real_ #missing value indicator for log Bayes Factor
simdf_null$logbf <- NA_real_ #missing value indicator for log Bayes Factor
simdf_null$pm_alpha <- NA_real_ #missing value indicator for the posterior mean of alpha
simdf_null$pm_xi <- NA_real_ #missing value indicator for the posterior mean of xi
simdf_null$chisq_stat <- NA_real_ #missing value indicator for chi-sq statistic of observed v. expected genotype counts
simdf_null$chisq_pvalue <- NA_real_ #missing value indicator for chi-sq p-value of observed v. expected genotype counts

outdf <- foreach(i = seq_len(nrow(simdf_null)),
                 .combine = rbind,
                 .export = c("simdf_null")) %dopar% {
  ## Simulate genotypes here
  ogf <- offspring_gf(alpha = simdf_null$alpha[i], xi = simdf_null$xi[i], p1 = simdf_null$p1[i], p2 = simdf_null$p2[i])
  ogc <- offspring_geno(x = ogf, n = simdf_null$n[i])
  genovec <- gcount_to_gvec(gcount = ogc)
  fout <- po_gl(genovec = genovec,
                p1_geno = simdf_null$p1[i],
                p2_geno = simdf_null$p2[i],
                ploidy = 4,
                rd = simdf_null$rd[i],
                seq = 0.01,
                bias = 1,
                od = 0.01)
  gl <- fout$genologlike

  ## Fit Bayes test here
  trash <- capture.output(
    marg_null <- marg_f1_dr_pp_glpknown4(gl = gl, p1 = simdf_null$p1[i], p2 = simdf_null$p2[i], output = "all")
  )
  trash <- capture.output(
    marg_alt <- marg_alt_gl(gl = gl)
  )

  log_bf <- marg_null[[1]] - marg_alt

  ## Assign the i-th value of logbf to be the log-BF
  simdf_null$logbf[i] <- log_bf

  ## Fit Chi Square test here
  suppressWarnings(
    chi <- chisq_gl4(gl = gl, l1 = simdf_null$p1[i], l2 = simdf_null$p2[i])
  )

  ## Assign the i-th value of chisq_stat to be the chi-sq statistic
  simdf_null$chisq_stat[i] <- chi[[1]]

  ## Assign the i-th value of chisq_pvalue to be the chi-sq p-value
  simdf_null$chisq_pvalue[i] <- chi[[2]]

  ## Assign the i-th value of pm_xi to be the posterior mean of xi
  simdf_null$pm_xi[i] <- mean(marg_null[[2]][[2]])

  ## Assign the i-th value of pm_alpha to be the posterior mean of alpha
  simdf_null$pm_alpha[i] <- mean(marg_null[[2]][[1]])

  simdf_null[i, ]
}

## Unregister workers ----
if (nc > 1) {
  plan(sequential)
}

write.csv(outdf, "./output/sims/null_sims_gl.csv")
