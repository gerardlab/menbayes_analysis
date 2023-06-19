library(updog)
library(menbayes)
library(tidyverse)
library(future)
library(foreach)
library(doFuture)
library(rngtools)
library(doRNG)
bluefits <- readRDS("./output/blue/bluefits.RDS")

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

## Remove 0/0 and 4/4 and 0/4 and 4/0 parental genotype scenarios
bluefits <- filter_snp(
  x = bluefits,
  expr = !(ell1 == 0 & ell2 == 0 |
             ell1 == 0 & ell2 == 4 |
             ell1 == 4 & ell2 == 0 |
             ell1 == 4 & ell2 == 4)
)

## This will give you an array with dimensions SNPs by Individuals by Genotypes
gl <- format_multidog(bluefits, varname = paste0("logL_", 0:4))
p1vec <- bluefits$snpdf$ell1
p2vec <- bluefits$snpdf$ell2

##Build dataframe
blue_df <- data.frame(snp = dimnames(gl)[[1]], p1 = p1vec, p2 = p2vec)
blue_df$logbf <- NA_real_ #missing value indicator for log Bayes Factor
blue_df$pm_alpha <- NA_real_ #missing value indicator for the posterior mean of alpha
blue_df$pm_xi <- NA_real_ #missing value indicator for the posterior mean of xi
blue_df$chisq_stat <- NA_real_ #missing value indicator for chi-sq statistic of observed v. expected genotype counts
blue_df$chisq_pvalue <- NA_real_ #missing value indicator for chi-sq p-value of observed v. expected genotype counts

## Sanity checks
stopifnot(nrow(blue_df) == dim(gl)[[1]])

outdf <- foreach(i = seq_len(dim(gl)[[1]]), .combine = rbind, .export = c("blue_df")) %dopar% {
  glmat <- na.omit(gl[i , ,])
  p1 <- p1vec[[i]]
  p2 <- p2vec[[i]]

  ## Fit Bayes test here
  trash <- capture.output(
    marg_null <- marg_f1_dr_pp_glpknown4(gl = glmat, p1 = p1, p2 = p2, output = "all")
  )
  trash <- capture.output(
    marg_alt <- marg_alt_gl(gl = glmat)
  )

  log_bf <- marg_null[[1]] - marg_alt

  ## Assign the i-th value of logbf to be the log-BF
  blue_df$logbf[i] <- log_bf

  ## Fit Chi Square test here
  suppressWarnings(
    chi <- chisq_gl4(gl = glmat, l1 = p1, l2 = p2)
  )

  ## Assign the i-th value of chisq_stat to be the chi-sq statistic
  blue_df$chisq_stat[i] <- chi[[1]]

  ## Assign the i-th value of chisq_pvalue to be the chi-sq p-value
  blue_df$chisq_pvalue[i] <- chi[[2]]

  ## Assign the i-th value of pm_xi to be the posterior mean of xi
  blue_df$pm_xi[i] <- mean(marg_null[[2]]$xi)

  ## Assign the i-th value of pm_alpha to be the posterior mean of alpha
  blue_df$pm_alpha[i] <- mean(marg_null[[2]]$alpha)

  blue_df[i, ]
}

## Unregister workers ----
if (nc > 1) {
  plan(sequential)
}

write.csv(outdf, "./output/blue/blue_df.csv")
