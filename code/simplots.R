## Libraries

  #library(menbayes)
  library(tidyverse)
  #devtools::install_github("thakkar-mira/girlboss")
  library(girlboss)

## Load Data

  alt_g <- readRDS("./output/sims/alt_sims_g.RDS")

  alt_gl <- readRDS("./output/sims/alt_sims_gl.RDS")

  null_g <- read.csv("./output/sims/null_sims_g.csv")

  null_gl <- read.csv("./output/sims/null_sims_gl.csv")

## Alternative Sims

    ## Genotypes Known - Boxplot

  alt_g <- alt_g %>%
    mutate(n = as.factor(n),
           genofreq = factor(genofreq,
                             levels=c("c(0.2, 0.2, 0.2, 0.2, 0.2)",
                                      "c(0.4, 0.1, 0, 0.1, 0.4)")))

  alt_g$genofreq <- recode_factor(alt_g$genofreq, "c(0.2, 0.2, 0.2, 0.2, 0.2)" = "(0.2, 0.2, 0.2, 0.2, 0.2)",
                                  "c(0.4, 0.1, 0, 0.1, 0.4)" = "(0.4, 0.1, 0, 0.1, 0.4)")

  ggplot(data = alt_g, mapping = aes(x = n, y = logbf)) +
    geom_boxplot() +
    facet_wrap(~genofreq, scales = "free") +
    xlab("Sample Size") +
    ylab("Log Bayes Factor") +
    theme_bw() +
    theme(strip.background = element_rect(fill = "white")) +
    geom_hline(yintercept = 0, lty = 2)

    ## Genotype Likelihoods - Boxplot

  alt_gl <- alt_gl %>%
    mutate(n = as.factor(n),
           genofreq = factor(genofreq, levels=c("c(0.2, 0.2, 0.2, 0.2, 0.2)","c(0.4, 0.1, 0, 0.1, 0.4)")))

  alt_gl$genofreq <- recode_factor(alt_gl$genofreq, "c(0.2, 0.2, 0.2, 0.2, 0.2)" = "(0.2, 0.2, 0.2, 0.2, 0.2)", "c(0.4, 0.1, 0, 0.1, 0.4)" = "(0.4, 0.1, 0, 0.1, 0.4)")

  ggplot(data = alt_gl, mapping = aes(x = n, y = logbf, color = as.factor(rd))) +
    geom_boxplot() +
    facet_wrap(~genofreq) +
    xlab("Sample Size") +
    ylab("Log Bayes Factor") +
    labs(color = "Read Depth") +
    scale_color_manual(values = girlboss_palette("girlboss_in_question"))
    theme_bw() +
    theme(strip.background = element_rect(fill = "white")) +
    geom_hline(yintercept = 0, lty = 2)

## Null Sims

    ## Genotypes Known - Boxplot
    null_g %>%
      mutate(n = as.factor(n),
             `Parent Genotypes` = paste0("(", p1, ",", p2, ")"),
             alpha = paste0("alpha==", as.character(MASS::fractions(alpha))),
             xi = paste0("xi==", as.character(MASS::fractions(xi))),
             xi = parse_factor(xi)) %>%
      ggplot(mapping = aes(x = n, y = logbf, color = `Parent Genotypes`)) +
      geom_boxplot() +
      facet_grid(alpha ~ xi, labeller = label_parsed) +
      xlab("Sample Size") +
      ylab("Log Bayes Factor") +
      scale_color_manual(values = girlboss_palette("girlboss_in_question")) +
      theme_bw() +
      theme(strip.background = element_rect(fill = "white")) +
      geom_hline(yintercept = 0, lty = 2)

    ## Genotypes Known - ChiSq QQ Plot
  null_g %>%
    mutate(n = as.factor(n),
           `Parent Genotypes` = paste0("(", p1, ",", p2, ")"),
           alpha = paste0("alpha==", as.character(MASS::fractions(alpha))),
           xi = paste0("xi==", as.character(MASS::fractions(xi))),
           xi = parse_factor(xi)) %>%
    ggplot(aes(sample=chisq_pvalue, color = n)) +
    geom_qq(size = 2, distribution = qunif) +
    geom_abline(slope = 1, intercept = 0) +
    facet_grid(alpha ~ xi, labeller = label_parsed) +
    xlab("Theoretical Quantiles") +
    ylab("Sample Quantiles") +
    scale_color_manual(values = girlboss_palette("girlboss_in_question")) +
    theme_bw() +
    theme(strip.background = element_rect(fill = "white"))

    ## Genotype Likelihoods - Boxplot


    ## Genotype Likelihoods - ChiSq QQ Plot

