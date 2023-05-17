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

  ggsave("alt_g_boxplot.pdf", plot = last_plot(), device = "pdf", path = "./output")

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
    scale_color_manual(values = girlboss_palette("girlboss_in_question")) +
    theme_bw() +
    theme(strip.background = element_rect(fill = "white")) +
    geom_hline(yintercept = 0, lty = 2)

    ggsave("alt_gl_boxplot.pdf", plot = last_plot(), device = "pdf", path = "./output")

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

   ggsave("null_g_boxplot.pdf", plot = last_plot(), width = 7, height = 5, units = "in", device = "pdf", path = "./output")


    ## Genotype Likelihoods - Boxplot

    #RD = 10
   null_gl %>%
      mutate(n = as.factor(n),
             `Parent Genotypes` = paste0("(", p1, ",", p2, ")"),
             alpha = paste0("alpha==", as.character(MASS::fractions(alpha))),
             xi = paste0("xi==", as.character(MASS::fractions(xi))),
             xi = parse_factor(xi)) %>%
      filter(rd == 10) %>%
      ggplot(mapping = aes(x = n, y = logbf, color = `Parent Genotypes`)) +
      geom_boxplot() +
      facet_grid(alpha ~ xi, labeller = label_parsed) +
      xlab("Sample Size") +
      ylab("Log Bayes Factor") +
      scale_color_manual(values = girlboss_palette("girlboss_in_question")) +
      theme_bw() +
      theme(strip.background = element_rect(fill = "white")) +
      geom_hline(yintercept = 0, lty = 2)

   ggsave("null_gl_boxplot_rd10.pdf", plot = last_plot(), width = 7, height = 5, units = "in", device = "pdf", path = "./output")

   ##RD = 100

    null_gl %>%
      mutate(n = as.factor(n),
             `Parent Genotypes` = paste0("(", p1, ",", p2, ")"),
             alpha = paste0("alpha==", as.character(MASS::fractions(alpha))),
             xi = paste0("xi==", as.character(MASS::fractions(xi))),
             xi = parse_factor(xi)) %>%
      filter(rd == 100) %>%
      ggplot(mapping = aes(x = n, y = logbf, color = `Parent Genotypes`)) +
      geom_boxplot() +
      facet_grid(alpha ~ xi, labeller = label_parsed) +
      xlab("Sample Size") +
      ylab("Log Bayes Factor") +
      scale_color_manual(values = girlboss_palette("girlboss_in_question")) +
      theme_bw() +
      theme(strip.background = element_rect(fill = "white")) +
      geom_hline(yintercept = 0, lty = 2)

    ggsave("null_gl_boxplot_rd100.pdf", plot = last_plot(), width = 7, height = 5, units = "in", device = "pdf", path = "./output")

    ## Genotypes Known - ChiSq QQ Plot -- can't get the loop to work. Separate file with
    ## all plots (inefficiently) written out

    chisq_plot <- function(df){
      csqq <- ggplot(data = df, aes(sample=chisq_pvalue, color = as.factor(n))) +
        geom_qq(size = 2, distribution = qunif) +
        geom_abline(slope = 1, intercept = 0) +
        facet_grid(alpha ~ xi, labeller = label_parsed) +
        xlab("Theoretical Quantiles") +
        ylab("Sample Quantiles") +
        scale_color_manual(values = girlboss_palette("girlboss_in_question")) +
        theme_bw() +
        theme(strip.background = element_rect(fill = "white"))
      print(csqq)
    }


    for (i in 0:2) {
      for (j in 0:2) {

        null_g %>%
          mutate(n = as.factor(n),
                 alpha = paste0("alpha==", as.character(MASS::fractions(alpha))),
                 alpha = parse_factor(alpha),
                 xi = paste0("xi==", as.character(MASS::fractions(xi))),
                 xi = parse_factor(xi),
                 p1 = as.factor(p1),
                 p2 = as.factor(p2)) %>%
          filter(p1 == i & p2 == j)

        chisq_plot(df = null_g)

      }
    }

    ## Genotype Likelihoods - ChiSq QQ Plot (same as above. can't get loop to work.)

