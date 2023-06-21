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

  ggsave("alt_g_boxplot_6723.pdf", plot = last_plot(), width = 6, height = 4, units = "in", device = "pdf", path = "./output")

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
    scale_color_manual(values = girlboss_palette("elf_bar"), name = "Read\nDepth") +
    theme_bw() +
    theme(strip.background = element_rect(fill = "white")) +
    geom_hline(yintercept = 0, lty = 2)

    ggsave("alt_gl_boxplot_6723.pdf", plot = last_plot(), width = 6, height = 3, units = "in", device = "pdf", path = "./output")

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
      scale_color_manual(values = girlboss_palette("elf_bar"), name = "Parent\nGenotypes") +
      theme_bw() +
      theme(strip.background = element_rect(fill = "white")) +
      geom_hline(yintercept = 0, lty = 2)

   ggsave("null_g_boxplot_62123.pdf", plot = last_plot(), width = 6, height = 4, units = "in", device = "pdf", path = "./output")


    ## Genotype Likelihoods - Boxplot

    #RD = 10
   null_gl %>%
     filter(rd == 10) %>%
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
      scale_color_manual(values = girlboss_palette("elf_bar"), name = "Parent\nGenotypes") +
      theme_bw() +
      theme(strip.background = element_rect(fill = "white")) +
      geom_hline(yintercept = 0, lty = 2)

   ggsave("null_gl_boxplot_rd10_62123.pdf", plot = last_plot(), width = 6, height = 6, units = "in", device = "pdf", path = "./output")

   #RD = 100
   null_gl %>%
     filter(rd == 100) %>%
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
     scale_color_manual(values = girlboss_palette("elf_bar"), name = "Parent\nGenotypes") +
     theme_bw() +
     theme(strip.background = element_rect(fill = "white")) +
     geom_hline(yintercept = 0, lty = 2)
