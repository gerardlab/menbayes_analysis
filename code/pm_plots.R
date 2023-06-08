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

#PM ALPHA ANALYSIS

#Null, genotypes known
null_g %>%
  mutate(n = as.factor(n),
         `Parent Genotypes` = paste0("(", p1, ",", p2, ")"),
         alpha = paste0("alpha==", as.character(MASS::fractions(alpha))))%>%
  ggplot(mapping = aes(x = pm_alpha, fill = `Parent Genotypes`)) +
  geom_histogram(bins = 40) +
  facet_grid(alpha ~ n, labeller = label_parsed) +
  xlab("Posterior Mean") +
  ylab("Frequency") +
  scale_fill_manual(values = girlboss_palette("elf_bar"), name = "Parent\nGenotypes") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white"))

ggsave("null_g_pmalpha_hist_6723.pdf", plot = last_plot(), width = 6, height = 6, units = "in", device = "pdf", path = "./output")

#Null, genotype likelihoods

#rd == 10
null_gl %>%
  mutate(n = as.factor(n),
         `Parent Genotypes` = paste0("(", p1, ",", p2, ")"),
         alpha = paste0("alpha==", as.character(MASS::fractions(alpha))))%>%
  filter(rd == 10) %>%
  ggplot(mapping = aes(x = pm_alpha, fill = `Parent Genotypes`)) +
  geom_histogram(bins = 40) +
  facet_grid(alpha ~ n, labeller = label_parsed) +
  xlab("Posterior Mean") +
  ylab("Frequency") +
  scale_fill_manual(values = girlboss_palette("elf_bar"), name = "Parent\nGenotypes") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white"))

ggsave("null_gl_pmalpha_rd10_hist_6723.pdf", plot = last_plot(), width = 6, height = 6, units = "in", device = "pdf", path = "./output")

#rd == 100
null_gl %>%
  mutate(n = as.factor(n),
         `Parent Genotypes` = paste0("(", p1, ",", p2, ")"),
         alpha = paste0("alpha==", as.character(MASS::fractions(alpha))))%>%
  filter(rd == 100) %>%
  ggplot(mapping = aes(x = pm_alpha, fill = `Parent Genotypes`)) +
  geom_histogram(bins = 40) +
  facet_grid(alpha ~ n, labeller = label_parsed) +
  xlab("Posterior Mean") +
  ylab("Frequency") +
  scale_fill_manual(values = girlboss_palette("elf_bar"), name = "Parent\nGenotypes") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white"))

ggsave("null_gl_pmalpha_rd100_hist_6723.pdf", plot = last_plot(), width = 6, height = 6, units = "in", device = "pdf", path = "./output")

#PM XI ANALYSIS

#Null, genotypes known
null_g %>%
  mutate(n = as.factor(n),
         `Parent Genotypes` = paste0("(", p1, ",", p2, ")"),
         xi = paste0("xi==", as.character(MASS::fractions(xi))),
         xi = parse_factor(xi))%>%
  ggplot(mapping = aes(x = pm_xi, fill = `Parent Genotypes`)) +
  geom_histogram(bins = 40) +
  facet_grid(xi ~ n, labeller = label_parsed) +
  xlab("Posterior Mean") +
  ylab("Frequency") +
  scale_fill_manual(values = girlboss_palette("elf_bar"), name = "Parent\nGenotypes") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white"))

ggsave("null_g_pmxi_hist_6723.pdf", plot = last_plot(), width = 6, height = 6, units = "in", device = "pdf", path = "./output")

#Null, genotype likelihoods

#rd == 10
null_gl %>%
  mutate(n = as.factor(n),
         `Parent Genotypes` = paste0("(", p1, ",", p2, ")"),
         xi = paste0("xi==", as.character(MASS::fractions(xi))),
         xi = parse_factor(xi))%>%
  filter(rd == 10) %>%
  ggplot(mapping = aes(x = pm_xi, fill = `Parent Genotypes`)) +
  geom_histogram(bins = 40) +
  facet_grid(xi ~ n, labeller = label_parsed) +
  xlab("Posterior Mean") +
  ylab("Frequency") +
  scale_fill_manual(values = girlboss_palette("elf_bar"), name = "Parent\nGenotypes") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white"))

ggsave("null_gl_pmxi_rd10_hist_6723.pdf", plot = last_plot(), width = 6, height = 6, units = "in", device = "pdf", path = "./output")

#rd == 100
null_gl %>%
  mutate(n = as.factor(n),
         `Parent Genotypes` = paste0("(", p1, ",", p2, ")"),
         xi = paste0("xi==", as.character(MASS::fractions(xi))),
         xi = parse_factor(xi))%>%
  filter(rd == 100) %>%
  ggplot(mapping = aes(x = pm_xi, fill = `Parent Genotypes`)) +
  geom_histogram(bins = 40) +
  facet_grid(xi ~ n, labeller = label_parsed) +
  xlab("Posterior Mean") +
  ylab("Frequency") +
  scale_fill_manual(values = girlboss_palette("elf_bar"), name = "Parent\nGenotypes") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white"))

ggsave("null_gl_pmxi_rd100_hist_6723.pdf", plot = last_plot(), width = 6, height = 6, units = "in", device = "pdf", path = "./output")


#############################
###### MORE TEST PLOTS ######
#############################

## g, pm alpha, n == 10
  null_g %>%
    mutate(n = as.factor(n),
           `Parent Genotypes` = paste0("(", p1, ",", p2, ")"),
           alpha = paste0("alpha==", as.character(MASS::fractions(alpha))),
           xi = paste0("xi==", as.character(MASS::fractions(xi))),
           xi = parse_factor(xi)) %>%
    filter(n == 10) %>%
    ggplot(mapping = aes(x = pm_alpha, fill = `Parent Genotypes`)) +
    geom_histogram(bins = 40) +
    facet_grid(alpha ~ xi, labeller = label_parsed) +
    xlab("Posterior Mean") +
    ylab("Frequency") +
    scale_fill_manual(values = girlboss_palette("elf_bar"), name = "Parent\nGenotypes") +
    theme_bw() +
    theme(strip.background = element_rect(fill = "white"))

ggsave("null_g_pmalpha_n10_hist_6823.pdf", plot = last_plot(), width = 6, height = 6, units = "in", device = "pdf", path = "./output")

  ## g, pm alpha, n == 100
  null_g %>%
    mutate(n = as.factor(n),
           `Parent Genotypes` = paste0("(", p1, ",", p2, ")"),
           alpha = paste0("alpha==", as.character(MASS::fractions(alpha))),
           xi = paste0("xi==", as.character(MASS::fractions(xi))),
           xi = parse_factor(xi)) %>%
    filter(n == 100) %>%
    ggplot(mapping = aes(x = pm_alpha, fill = `Parent Genotypes`)) +
    geom_histogram(bins = 40) +
    facet_grid(alpha ~ xi, labeller = label_parsed) +
    xlab("Posterior Mean") +
    ylab("Frequency") +
    scale_fill_manual(values = girlboss_palette("elf_bar"), name = "Parent\nGenotypes") +
    theme_bw() +
    theme(strip.background = element_rect(fill = "white"))

  ggsave("null_g_pmalpha_n100_hist_6823.pdf", plot = last_plot(), width = 6, height = 6, units = "in", device = "pdf", path = "./output")

  ## g, pm alpha, n == 1000
  null_g %>%
    mutate(n = as.factor(n),
           `Parent Genotypes` = paste0("(", p1, ",", p2, ")"),
           alpha = paste0("alpha==", as.character(MASS::fractions(alpha))),
           xi = paste0("xi==", as.character(MASS::fractions(xi))),
           xi = parse_factor(xi)) %>%
    filter(n == 1000) %>%
    ggplot(mapping = aes(x = pm_alpha, fill = `Parent Genotypes`)) +
    geom_histogram(bins = 40) +
    facet_grid(alpha ~ xi, labeller = label_parsed) +
    xlab("Posterior Mean") +
    ylab("Frequency") +
    scale_fill_manual(values = girlboss_palette("elf_bar"), name = "Parent\nGenotypes") +
    theme_bw() +
    theme(strip.background = element_rect(fill = "white"))

  ggsave("null_g_pmalpha_n1000_hist_6823.pdf", plot = last_plot(), width = 6, height = 6, units = "in", device = "pdf", path = "./output")


  ## g, pm xi, n == 10
  null_g %>%
    mutate(n = as.factor(n),
           `Parent Genotypes` = paste0("(", p1, ",", p2, ")"),
           alpha = paste0("alpha==", as.character(MASS::fractions(alpha))),
           xi = paste0("xi==", as.character(MASS::fractions(xi))),
           xi = parse_factor(xi)) %>%
    filter(n == 10) %>%
    ggplot(mapping = aes(x = pm_xi, fill = `Parent Genotypes`)) +
    geom_histogram(bins = 40) +
    facet_grid(alpha ~ xi, labeller = label_parsed) +
    xlab("Posterior Mean") +
    ylab("Frequency") +
    scale_fill_manual(values = girlboss_palette("elf_bar"), name = "Parent\nGenotypes") +
    theme_bw() +
    theme(strip.background = element_rect(fill = "white"))

  ## g, pm xi, n == 100
  null_g %>%
    mutate(n = as.factor(n),
           `Parent Genotypes` = paste0("(", p1, ",", p2, ")"),
           alpha = paste0("alpha==", as.character(MASS::fractions(alpha))),
           xi = paste0("xi==", as.character(MASS::fractions(xi))),
           xi = parse_factor(xi)) %>%
    filter(n == 100) %>%
    ggplot(mapping = aes(x = pm_xi, fill = `Parent Genotypes`)) +
    geom_histogram(bins = 40) +
    facet_grid(alpha ~ xi, labeller = label_parsed) +
    xlab("Posterior Mean") +
    ylab("Frequency") +
    scale_fill_manual(values = girlboss_palette("elf_bar"), name = "Parent\nGenotypes") +
    theme_bw() +
    theme(strip.background = element_rect(fill = "white"))

  ## g, pm xi, n == 1000
  null_g %>%
    mutate(n = as.factor(n),
           `Parent Genotypes` = paste0("(", p1, ",", p2, ")"),
           alpha = paste0("alpha==", as.character(MASS::fractions(alpha))),
           xi = paste0("xi==", as.character(MASS::fractions(xi))),
           xi = parse_factor(xi)) %>%
    filter(n == 1000) %>%
    ggplot(mapping = aes(x = pm_xi, fill = `Parent Genotypes`)) +
    geom_histogram(bins = 40) +
    facet_grid(alpha ~ xi, labeller = label_parsed) +
    xlab("Posterior Mean") +
    ylab("Frequency") +
    scale_fill_manual(values = girlboss_palette("elf_bar"), name = "Parent\nGenotypes") +
    theme_bw() +
    theme(strip.background = element_rect(fill = "white"))
