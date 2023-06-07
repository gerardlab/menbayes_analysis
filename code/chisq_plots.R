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


## Genotypes Known - Uniform Chi Sq Plots

null_g %>%
  mutate(n = as.factor(n),
         alpha = paste0("alpha==", as.character(MASS::fractions(alpha))),
         alpha = parse_factor(alpha),
         xi = paste0("xi==", as.character(MASS::fractions(xi))),
         xi = parse_factor(xi),
         p1 = as.factor(p1),
         p2 = as.factor(p2)) %>%
  filter(p1 == 0 & p2 == 1) %>%
ggplot(aes(sample=chisq_pvalue, color = n)) +
    geom_qq(size = 2, distribution = qunif) +
    geom_abline(slope = 1, intercept = 0) +
    facet_grid(alpha ~ xi, labeller = label_parsed) +
    xlab("Theoretical Quantiles") +
    ylab("Sample Quantiles") +
    scale_color_manual(values = girlboss_palette("elf_bar")) +
    theme_bw() +
    theme(strip.background = element_rect(fill = "white"))

ggsave("ucsq_p1_0_p2_1_6623.pdf", plot = last_plot(), width = 6, height = 6, units = "in", device = "pdf", path = "./output")

null_g %>%
  mutate(n = as.factor(n),
         alpha = paste0("alpha==", as.character(MASS::fractions(alpha))),
         alpha = parse_factor(alpha),
         xi = paste0("xi==", as.character(MASS::fractions(xi))),
         xi = parse_factor(xi),
         p1 = as.factor(p1),
         p2 = as.factor(p2)) %>%
  filter(p1 == 0 & p2 == 2) %>%
  ggplot(aes(sample=chisq_pvalue, color = n)) +
  geom_qq(size = 2, distribution = qunif) +
  geom_abline(slope = 1, intercept = 0) +
  facet_grid(alpha ~ xi, labeller = label_parsed) +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles") +
  scale_color_manual(values = girlboss_palette("elf_bar")) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white"))

ggsave("ucsq_p1_0_p2_2_6623.pdf", plot = last_plot(), width = 6, height = 6, units = "in", device = "pdf", path = "./output")

null_g %>%
  mutate(n = as.factor(n),
         alpha = paste0("alpha==", as.character(MASS::fractions(alpha))),
         alpha = parse_factor(alpha),
         xi = paste0("xi==", as.character(MASS::fractions(xi))),
         xi = parse_factor(xi),
         p1 = as.factor(p1),
         p2 = as.factor(p2)) %>%
  filter(p1 == 1 & p2 == 1) %>%
  ggplot(aes(sample=chisq_pvalue, color = n)) +
  geom_qq(size = 2, distribution = qunif) +
  geom_abline(slope = 1, intercept = 0) +
  facet_grid(alpha ~ xi, labeller = label_parsed) +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles") +
  scale_color_manual(values = girlboss_palette("elf_bar")) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white"))

ggsave("ucsq_p1_1_p2_1_6623.pdf", plot = last_plot(), width = 6, height = 6, units = "in", device = "pdf", path = "./output")


null_g %>%
  mutate(n = as.factor(n),
         alpha = paste0("alpha==", as.character(MASS::fractions(alpha))),
         alpha = parse_factor(alpha),
         xi = paste0("xi==", as.character(MASS::fractions(xi))),
         xi = parse_factor(xi),
         p1 = as.factor(p1),
         p2 = as.factor(p2)) %>%
  filter(p1 == 1 & p2 == 2) %>%
  ggplot(aes(sample=chisq_pvalue, color = n)) +
  geom_qq(size = 2, distribution = qunif) +
  geom_abline(slope = 1, intercept = 0) +
  facet_grid(alpha ~ xi, labeller = label_parsed) +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles") +
  scale_color_manual(values = girlboss_palette("elf_bar")) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white"))

ggsave("ucsq_p1_1_p2_2_6623.pdf", plot = last_plot(), width = 6, height = 6, units = "in", device = "pdf", path = "./output")

null_g %>%
  mutate(n = as.factor(n),
         alpha = paste0("alpha==", as.character(MASS::fractions(alpha))),
         alpha = parse_factor(alpha),
         xi = paste0("xi==", as.character(MASS::fractions(xi))),
         xi = parse_factor(xi),
         p1 = as.factor(p1),
         p2 = as.factor(p2)) %>%
  filter(p1 == 2 & p2 == 2) %>%
  ggplot(aes(sample=chisq_pvalue, color = n)) +
  geom_qq(size = 2, distribution = qunif) +
  geom_abline(slope = 1, intercept = 0) +
  facet_grid(alpha ~ xi, labeller = label_parsed) +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles") +
  scale_color_manual(values = girlboss_palette("elf_bar")) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white"))

ggsave("ucsq_p1_2_p2_2_6623.pdf", plot = last_plot(), width = 6, height = 6, units = "in", device = "pdf", path = "./output")

## Genotype Likelihoods - Uniform Chi Sq Plots

#p1 = 0, p2 = 1, rd = 10
null_gl %>%
  mutate(n = as.factor(n),
         alpha = paste0("alpha==", as.character(MASS::fractions(alpha))),
         alpha = parse_factor(alpha),
         xi = paste0("xi==", as.character(MASS::fractions(xi))),
         xi = parse_factor(xi),
         p1 = as.factor(p1),
         p2 = as.factor(p2)) %>%
  filter(p1 == 0 & p2 == 1,
         rd == 10) %>%
  ggplot(aes(sample=chisq_pvalue, color = n)) +
  geom_qq(size = 2, distribution = qunif) +
  geom_abline(slope = 1, intercept = 0) +
  facet_grid(alpha ~ xi, labeller = label_parsed) +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles") +
  scale_color_manual(values = girlboss_palette("elf_bar")) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white"))

ggsave("ucsq_gl_rd10_p1_0_p2_1_6623.pdf", plot = last_plot(), width = 6, height = 6, units = "in", device = "pdf", path = "./output")

#p1 = 0, p2 = 1, rd = 100
null_gl %>%
  mutate(n = as.factor(n),
         alpha = paste0("alpha==", as.character(MASS::fractions(alpha))),
         alpha = parse_factor(alpha),
         xi = paste0("xi==", as.character(MASS::fractions(xi))),
         xi = parse_factor(xi),
         p1 = as.factor(p1),
         p2 = as.factor(p2)) %>%
  filter(p1 == 0 & p2 == 1,
         rd == 100) %>%
  ggplot(aes(sample=chisq_pvalue, color = n)) +
  geom_qq(size = 2, distribution = qunif) +
  geom_abline(slope = 1, intercept = 0) +
  facet_grid(alpha ~ xi, labeller = label_parsed) +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles") +
  scale_color_manual(values = girlboss_palette("elf_bar")) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white"))

ggsave("ucsq_gl_rd_100_p1_0_p2_1_6623.pdf", plot = last_plot(), width = 6, height = 6, units = "in", device = "pdf", path = "./output")

#p1 = 0, p2 = 2, rd = 10
null_gl %>%
  mutate(n = as.factor(n),
         alpha = paste0("alpha==", as.character(MASS::fractions(alpha))),
         alpha = parse_factor(alpha),
         xi = paste0("xi==", as.character(MASS::fractions(xi))),
         xi = parse_factor(xi),
         p1 = as.factor(p1),
         p2 = as.factor(p2)) %>%
  filter(p1 == 0 & p2 == 2,
         rd == 10) %>%
  ggplot(aes(sample=chisq_pvalue, color = n)) +
  geom_qq(size = 2, distribution = qunif) +
  geom_abline(slope = 1, intercept = 0) +
  facet_grid(alpha ~ xi, labeller = label_parsed) +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles") +
  scale_color_manual(values = girlboss_palette("elf_bar")) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white"))

ggsave("ucsq_gl_rd_10_p1_0_p2_2_6623.pdf", plot = last_plot(), width = 6, height = 6, units = "in", device = "pdf", path = "./output")

#p1 = 0, p2 = 2, rd = 100
null_gl %>%
  mutate(n = as.factor(n),
         alpha = paste0("alpha==", as.character(MASS::fractions(alpha))),
         alpha = parse_factor(alpha),
         xi = paste0("xi==", as.character(MASS::fractions(xi))),
         xi = parse_factor(xi),
         p1 = as.factor(p1),
         p2 = as.factor(p2)) %>%
  filter(p1 == 0 & p2 == 2,
         rd == 100) %>%
  ggplot(aes(sample=chisq_pvalue, color = n)) +
  geom_qq(size = 2, distribution = qunif) +
  geom_abline(slope = 1, intercept = 0) +
  facet_grid(alpha ~ xi, labeller = label_parsed) +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles") +
  scale_color_manual(values = girlboss_palette("elf_bar")) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white"))

ggsave("ucsq_gl_rd_100_p1_0_p2_2_6623.pdf", plot = last_plot(), width = 6, height = 6, units = "in", device = "pdf", path = "./output")

#p1 = 1, p2 = 1, rd = 10
null_gl %>%
  mutate(n = as.factor(n),
         alpha = paste0("alpha==", as.character(MASS::fractions(alpha))),
         alpha = parse_factor(alpha),
         xi = paste0("xi==", as.character(MASS::fractions(xi))),
         xi = parse_factor(xi),
         p1 = as.factor(p1),
         p2 = as.factor(p2)) %>%
  filter(p1 == 1 & p2 == 1,
         rd == 10) %>%
  ggplot(aes(sample=chisq_pvalue, color = n)) +
  geom_qq(size = 2, distribution = qunif) +
  geom_abline(slope = 1, intercept = 0) +
  facet_grid(alpha ~ xi, labeller = label_parsed) +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles") +
  scale_color_manual(values = girlboss_palette("elf_bar")) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white"))

ggsave("ucsq_gl_rd_10_p1_1_p2_1_6623.pdf", plot = last_plot(), width = 6, height = 6, units = "in", device = "pdf", path = "./output")

#p1 = 1, p2 = 1, rd = 100
null_gl %>%
  mutate(n = as.factor(n),
         alpha = paste0("alpha==", as.character(MASS::fractions(alpha))),
         alpha = parse_factor(alpha),
         xi = paste0("xi==", as.character(MASS::fractions(xi))),
         xi = parse_factor(xi),
         p1 = as.factor(p1),
         p2 = as.factor(p2)) %>%
  filter(p1 == 1 & p2 == 1,
         rd == 100) %>%
  ggplot(aes(sample=chisq_pvalue, color = n)) +
  geom_qq(size = 2, distribution = qunif) +
  geom_abline(slope = 1, intercept = 0) +
  facet_grid(alpha ~ xi, labeller = label_parsed) +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles") +
  scale_color_manual(values = girlboss_palette("elf_bar")) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white"))

ggsave("ucsq_gl_rd_100_p1_1_p2_1_6623.pdf", plot = last_plot(), width = 6, height = 6, units = "in", device = "pdf", path = "./output")

#p1 = 1, p2 = 2, rd = 10
null_gl %>%
  mutate(n = as.factor(n),
         alpha = paste0("alpha==", as.character(MASS::fractions(alpha))),
         alpha = parse_factor(alpha),
         xi = paste0("xi==", as.character(MASS::fractions(xi))),
         xi = parse_factor(xi),
         p1 = as.factor(p1),
         p2 = as.factor(p2)) %>%
  filter(p1 == 1 & p2 == 2,
         rd == 10) %>%
  ggplot(aes(sample=chisq_pvalue, color = n)) +
  geom_qq(size = 2, distribution = qunif) +
  geom_abline(slope = 1, intercept = 0) +
  facet_grid(alpha ~ xi, labeller = label_parsed) +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles") +
  scale_color_manual(values = girlboss_palette("elf_bar")) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white"))

ggsave("ucsq_gl_rd_10_p1_1_p2_2_6623.pdf", plot = last_plot(), width = 6, height = 6, units = "in", device = "pdf", path = "./output")

#p1 = 1, p2 = 2, rd = 100
null_gl %>%
  mutate(n = as.factor(n),
         alpha = paste0("alpha==", as.character(MASS::fractions(alpha))),
         alpha = parse_factor(alpha),
         xi = paste0("xi==", as.character(MASS::fractions(xi))),
         xi = parse_factor(xi),
         p1 = as.factor(p1),
         p2 = as.factor(p2)) %>%
  filter(p1 == 1 & p2 == 2,
         rd == 100) %>%
  ggplot(aes(sample=chisq_pvalue, color = n)) +
  geom_qq(size = 2, distribution = qunif) +
  geom_abline(slope = 1, intercept = 0) +
  facet_grid(alpha ~ xi, labeller = label_parsed) +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles") +
  scale_color_manual(values = girlboss_palette("elf_bar")) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white"))

ggsave("ucsq_gl_rd_100_p1_1_p2_2_6623.pdf", plot = last_plot(), width = 6, height = 6, units = "in", device = "pdf", path = "./output")

#p1 = 2, p2 = 2, rd = 10
null_gl %>%
  mutate(n = as.factor(n),
         alpha = paste0("alpha==", as.character(MASS::fractions(alpha))),
         alpha = parse_factor(alpha),
         xi = paste0("xi==", as.character(MASS::fractions(xi))),
         xi = parse_factor(xi),
         p1 = as.factor(p1),
         p2 = as.factor(p2)) %>%
  filter(p1 == 2 & p2 == 2,
         rd == 10) %>%
  ggplot(aes(sample=chisq_pvalue, color = n)) +
  geom_qq(size = 2, distribution = qunif) +
  geom_abline(slope = 1, intercept = 0) +
  facet_grid(alpha ~ xi, labeller = label_parsed) +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles") +
  scale_color_manual(values = girlboss_palette("elf_bar")) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white"))

ggsave("ucsq_gl_rd_10_p1_2_p2_2_6623.pdf", plot = last_plot(), width = 6, height = 6, units = "in", device = "pdf", path = "./output")

#p1 = 2, p2 = 2, rd = 100
null_gl %>%
  mutate(n = as.factor(n),
         alpha = paste0("alpha==", as.character(MASS::fractions(alpha))),
         alpha = parse_factor(alpha),
         xi = paste0("xi==", as.character(MASS::fractions(xi))),
         xi = parse_factor(xi),
         p1 = as.factor(p1),
         p2 = as.factor(p2)) %>%
  filter(p1 == 2 & p2 == 2,
         rd == 100) %>%
  ggplot(aes(sample=chisq_pvalue, color = n)) +
  geom_qq(size = 2, distribution = qunif) +
  geom_abline(slope = 1, intercept = 0) +
  facet_grid(alpha ~ xi, labeller = label_parsed) +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles") +
  scale_color_manual(values = girlboss_palette("elf_bar")) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white"))

ggsave("ucsq_gl_rd_100_p1_2_p2_2_6623.pdf", plot = last_plot(), width = 6, height = 6, units = "in", device = "pdf", path = "./output")
