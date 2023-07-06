library(tidyverse)
#devtools::install_github("thakkar-mira/girlboss")
library(girlboss)
alt_g <- readRDS("./output/sims/alt_sims_g.RDS")
alt_gl <- readRDS("./output/sims/alt_sims_gl.RDS")
null_g <- read.csv("./output/sims/null_sims_g.csv")
null_gl <- read.csv("./output/sims/null_sims_gl.csv")


xi_df <- null_g %>%
  mutate(xi = paste0("xi==", as.character(MASS::fractions(xi)))) %>%
  select(xi) %>%
  distinct() %>%
  unlist()

## Genotypes Known

null_g %>%
  filter(xi == 1/3) %>%
  mutate(n = as.factor(n),
         alpha = paste0("alpha==", as.character(MASS::fractions(alpha))),
         alpha = parse_factor(alpha),
         xi = paste0("xi==", as.character(MASS::fractions(xi))),
         xi = parse_factor(xi, levels = xi_df),
         p1 = as.factor(p1),
         p2 = as.factor(p2)) %>%
  filter(p1 == 0 & p2 == 1) %>%
  ggplot(aes(sample=chisq_pvalue, color = n)) +
    geom_qq(size = 2, distribution = qunif) +
    geom_abline(slope = 1, intercept = 0) +
    facet_wrap(~alpha, labeller = label_parsed) +
    xlab("Theoretical Quantiles") +
    ylab("Sample Quantiles") +
    scale_color_manual(values = girlboss_palette("elf_bar")) +
    theme_bw() +
    theme(strip.background = element_rect(fill = "white"))

ggsave("./output/sims/chisq/ucsq_g_01.pdf", plot = last_plot(), width = 6, height = 4, units = "in")

null_g %>%
  mutate(n = as.factor(n),
         alpha = paste0("alpha==", as.character(MASS::fractions(alpha))),
         alpha = parse_factor(alpha),
         xi = paste0("xi==", as.character(MASS::fractions(xi))),
         xi = parse_factor(xi, levels = xi_df),
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

ggsave("./output/sims/chisq/ucsq_g_02.pdf", plot = last_plot(), width = 6, height = 6, units = "in")

null_g %>%
  filter(xi == 1/3) %>%
  mutate(n = as.factor(n),
         alpha = paste0("alpha==", as.character(MASS::fractions(alpha))),
         alpha = parse_factor(alpha),
         xi = paste0("xi==", as.character(MASS::fractions(xi))),
         xi = parse_factor(xi, levels = xi_df),
         p1 = as.factor(p1),
         p2 = as.factor(p2)) %>%
  filter(p1 == 1 & p2 == 1) %>%
  ggplot(aes(sample=chisq_pvalue, color = n)) +
  geom_qq(size = 2, distribution = qunif) +
  geom_abline(slope = 1, intercept = 0) +
  facet_grid(~ alpha, labeller = label_parsed) +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles") +
  scale_color_manual(values = girlboss_palette("elf_bar")) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white"))

ggsave("./output/sims/chisq/ucsq_g_11.pdf", plot = last_plot(), width = 6, height = 6, units = "in")


null_g %>%
  mutate(n = as.factor(n),
         alpha = paste0("alpha==", as.character(MASS::fractions(alpha))),
         alpha = parse_factor(alpha),
         xi = paste0("xi==", as.character(MASS::fractions(xi))),
         xi = parse_factor(xi, levels = xi_df),
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

ggsave("./output/sims/chisq/ucsq_g_12.pdf", plot = last_plot(), width = 6, height = 6, units = "in")

null_g %>%
  mutate(n = as.factor(n),
         alpha = paste0("alpha==", as.character(MASS::fractions(alpha))),
         alpha = parse_factor(alpha),
         xi = paste0("xi==", as.character(MASS::fractions(xi))),
         xi = parse_factor(xi, levels = xi_df),
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

ggsave("./output/sims/chisq/ucsq_g_22.pdf", plot = last_plot(), width = 6, height = 6, units = "in")

#################################################
## Genotype Likelihoods - Uniform Chi Sq Plots ##
#################################################

#p1 = 0, p2 = 1, rd = 10
null_gl %>%
  filter(xi > 1/5) %>%
  mutate(n = as.factor(n),
         alpha = paste0("alpha==", as.character(MASS::fractions(alpha))),
         alpha = parse_factor(alpha),
         xi = paste0("xi==", as.character(MASS::fractions(xi))),
         xi = parse_factor(xi, levels = xi_df),
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

ggsave("./output/sims/chisq/ucsq_gl_rd10_01.pdf", plot = last_plot(), width = 6, height = 6, units = "in")

#p1 = 0, p2 = 1, rd = 100
null_gl %>%
  filter(xi > 1/5) %>%
  mutate(n = as.factor(n),
         alpha = paste0("alpha==", as.character(MASS::fractions(alpha))),
         alpha = parse_factor(alpha),
         xi = paste0("xi==", as.character(MASS::fractions(xi))),
         xi = parse_factor(xi, levels = xi_df),
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

ggsave("./output/sims/chisq/ucsq_gl_rd100_01.pdf", plot = last_plot(), width = 6, height = 6, units = "in")

#p1 = 0, p2 = 2, rd = 10
null_gl %>%
  mutate(n = as.factor(n),
         alpha = paste0("alpha==", as.character(MASS::fractions(alpha))),
         alpha = parse_factor(alpha),
         xi = paste0("xi==", as.character(MASS::fractions(xi))),
         xi = parse_factor(xi, levels = xi_df),
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

ggsave("./output/sims/chisq/ucsq_gl_rd10_02.pdf", plot = last_plot(), width = 6, height = 6, units = "in")

#p1 = 0, p2 = 2, rd = 100
null_gl %>%
  mutate(n = as.factor(n),
         alpha = paste0("alpha==", as.character(MASS::fractions(alpha))),
         alpha = parse_factor(alpha),
         xi = paste0("xi==", as.character(MASS::fractions(xi))),
         xi = parse_factor(xi, levels = xi_df),
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

ggsave("./output/sims/chisq/ucsq_gl_rd100_02.pdf", plot = last_plot(), width = 6, height = 6, units = "in")

#p1 = 1, p2 = 1, rd = 10
null_gl %>%
  filter(xi > 1/5) %>%
  mutate(n = as.factor(n),
         alpha = paste0("alpha==", as.character(MASS::fractions(alpha))),
         alpha = parse_factor(alpha),
         xi = paste0("xi==", as.character(MASS::fractions(xi))),
         xi = parse_factor(xi, levels = xi_df),
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

ggsave("./output/sims/chisq/ucsq_gl_rd10_11.pdf", plot = last_plot(), width = 6, height = 6, units = "in")

#p1 = 1, p2 = 1, rd = 100
null_gl %>%
  filter(xi > 1/5) %>%
  mutate(n = as.factor(n),
         alpha = paste0("alpha==", as.character(MASS::fractions(alpha))),
         alpha = parse_factor(alpha),
         xi = paste0("xi==", as.character(MASS::fractions(xi))),
         xi = parse_factor(xi, levels = xi_df),
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

ggsave("./output/sims/chisq/ucsq_gl_rd100_11.pdf", plot = last_plot(), width = 6, height = 6, units = "in")

#p1 = 1, p2 = 2, rd = 10
null_gl %>%
  mutate(n = as.factor(n),
         alpha = paste0("alpha==", as.character(MASS::fractions(alpha))),
         alpha = parse_factor(alpha),
         xi = paste0("xi==", as.character(MASS::fractions(xi))),
         xi = parse_factor(xi, levels = xi_df),
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

ggsave("./output/sims/chisq/ucsq_gl_rd10_12.pdf", plot = last_plot(), width = 6, height = 6, units = "in")

#p1 = 1, p2 = 2, rd = 100
null_gl %>%
  mutate(n = as.factor(n),
         alpha = paste0("alpha==", as.character(MASS::fractions(alpha))),
         alpha = parse_factor(alpha),
         xi = paste0("xi==", as.character(MASS::fractions(xi))),
         xi = parse_factor(xi, levels = xi_df),
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

ggsave("./output/sims/chisq/ucsq_gl_rd100_12.pdf", plot = last_plot(), width = 6, height = 6, units = "in")

#p1 = 2, p2 = 2, rd = 10
null_gl %>%
  mutate(n = as.factor(n),
         alpha = paste0("alpha==", as.character(MASS::fractions(alpha))),
         alpha = parse_factor(alpha),
         xi = paste0("xi==", as.character(MASS::fractions(xi))),
         xi = parse_factor(xi, levels = xi_df),
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

ggsave("./output/sims/chisq/ucsq_gl_rd10_22.pdf", plot = last_plot(), width = 6, height = 6, units = "in")

#p1 = 2, p2 = 2, rd = 100
null_gl %>%
  mutate(n = as.factor(n),
         alpha = paste0("alpha==", as.character(MASS::fractions(alpha))),
         alpha = parse_factor(alpha),
         xi = paste0("xi==", as.character(MASS::fractions(xi))),
         xi = parse_factor(xi, levels = xi_df),
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

ggsave("./output/sims/chisq/ucsq_gl_rd100_22.pdf", plot = last_plot(), width = 6, height = 6, units = "in")
