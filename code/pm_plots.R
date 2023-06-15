## Libraries

#library(menbayes)
library(tidyverse)
#devtools::install_github("thakkar-mira/girlboss")
library(girlboss)

## Load Data
null_g <- read.csv("./output/sims/null_sims_g.csv")

null_gl <- read.csv("./output/sims/null_sims_gl.csv")

#PM ALPHA ANALYSIS

#Null, genotypes known
null_g %>%
  mutate(n = as.factor(n),
         `Parent Genotypes` = paste0("(", p1, ",", p2, ")"),
         alpha = paste0("alpha==", as.character(MASS::fractions(alpha))))%>%
  filter((p1 == 0 & p2 == 1) |
         (p1 == 1 & p2 == 1)) %>%
  ggplot(mapping = aes(x = pm_alpha, fill = `Parent Genotypes`)) +
  geom_boxplot() +
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

