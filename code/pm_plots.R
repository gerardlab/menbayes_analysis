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
alpha_df <- null_g %>%
  mutate(n = as.factor(n),
         `Parent Genotypes` = paste0("(", p1, ",", p2, ")"),
         xi = paste0("xi==", as.character(MASS::fractions(xi))),
         xi = parse_factor(xi),
         alpha_num = alpha,
         alpha = paste0("alpha==", as.character(MASS::fractions(alpha))))%>%
  filter((p1 == 0 & p2 == 1) |
         (p1 == 1 & p2 == 1))

facet_df <- alpha_df %>%
  select(alpha_num, alpha, xi) %>%
  distinct()

ggplot(data = alpha_df, mapping = aes(x = n, y = pm_alpha, fill = `Parent Genotypes`)) +
  geom_boxplot() +
  geom_hline(data = facet_df, mapping = aes(yintercept = alpha_num), linetype = "dashed") +
  facet_grid(alpha ~ xi, labeller = label_parsed) +
  xlab("Sample Size") +
  ylab("Posterior Mean") +
  scale_fill_manual(values = girlboss_palette("elf_bar"), name = "Parent\nGenotypes") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white"))

ggsave("null_g_pmalpha_box_61923.pdf", plot = last_plot(), width = 6, height = 6, units = "in", device = "pdf", path = "./output")

#' Null, genotype likelihoods: When running the plots below, it looks like the log-BFs and posterior means of alpha
#' are the same for all values of xi. It looks like it's because of the seeds in the dataset (?).
#' Not sure how to filter (or if I should at all)

alpha_df_gl <- null_gl %>%
  mutate(n = as.factor(n),
         `Parent Genotypes` = paste0("(", p1, ",", p2, ")"),
         xi = paste0("xi==", as.character(MASS::fractions(xi))),
         xi = parse_factor(xi),
         alpha_num = alpha,
         alpha = paste0("alpha==", as.character(MASS::fractions(alpha)))) %>%
  filter((p1 == 0 & p2 == 1) |
         (p1 == 1 & p2 == 1))

facet_df_gl <- alpha_df_gl %>%
  select(alpha_num, alpha, xi) %>%
  distinct()

# rd == 10

alpha_df_gl %>%
  filter(xi == 0) %>%
ggplot(mapping = aes(x = n, y = pm_alpha, fill = `Parent Genotypes`)) +
  geom_boxplot() +
  geom_hline(data = facet_df_gl, mapping = aes(yintercept = alpha_num), linetype = "dashed") +
  facet_grid(alpha ~ rd, labeller = label_parsed) +
  xlab("Sample Size") +
  ylab("Posterior Mean") +
  scale_fill_manual(values = girlboss_palette("elf_bar"), name = "Parent\nGenotypes") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white"))

ggsave("null_gl_pmalpha_rd10_box_61923.pdf", plot = last_plot(), width = 6, height = 6, units = "in", device = "pdf", path = "./output")


#rd == 100

alpha_df_gl %>%
  filter(rd == 100) %>%
ggplot(data = alpha_df_gl, mapping = aes(x = n, y = pm_alpha, fill = `Parent Genotypes`)) +
  geom_boxplot() +
  geom_hline(data = facet_df_gl, mapping = aes(yintercept = alpha_num), linetype = "dashed") +
  facet_grid(alpha ~ xi, labeller = label_parsed) +
  xlab("Sample Size") +
  ylab("Posterior Mean") +
  scale_fill_manual(values = girlboss_palette("elf_bar"), name = "Parent\nGenotypes") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white"))

ggsave("null_gl_pmalpha_rd100_box_61923.pdf", plot = last_plot(), width = 6, height = 6, units = "in", device = "pdf", path = "./output")


# just on
