library(tidyverse)
#devtools::install_github("thakkar-mira/girlboss")
library(girlboss)
null_g <- read.csv("./output/sims/null_sims_g.csv")
null_gl <- read.csv("./output/sims/null_sims_gl.csv")

#PM ALPHA PLOTS

#Null, genotypes known
alpha_df <- null_g %>%
  mutate(n = as.factor(n),
         `Parent Genotypes` = paste0("(", p1, ",", p2, ")"),
         alpha_num = alpha,
         alpha = paste0("alpha==", as.character(MASS::fractions(alpha))))%>%
  filter((p1 == 0 & p2 == 1) |
         (p1 == 1 & p2 == 1)) %>%
  filter(xi == 0)

facet_df <- alpha_df %>%
  select(alpha_num, alpha) %>%
  distinct()

ggplot(data = alpha_df, mapping = aes(x = n, y = pm_alpha, fill = `Parent Genotypes`)) +
  geom_boxplot() +
  geom_hline(data = facet_df, mapping = aes(yintercept = alpha_num), linetype = "dashed") +
  facet_wrap(~ alpha, labeller = label_parsed) +
  xlab("Sample Size") +
  ylab("Posterior Mean") +
  scale_fill_manual(values = girlboss_palette("elf_bar"), name = "Parent\nGenotypes") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white"))

ggsave("./output/sims/pm/null_g_pmalpha_box.pdf", plot = last_plot(), width = 6, height = 2, units = "in")

#Null, genotype likelihoods

alpha_df_gl <- null_gl %>%
  mutate(n = as.factor(n),
         `Parent Genotypes` = paste0("(", p1, ",", p2, ")"),
         rd = paste0("rd==", rd),
         rd = parse_factor(rd),
         alpha_num = alpha,
         alpha = paste0("alpha==", as.character(MASS::fractions(alpha)))) %>%
  filter((p1 == 0 & p2 == 1) |
         (p1 == 1 & p2 == 1)) %>%
  filter(xi == 0)

facet_df_gl <- alpha_df_gl %>%
  select(alpha_num, alpha, rd) %>%
  distinct()

ggplot(data = alpha_df_gl, mapping = aes(x = n, y = pm_alpha, fill = `Parent Genotypes`)) +
  geom_boxplot() +
  geom_hline(data = facet_df_gl, mapping = aes(yintercept = alpha_num), linetype = "dashed") +
  facet_grid(alpha ~ rd, labeller = label_parsed) +
  xlab("Sample Size") +
  ylab("Posterior Mean") +
  scale_fill_manual(values = girlboss_palette("elf_bar"), name = "Parent\nGenotypes") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white"))

ggsave("./output/sims/pm/null_gl_pmalpha_box.pdf", plot = last_plot(), width = 6, height = 6, units = "in")
