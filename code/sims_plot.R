library(tidyverse)
library(ggthemes)

sdf <- read_csv("./output/sims/null_sims_g.csv")

sdf %>%
  mutate(n = as.factor(n),
         `Parent Genotypes` = paste0("(", p1, ",", p2, ")"),
         alpha = paste0("alpha==", as.character(MASS::fractions(alpha))),
         xi = paste0("xi==", as.character(MASS::fractions(xi)))) %>%
  ggplot(aes(x = n, y = logbf, color = `Parent Genotypes`)) +
  facet_grid(alpha ~ xi, labeller = label_parsed) +
  geom_boxplot() +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white")) +
  geom_hline(yintercept = 0, lty = 2) +
  scale_color_colorblind() +
  xlab("Sample Size") +
  ylab("Log Bayes Factor")
