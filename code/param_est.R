library(tidyverse)
library(ggthemes)
library(latex2exp)
sdf <- read_csv("./output/sims/null_sims_g.csv")

sdf %>%
  mutate(n = as.factor(n),
         `Parent Genotypes` = paste0("(", p1, ",", p2, ")"),
         alpha_text = paste0("alpha==", as.character(MASS::fractions(alpha))),
         xi_text = paste0("xi==", as.character(MASS::fractions(xi))),
         xi_text = parse_factor(xi_text)) ->
  sumdf

sumdf %>%
  group_by(xi_text, alpha_text, alpha) %>%
  summarize(alpha = unique(alpha)) ->
  hlinedf

## Only include scenarios where identified
sumdf %>%
  filter(p1 != 2, p2 != 2) %>%
  ggplot(aes(x = n, y = pm_alpha, color = `Parent Genotypes`)) +
  facet_grid(alpha_text ~ xi_text, labeller = label_parsed) +
  geom_boxplot() +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white")) +
  scale_color_colorblind() +
  xlab("Sample Size") +
  ylab(TeX("Posterior Mean of $\\alpha$")) +
  geom_hline(data = hlinedf, mapping = aes(yintercept = alpha), lty = 2)

## Having other parent be 1 still doesn't seem to help
sumdf %>%
  group_by(xi_text, alpha_text, xi) %>%
  summarize(xi = unique(xi)) ->
  hlinedf

sumdf %>%
  filter((p1 == 1 & p2 == 2)) %>%
  ggplot(aes(x = n, y = pm_xi, color = `Parent Genotypes`)) +
  facet_grid(alpha_text ~ xi_text, labeller = label_parsed) +
  geom_boxplot() +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white")) +
  scale_color_colorblind() +
  xlab("Sample Size") +
  ylab(TeX("Posterior Mean of $\\xi$")) +
  geom_hline(data = hlinedf, mapping = aes(yintercept = xi), lty = 2)
