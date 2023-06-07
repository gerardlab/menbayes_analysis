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
