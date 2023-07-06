#####################
### Blueberry EDA ###
#####################

## Libraries

#library(menbayes)
library(tidyverse)
#devtools::install_github("thakkar-mira/girlboss")
library(girlboss)
library(updog)
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
library(ggpubr)

## Load Data
blue_df <- read.csv("./output/blue/blue_df.csv")

bluefits <- readRDS("./output/blue/bluefits.RDS")

######################################################################################
## Do most of the SNPs suggest conformity with mendelian segregation? (they should) ##
######################################################################################

# Histogram of logbf
blue_df %>%
  mutate(`Parent Genotypes` = paste0("(", p1, ",", p2, ")")) %>%
  ggplot(mapping = aes(x = logbf)) +
  geom_histogram(color = "black", fill ="white") +
  xlab("Log-Bayes Factor") +
  ylab("Count") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white")) +
  geom_vline(xintercept = 0, lty = 2)

ggsave("blue_logbf_hist.pdf", plot = last_plot(), width = 4, height = 3, units = "in", path = "./output")

##########################################################
## How does the chi-squared test comparatively perform? ##
##########################################################

blue_df %>%
  mutate(p1 = as.factor(p1),
         p2 = as.factor(p2)) %>%
  filter(is.finite(chisq_stat)) %>%
  ggplot(aes(sample=chisq_pvalue)) +
  geom_qq(size = 2, distribution = qunif) +
  geom_abline(slope = 1, intercept = 0) +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white"))

ggsave("blue_qq.pdf", plot = last_plot(), width = 3, height = 3, units = "in", path = "./output")

######################################################################
## What about the SNPs that seem to suggest segregation distortion? ##
######################################################################
##################################################################
## Plot the raw data from those (using updog::plot.multidog()). ##
## Does there appear to be systematic genotyping errors? #########
##################################################################

# SNPs that don't suggest conformity with Mendelian segregation (log-BF <= 0)
dist_df <- blue_df %>%
  filter(logbf <= 0) %>%
  select(snp, logbf) %>%
  distinct() %>%
  arrange(logbf) %>%
  slice(1:5)

blue_sub <- filter_snp(x = bluefits, expr = snp %in% dist_df$snp[[1]]) # only keeps updog fits for filtered out snps

lg <- get_legend(plot(blue_sub, indices = 1)[[1]] + guides(alpha = "none"))

pl1 <- plot(blue_sub, indices = 1)[[1]] +
  theme(legend.position="none")

blue_sub <- filter_snp(x = bluefits, expr = snp %in% dist_df$snp[[2]]) # only keeps updog fits for filtered out snps

pl2 <- plot(blue_sub, indices = 1)[[1]] +
  theme(legend.position="none")

blue_sub <- filter_snp(x = bluefits, expr = snp %in% dist_df$snp[[3]]) # only keeps updog fits for filtered out snps

pl3 <- plot(blue_sub, indices = 1)[[1]] +
  theme(legend.position="none")

blue_sub <- filter_snp(x = bluefits, expr = snp %in% dist_df$snp[[4]]) # only keeps updog fits for filtered out snps

pl4 <- plot(blue_sub, indices = 1)[[1]] +
  theme(legend.position="none")

blue_sub <- filter_snp(x = bluefits, expr = snp %in% dist_df$snp[[5]]) # only keeps updog fits for filtered out snps

pl5 <- plot(blue_sub, indices = 1)[[1]] +
  theme(legend.position="none")

pdf(file = "./output/blue/bad_snps.pdf", width = 6, height = 7, family = "Times")
grid.arrange(pl1, pl2, pl3, pl4, pl5, lg, ncol = 2)
dev.off()

#SNPs with High log-BF but low p-value

lbfp_df <- blue_df %>%
  filter(logbf >= 5 & chisq_pvalue <= 0.05) %>%
  filter(chisq_pvalue > 0) %>%
  select(snp, logbf, chisq_pvalue) %>%
  distinct() %>%
  arrange(desc(logbf)) %>%
  slice(1:5)

lbfp_sub <- filter_snp(x = bluefits, expr = snp %in% lbfp_df$snp)

lg <- get_legend(plot(lbfp_sub, indices = 1)[[1]] + guides(alpha = "none"))

p1 <- plot(lbfp_sub, indices = 1)[[1]] +
  theme(legend.position="none")

p2 <- plot(lbfp_sub, indices = 2)[[1]] +
  theme(legend.position="none")

p3 <- plot(lbfp_sub, indices = 3)[[1]] +
  theme(legend.position="none")

p4 <- plot(lbfp_sub, indices = 4)[[1]] +
  theme(legend.position="none")

p5 <- plot(lbfp_sub, indices = 5)[[1]] +
  theme(legend.position="none")

pdf(file = "./output/blue/bad_diff.pdf", width = 6, height = 7, family = "Times")
grid.arrange(p1, p2, p3, p4, p5, lg, ncol = 2)
dev.off()

##########################################################################################################
## Plot the double reduction rate vs chromosome position (using only 0/1/3/4 parental genotype combos). ##
## Apply a smoother. Does it seem to decrease toward the centromere? #####################################
##########################################################################################################

#names of snps are positions. chromosomenumber_position
#don't use parental genotypes = 2

blue_2 <- blue_df %>%
  filter(p1 != 2) %>%
  filter(p2 != 2)

#took out chromosome number since they're all on chromosome 1
blue_2$snp <- as.numeric(sub("1_", "", blue_2$snp))

ggplot(data = blue_2, mapping = aes(x = snp, y = pm_alpha)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  xlab("SNP") +
  ylab("Double Reduction Rate") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white"))

ggsave("chr_dr_scatter.pdf", plot = last_plot(), width = 3, height = 3, units = "in", path = "./output")
