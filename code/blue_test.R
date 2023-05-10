library(updog)

bluefits <- readRDS("./output/blue/bluefits.RDS")

## This will give you a array with dimensions SNPs by Individuals by Genotypes
gl <- format_multidog(bluefits, varname = paste0("logL_", 0:4))
p1vec <- bluefits$snpdf$ell1
p2vec <- bluefits$snpdf$ell2

for (i in seq_len(dim(gl)[[1]])) {
  glmat <- gl[i , ,]
  p1 <- p1vec[i]
  p2 <- p2vec[i]


}

#chisq segdis genotypes known
