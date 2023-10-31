library(dplyr)

#### Data
null_dist <- read.table("path to null ranks in txt form", header = TRUE, sep = "\t")
sig_dist <- read.table("path to trait-associated gene ranks in txt form", header = TRUE, sep="\t")

null_dist_ranks <- null_dist %>%
  select(matches("^rank"))

null_dist_ranks_mean <- colMeans(null_dist_ranks)


null_dist_ranks_median <- apply(null_dist_ranks,2,median)


Plots - visualization

par(mfrow=c(2,2))
boxplot(null_dist_ranks_mean, main="Ranks of the null distribution", xlab="Rank (Null)")
boxplot(sig_dist$rank, main="Ranks of the significant genes", xlab="Rank (Sig. Genes)")
hist(null_dist_ranks_mean, main="Ranks of the null distribution", xlab="Rank (Null)")
hist(sig_dist$rank, main="Ranks of the significant genes", xlab="Rank (Sig. Genes)")


## Randomization tests
hist(null_dist_ranks_mean, breaks=30,  main="Randomzation - Mean", xlab='')
abline(v=mean(sig_dist$rank), lwd=2, col="red")
perm_pval_mean <- sum((null_dist_ranks_mean) <= (mean(sig_dist$rank)))/500
perm_pval_mean

hist(null_dist_ranks_median, breaks=30,  main="Randomzation - Median", xlab='')
abline(v=median(sig_dist$rank), lwd=2, col="red")
perm_pval_median <- sum((null_dist_ranks_median) <= (median(sig_dist$rank)))/500
perm_pval_median
