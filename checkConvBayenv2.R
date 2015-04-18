### 
# klott@hulk
# cd /data/seqcap/pine/bwa_pseudo/round2_bams/final_tables_GATK3/bayenv2$ 

# VAR-COVAR MATRIX: ITER = 88500

#a1 <- system("grep -n 'VAR-COVAR MATRIX: ITER = 88000' *covmatv1", intern=TRUE)
#a2 <- system("grep -n 'VAR-COVAR MATRIX: ITER = 100' *covmatv1", intern=TRUE)
#a3 <- system("grep -n 'VAR-COVAR MATRIX: ITER = 500' *covmatv1", intern=TRUE)

a4 <- system("grep -n 'VAR-COVAR MATRIX: ITER = ' *covmatv1", intern=TRUE)
b <-strsplit(a4, ":")
line <- sapply(b, "[[", 1)
iter <- sapply(b, "[[", 3)
iter2 <- strsplit(iter, " ")
iter3 <- as.numeric(sapply(iter2, "[[", 4))


#### Assess convergence in first replicate ### 
f1 <- "var_out_GATK3_pine688_bayenv.covmatv1"
i=1
  m1 <- scan(f1, skip=line[(length(line)-1)], nlines=278)
  length(m1)==(278*278)

cor_i <- c()
for (i in 1:(length(line)-1)){ # minus two because of last one is writing
  m2 <- scan(f1, skip=line[i], nlines=278)
  length(m2)==(278*278)
  cor_i[i] <- cor.test(m1, m2)$est
}
cor_i[length(line)] <- NA
length(cor_i)

write.table(data.frame(line, iter3, cor_i), "../var_out_GATK3_pine688_bayenv.covmatv1.converge")

#### Assess correlations between 3 independent replicates ### 
f2 <- "var_out_GATK3_pine688_bayenv.covmatv2"
f3 <- "var_out_GATK3_pine688_bayenv.covmatv3"

 m1.2 <- scan(f2, skip=line[(length(line)-1)], nlines=278)
  length(m1.2)==(278*278)

 m1.3 <- scan(f3, skip=line[(length(line)-1)], nlines=278)
  length(m1.3)==(278*278)

c1.2 <- cor.test(m1, m1.2)$est
c1.3 <- cor.test(m1, m1.3)$est
c2.3 <- cor.test(m1.2, m1.3)$est
c1.2; c1.3; c2.3

##### Moved file to desktop for plotting
f <- read.table("var_out_GATK3_pine688_bayenv.covmatv1.converge", header=TRUE)
tail(f)
plot (f$iter3[1:182], f$cor_i[1:182], ylim=c(0.6,1), xlab="iteration", ylab="correlation with final step")
