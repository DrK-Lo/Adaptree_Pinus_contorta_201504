### This file takes the Bayes Factors, log-transforms them (base 10),
### and writes to a new file

# bayenv files at:
# klott@hulk.zoology.ubc.ca
# /data/seqcap/pine/bwa_pseudo/round2_bams/var_out_GATK3_allhet_pine688_ALL.summary.ALL.annots.sorted.GOOD.window.3xbf
# /data/seqcap/pine/bwa_pseudo/round2_bams/var_out_GATK3_allhet_pine688_ALL.summary.ALL.annots.sorted.GOOD.window.3xbfPCA

# additional files and results at:
# /data/seqcap/pine/bwa_pseudo/round2_bams/bayenvResults20150416

setwd("/data/seqcap/pine/bwa_pseudo/round2_bams/bayenvResults20150416")

### read in bayenv results file
  f <-  read.table("/data/seqcap/pine/bwa_pseudo/round2_bams/var_out_GATK3_allhet_pine688_ALL.summary.ALL.annots.sorted.GOOD.window.3xbf", header=TRUE, comment.char="")
  # f <- read.table("/data/seqcap/pine/bwa_pseudo/round2_bams/bayenvResults20150416/headerBayenv2results", header=TRUE, comment.char="")

### read in environment file and environment clusters
  env <- read.table("/data/seqcap/pine/bwa_pseudo/round2_bams/bayenvResults20150416/sprucePineEnvi.clusters", header=TRUE)

### get columns of BF
  head(env)
  names(f)
  colBF <- which(names(f) %in% env$enviAbb)
  ### unit test to make sure all 22 environments are represented
  if (length(colBF)!=22){print("Error: missing environments in columns")}

### calculate BF to log-BF and rewrite to file
  f2 <- f
  f2[,colBF] <- log(f[,colBF], 10)
  write.table(f2,"/data/seqcap/pine/bwa_pseudo/round2_bams/bayenvResults20150416/var_out_GATK3_allhet_pine688_ALL.summary.ALL.annots.sorted.GOOD.window.3xbf.log10", row.names=FALSE)
