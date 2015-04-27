### This script takes the log-transformed BF and calculates the cutoffs- 
### writes cutoffs to file

filename <- "/data/seqcap/pine/bwa_pseudo/round2_bams/bayenvResults20150416/var_out_GATK3_allhet_pine688_ALL.summary.ALL.annots.sorted.GOOD.window.3xbf.log10"
### read in file with log-BF
 f2<- read.table(filename, header=TRUE, comment.char="")
### read in environment file and environment clusters
  env <- read.table("/data/seqcap/pine/bwa_pseudo/round2_bams/bayenvResults20150416/sprucePineEnvi.clusters", header=TRUE)
  env <- read.table("sprucePineEnvi.clusters", header=TRUE)
### get columns of BF
  head(env)
  names(f2)
  colBF <- which(names(f2) %in% env$enviAbb)
  ### unit test to make sure all 22 environments are represented
  if (length(colBF)!=22){print("Error: missing environments in columns")}

### calculate BF cutoffs
  ### (V.999, V.99, V.95, V.90, max.V)
  source("get.cutoffs.R")
  cos <- get.cutoffs(f2[,colBF])
  cos
  ### write cutoffs to file
 write.table(cos, paste(filename,".cutoffs", sep=""), row.names=FALSE)

### Calculate XTX cutoffs
  cx <- get.cutoffs(f2$xtx)
  cx
  write.table(cx, paste(filename,".XTXcutoffs", sep=""), row.names=FALSE)
