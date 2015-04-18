### This file takes the log(BF) file and replaces BF with logical T/F for 
### different cutoffs in the tails.  Also computes logical T/F for clustered environmental variables

filename <- "/data/seqcap/pine/bwa_pseudo/round2_bams/bayenvResults20150416/var_out_GATK3_allhet_pine688_ALL.summary.ALL.annots.sorted.GOOD.window.3xbf.log10"
### read in file with log-BF
  f2<- read.table(filename, header=TRUE, comment.char="")
### read in file with cutoffs
  cos <- read.table(paste(filename,".cutoffs"), header=TRUE)
### read in environment file describing clusters
  env <- read.table("/data/seqcap/pine/bwa_pseudo/round2_bams/bayenvResults20150416/sprucePineEnvi.clusters", header=TRUE)

### get BF columns
  colBF <- which(names(f2) %in% env$enviAbb)

### for each cutoff, 
  f3 <- f2
  for (i in 1:4){
    BFcutoff <- cos[1,i]
      ### convert bF columns to T/F for outliers
    f3[,colBF] <- f2[,colBF] > BFcutoff
    #head(f3[,colBF])

      ### compute outliers for each cluster (3 columns) and write T/F to file
      G1cols <- which(names(f3) %in% env$enviAbb[env$G4==1])
      G2cols <- which(names(f3) %in% env$enviAbb[env$G4==2])
      G3cols <- which(names(f3) %in% env$enviAbb[env$G4==3])
      ### unit test
       if((length(G1cols) + length(G2cols) + length(G3cols))!=19){print("Error in envi clusters")}
    
      G1 <- rowSums(f3[,G1cols])>0
      G2 <- rowSums(f3[,G2cols])>0
      G3 <- rowSums(f3[,G3cols])>0
    
      overall.out <- rowSums(f3[,colBF])>0   
      # expected.num <- nrow(f3)*22*(1 - as.numeric(unlist(strsplit(names(cos)[i], "V"))[2]))
    
      sum(f3$LAT, na.rm=TRUE); sum(f3$LONG, na.rm=TRUE); sum(f3$ELEVATION, na.rm=TRUE)
      sum(G1, na.rm=TRUE); sum(G2, na.rm=TRUE); sum(G3, na.rm=TRUE); 
      sum(overall.out, na.rm=TRUE)
    
      f3b <- data.frame(f3, G1, G2, G3, overall.out)
      
      write.table(f3, paste(filename,"gt", names(cos[i]), sep=""))
    
  }