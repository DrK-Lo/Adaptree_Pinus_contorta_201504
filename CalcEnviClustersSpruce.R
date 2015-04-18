# CalcEnviClusters.R
# KE Lotterhos
# Oct 22, 2014

# Takes a matrix of environmental variables across populations as input
# Does a cluster analysis
# Assigns environmental variables to a cluster

setwd("/Users/katie/Desktop/AdaptreeData/src_spruce/src/src-remove342and479")
envimat <- "seqcap.bayenv.BF.envi2"
envinames <- "enviNamesAllAnalyses.txt"

  envi <- read.table(envimat)
  rowenvi <- read.table(envinames)
  rownames(envi) <- rowenvi$V1
  e <- scale(envi)

  # K-Means Cluster Analysis
  #fit3 <- kmeans(e, 3) # 3 cluster solution
  # get cluster means 
  #clusterMeans <- t(aggregate(e,by=list(fit3$cluster),FUN=mean))

  # Ward Hierarchical Clustering
  makeplot<- function(d, labs, k1){
    fit <- hclust(d, method="ward.D") 
    pdf(paste(envimat,".",k1, "clusters.pdf", sep=""), width=6, height=8)
      plot(fit, labels = labs, main="Spruce", xlab="") # display dendogram
      groups <- cutree(fit, k=k1) # cut tree into x clusters
      # draw dendogram with red borders around the x clusters 
      rect.hclust(fit, k=k1, border="red")
    dev.off()
    return(as.numeric(groups))
  }
  par(mfrow=c(1,1), mar=c(1,4,1,1))

  te <- t(e)
  quartz()
  d2 <- dist(abs(cor(te)),upper = T,method ="euclidean",diag = T)
  d3 <- as.matrix(d2)
  cor2 <- abs(cor(te))

# #now the absolute values of the correlations match up quite nicely with the distances:
   plot (cor2,d3)

# #this makes some quite different groupings:
  plot (hclust (dist(abs(cor(te)),upper = T,method ="euclidean",diag = T),method = "ward.D"))
  G3 <- makeplot(dist(abs(cor(te))),rowenvi$V2,3 )
  G3 <- makeplot(dist(abs(cor(te))),rowenvi$V2,3 )
# 
  mydata <- data.frame(enviAbb = rowenvi$V1, enviDesc = rowenvi$V2, G3)
  mydata
  mydata$G3.desc <- NA
  mydata$G3.desc[G3==1] <- "Temperature"
  mydata$G3.desc[G3==2] <- "Precipitation"
  mydata$G3.desc[G3==3] <- "Elevation and frost"
write.table(mydata, paste(envimat, ".clusters", sep=""), row.names=FALSE, col.names=TRUE)
