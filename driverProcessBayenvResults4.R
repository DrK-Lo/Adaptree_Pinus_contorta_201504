
### install Venn diagram
source("http://www.bioconductor.org/biocLite.R")
class(biocLite)
biocLite("limma")
library(limma)

flist <- list.files(pattern = "gt")
env <- read.table("/data/seqcap/pine/bwa_pseudo/round2_bams/bayenvResults20150416/sprucePineEnvi.clusters", header=TRUE)
groupsize <- tapply(env$G4,env$G4, length)

for (i in 1:length(flist)){
    
    ### upload file
      filename <- flist[i]#"var_out_GATK3_allhet_pine688_ALL.summary.ALL.annots.sorted.GOOD.window.3xbf.log10gtV.999"
      cutoff <- as.numeric(sub(".*gtV","",flist[i]))
      f3b <- read.table(filename, header=TRUE, comment.char="")  
      
    ### Venn diagram for Lat/Long/Elevation
      c1 <- names(f3b) %in% c("LAT", "LONG", "ELEVATION")
      TF.LLE <- f3b[,c1]
      counts.LLE <- vennCounts(TF.LLE)
      vennDiagram(counts.LLE, main = cutoff)
    
    ### Venn diagram for G1/G2/G3
      c2 <- names(f3b) %in% c("G1", "G2", "G3")
      TF.Gs <- f3b[,c2]
      counts.Gs <- vennCounts(TF.Gs)
      vennDiagram(counts.Gs, main = paste("Unscaled", cutoff))
    
    ### Venn diagram for scaled environment (not sure if this makes sense)
#       counts.G.scaled <- counts.Gs
#       counts.G.scaled[counts.G.scaled[,1]==1,4] <- counts.G.scaled[counts.G.scaled[,1]==1,4]/groupsize[2]
#       counts.G.scaled[counts.G.scaled[,1]==2,4] <- counts.G.scaled[counts.G.scaled[,1]==1,4]/groupsize[3]
#       counts.G.scaled[counts.G.scaled[,1]==3,4] <- counts.G.scaled[counts.G.scaled[,1]==1,4]/groupsize[4]
#       vennDiagram(counts.Gs, main = paste("Scaled", cutoff))
}