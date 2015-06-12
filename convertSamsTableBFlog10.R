### This code checks the BF in Sam's file and sees if they are log transformed
### I find that some are and some aren't; this code outputs a corrected dataframe

filename <- "../pine_data/var_out_GATK3_allhet_pine688_ALL.summary.ALL.annots.sorted.GOOD.window_RESULTS"
f2 <- read.table(filename, header=TRUE, comment.char="&")

f2b <- f2
envicols <- seq(34, 97, by=3)
pcacols <- seq(144, 207, by=3)
bfcols <- c(envicols, pcacols)
names(f2b)[bfcols]
min(f2b[,envicols], na.rm=TRUE); min(f2b[,pcacols], na.rm=TRUE); 
round(colMeans(f2b[,bfcols], na.rm=TRUE), 2)
max(f2b[,envicols], na.rm=TRUE); max(f2b[,pcacols], na.rm=TRUE)

# You can see from the above output, that the BF in the environmental variables
# have been log transformed
f2[,pcacols] <- log10(f2b[,pcacols])

min(f2[,envicols], na.rm=TRUE); min(f2[,pcacols], na.rm=TRUE); 
max(f2[,envicols], na.rm=TRUE); max(f2[,pcacols], na.rm=TRUE)

write.table(f2, paste(filename, ".log10bf", sep=""), row.names=FALSE)

