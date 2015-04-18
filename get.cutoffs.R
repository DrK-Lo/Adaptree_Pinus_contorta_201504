 get.cutoffs <- function(var){
	 var2 <- as.vector(unlist(var), mode="numeric") #slow for large dataframe
	#length(var2)
    sort.BF<- sort(var2)
    #hist(sort.BF)
    n <- length(sort.BF)
    max.V <- sort.BF[n]
	V.9999 <- sort.BF[n*.9999]
    V.999 <- sort.BF[n*.999]
    V.99 <- sort.BF[n*.99]
    V.95 <- sort.BF[n*.95]
    return(data.frame(V.9999, V.999, V.99, V.95, max.V))
  }
