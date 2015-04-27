 get.cutoffs <- function(var){
	 var2 <- as.vector(unlist(var), mode="numeric") #slow for large dataframe
	#length(var2)
    sort.BF<- sort(var2)
    #hist(sort.BF)
    n <- length(sort.BF)
    max.V <- sort.BF[n]
	V.99999 <- sort.BF[round(n*.99999,0)]
	V.9999 <- sort.BF[round(n*.9999,0)]
	V.9995 <- sort.BF[round(n*.9995,0)]
    V.999 <- sort.BF[round(n*.999,0)]
    V.99 <- sort.BF[round(n*.99,0)]
    V.95 <- sort.BF[round(n*.95,0)]
	V.90 <- sort.BF[round(n*.90,0)]
    return(data.frame(V.99999, V.9999, V.9995, V.999, V.99, V.95, V.90, max.V))
  }
