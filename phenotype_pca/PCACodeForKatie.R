#Create Phenotype File w/ extra columns
all = read.table("PinePhenotypicData.txt", header=T, sep="\t")
all = all[all$ExperimentalClimate == 'MAT06',]

library('xlsx')
a=read.xlsx('traits-1.xlsx',1)
b=as.vector(a$Trait)
newnames=as.vector(a$New.name.for.paper)

#58,60,62 cold hard #58,60 close to 50 injury
#65,67,69 midwinter hard #65,67 close to 50 injury
#72,74,76 spring hard #72,74 close to 50 injury
#start
allf = all[,c(5,10,58,60,62)]
sum(abs(allf[,3]-50), na.rm=T)
sum(abs(allf[,4]-50), na.rm=T)
sum(abs(allf[,5]-50), na.rm=T)

allf = all[,c(5,10,65,67,69)]
sum(abs(allf[,3]-50), na.rm=T)
sum(abs(allf[,4]-50), na.rm=T)
sum(abs(allf[,5]-50), na.rm=T)

allf = all[,c(5,10,72,74,76)]
sum(abs(allf[,3]-50), na.rm=T)
sum(abs(allf[,4]-50), na.rm=T)
sum(abs(allf[,5]-50), na.rm=T)
#end

y = all[,c(5,10,which(names(all) %in% b))]

x = apply(all[,c(58,60)], 1, mean)
y = cbind(y,"Fall cold injury"=x)

x = apply(all[,c(65,67)], 1, mean)
y = cbind(y,"Winter cold injury"=x)

x = apply(all[,c(72,74)], 1, mean)
y = cbind(y,"Spring cold injury"=x)

names(y)[-c(1:2)] = newnames

#newtraits
y$"root wt/shoot wt" = y$Root_weight / y$Shoot_weight
y$"root wt + shoot wt" = y$Root_weight + y$Shoot_weight

pheno[,-c(1:2)] = log(pheno[,-c(1:2)]) #log transform seems to give best output (wrt normal dist.)

#PCA on Traits
library("HSAUR2")
X = as.matrix(pheno[,-c(1:2)])
pc=prcomp(na.omit(X), scale=TRUE)
res = pheno[,1:2]
x1 = pc$x
res2 = cbind(res,x1[match(rownames(res),rownames(x1)),])
res2[,1] = paste0("Pi_",res2[,1]) #I need this for Association Analysis
x = data.frame(-9, res2[,-2]) #I need this for GCTA
write.table(x, 'data-pca.pheno', quote=F, row.names=F, col.names=F, sep="\t")
