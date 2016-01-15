library("DESeq")
library("vsn")

args = commandArgs(trailingOnly=T)

countsfile<- args[1]
conditionsfile <- args[2]
subjectfile <- args[3]
isnormalized <- args[4]
outfile <- args[5]
sizeFactorsFile <- args[6]


mySizeFactors =  read.table(sizeFactorsFile,header=F,colClasses="numeric")$V1 

raw = read.csv(countsfile,sep=",",header=T,na.strings="-2147483648",row.names=1,
	colClasses=c("character",rep("numeric",length(mySizeFactors))))
raw[is.na(raw)] = 0



conditions = as.vector(read.table(conditionsfile,header=F)$V1)

subjects = as.vector(read.table(subjectfile,header=F)$V1)


designTable = data.frame(row.names=colnames(raw),condition=conditions,subject=subjects)

cds = newCountDataSet(raw,designTable)

if (isnormalized == 0) {
	cds = estimateSizeFactors(cds)
} else {
	sizeFactors(cds) = mySizeFactors
	}
cds = estimateDispersions(cds,sharingMode="maximum",method="pooled-CR")	



fit1 = fitNbinomGLMs( cds, count ~ subject * condition )
fit0 = fitNbinomGLMs( cds, count ~ subject  )

pvalsGLM = nbinomGLMTest( fit1, fit0 )
padjGLM = p.adjust( pvalsGLM, method="BH" )

fitwithp = fit1
fitwithp$p = padjGLM
fitwithp = na.omit(fitwithp[fitwithp$converged == T,])

write.csv(fitwithp,outfile,quote=F,sep=",",row.names=T)