cat("MARK\n")
sink("/dev/null")
bigT1 = proc.time()["elapsed"]

args = commandArgs(trailingOnly=T)


genotypefile<- args[1]
phenotypefile <- args[2]
covariateFile <- args[3]
out = args[4]
SetID <- args[5]
phenoscale = args[6]
alpha1 = args[7]
alpha2 = args[8]
doskatC = args[9]
method = args[10]

suppressPackageStartupMessages(library("SKAT"))

 # read covariates
 pheno = read.table(phenotypefile,header=F)$V1
 covtable = try(read.table(covariateFile,header=F),silent=T)
 covariates = NULL
 if (class(covtable)!= 'try-error') {
 	covariates = data.matrix(covtable)
 }
 
 genotypes = data.matrix(read.table(genotypefile))

# print(str(pheno))
# print(str(covariates))
# print(str(genotypes))

t1 = proc.time()["elapsed"]
obj = if (is.null(covariates)) SKAT_Null_Model(pheno ~ 1, out_type=phenoscale,Adjustment=F) else SKAT_Null_Model(pheno ~ covariates, out_type=phenoscale,Adjustment=F)
t2 = proc.time()["elapsed"]

sink(NULL)

cat(paste(t2-t1,"\n"))
	
t1 = proc.time()["elapsed"]
pvalue = if (doskatC == "0") {
SKAT(genotypes,
 obj,
 method=method,
 is_dosage=TRUE,
 weights.beta=c(as.numeric(alpha1),as.numeric(alpha2)),
 kernel="linear.weighted")$p.value } else {
	SKAT_CommonRare(genotypes,
 obj,
 method="C",
 is_dosage=TRUE,
 weights.beta.rare=c(as.numeric(alpha1),as.numeric(alpha2)))$p.value
}
t2 = proc.time()["elapsed"]
cat(paste(t2-t1,"\n"))

# write(pvalue,file=out)

bigT2 = proc.time()["elapsed"]
cat(paste(bigT2-bigT1,"\n"))

cat(paste(pvalue,"\n"))
