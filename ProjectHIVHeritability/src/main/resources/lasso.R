library(glmnet)

args = commandArgs(trailingOnly=T)

phenotypefile <- args[1]
genotypefile <- args[2]
outfile <- args[3]

genotype = read.csv(genotypefile,sep=",",header=T,na.strings=c("-2147483648","NaN"),row.names=1)
phenotype = read.csv(phenotypefile,sep=",",header=T,na.strings=c("-2147483648","NaN"),row.names="FID")

merged = na.omit(merge(genotype,phenotype,by="row.names"))

rownames(merged) = merged$Row.names
merged$Row.names = NULL

predictors = merged[,!names(merged) %in% c("PHENO")]


cvfit = cv.glmnet(as.matrix(predictors),merged$PHENO,standardize=F)
variablesinmodel = names(predictors)[(abs(as.matrix(coef(cvfit, s = "lambda.min"))[,1]) > 0.00)[2:length(coef(cvfit))]]

lmfit = summary(lm(merged$PHENO ~ .,predictors[, variablesinmodel]))

variablesinmodelnonna = row.names(lmfit$coefficients)[!is.na(lmfit$coefficients[,4])][2:length(row.names(lmfit$coefficients)[!is.na(lmfit$coefficients[,4])])]
write.table(variablesinmodelnonna,outfile,quote=F,row.names=F,col.names=F)
