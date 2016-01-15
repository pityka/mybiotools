rm(list=objects())
library(ggplot2)
library(reshape2)
library(plyr)
library(scales)
library(RColorBrewer)
library(grid)

args = commandArgs(trailingOnly=T)
file<- args[1]
output <- args[2]
blue <- args[3]
df<-read.table(file,header=TRUE,sep="\t",dec=".",quote="")

# df<-read.table("/Users/arausell/Downloads/out.nutvarsummed.regression.biomart.biologicalprocess.gmt_compact.ProbPath",header=TRUE,sep=" ",dec=".")
# df<-read.table("/Users/arausell/Downloads/out.nutvarsummed.regression.withnonannotated.biomart.biologicalprocess.gmt_compact.ProbPath",header=TRUE,sep=" ",dec=".")



# df=df[order(df$LRT), ]
# df<-subset(df,(LRT<0.05)|as.character(set)== "Background")

#dfm = melt(df, id.vars=c("set"), measure.vars=c("numberOfGenesWithSevere","numberOfGenesWithNonSevere","numberOfGenesWithoutLoF"),variable.name="LOF", value.name="NumberOfGenes")

df$numberOfGenesWithSevere_perc=df$numberOfGenesWithSevere/(df$numberOfGenesWithSevere+df$numberOfGenesWithNonSevere+df$numberOfGenesWithoutLoF)
df$numberOfGenesWithNonSevere_perc=df$numberOfGenesWithNonSevere/(df$numberOfGenesWithSevere+df$numberOfGenesWithNonSevere+df$numberOfGenesWithoutLoF)
df$numberOfGenesWithoutLoF_perc=df$numberOfGenesWithoutLoF/(df$numberOfGenesWithSevere+df$numberOfGenesWithNonSevere+df$numberOfGenesWithoutLoF)

df$LRT<-as.numeric(as.character(df$LRT))
df$LRT_labels<-paste(format(df$LRT, digits = 3, scientific = 2),"/",(df$numberOfGenesWithSevere+df$numberOfGenesWithNonSevere+df$numberOfGenesWithoutLoF))

dfm = melt(df, id.vars=c("set"), measure.vars=c("numberOfGenesWithSevere","numberOfGenesWithNonSevere","numberOfGenesWithoutLoF"),variable.name="Category", value.name="NumGenes")

dfm$id=1:nrow(dfm)

dfm_perc = melt(df, id.vars=c("set"), measure.vars=c("numberOfGenesWithSevere_perc","numberOfGenesWithNonSevere_perc","numberOfGenesWithoutLoF_perc"),variable.name="Category", value.name="NumGenes_perc")
dfm_perc$id=1:nrow(dfm_perc)
dfm_perc = ddply(dfm_perc, .(set), transform, pos = cumsum(NumGenes_perc) - 0.5*NumGenes_perc)
dfm_perc=dfm_perc[order(dfm_perc$id), ]

dfm_perc$NumGenes<-dfm$NumGenes
dfm_perc$MyLabel_pos<- 1.1
dfm_perc$id_revers<-(-1)*(dfm$id-max(dfm$id)-1)
dfm_perc$LRT<-character(length(dfm_perc$set))
if(length(df$set)==as.integer(summary(dfm_perc$set[dfm_perc$Category=="numberOfGenesWithSevere_perc"]==df$set)[names(summary(dfm_perc$set[dfm_perc$Category=="numberOfGenesWithSevere_perc"]==df$set))=="TRUE"])){
  for(i in 2:length(df$set)){
    dfm_perc$LRT[i]<-df$LRT_labels[i]
  }
} 

dfm_perc$GeneSet<-factor(dfm_perc$set,levels=unique(dfm_perc$set))

  
# ggplot(dfm_perc, aes(x=factor(set,levels=unique(set)), y=NumGenes_perc,fill=Category)) +
pdf(output, bg = "transparent",title ='',paper="special",width = 15, height = 12,version='1.6')

color = if (blue == "1") scale_fill_manual(values = rev(brewer.pal(3,"Blues")),labels=c("Genes with severe truncation", "Genes with non-severe truncation", "Genes without truncation")) else scale_fill_manual(values = rev(brewer.pal(3,"Greens")))

ggplot(dfm_perc, aes(x=reorder(GeneSet,id_revers), y=NumGenes_perc,fill=Category)) +
  geom_bar( stat='identity') +
  scale_y_continuous(label=percent,breaks=c(0,.25,.50,.75,1),limits=c(0,1.15)) +
  coord_flip()+
  # scale_fill_brewer() +
  color+
  xlab("Gene Set") +
  ylab("Relative frequency of genes bearing truncating variants") +
  theme(axis.text.y = element_text(colour="black",size=3)) + # Rotate tick mark labels
  theme(axis.text.x = element_text(angle = 0, hjust = 1,colour="black",size=3)) + # Rotate tick mark labels
  # guides(fill = guide_legend(reverse = TRUE)) + # Reverse the order of the colours in the legend
  guides(fill = guide_legend()) + # Reverse the order of the colours in the legend
  # geom_text(aes(label = NumGenes, y = pos), size = 3*(dfm_perc$NumGenes>0))+  # labels inside the bar segments
  geom_text(aes(label = LRT, y = MyLabel_pos), size = 2.5,angle=0)+
  geom_hline(aes(yintercept=dfm_perc$NumGenes_perc[1]),linetype="dashed",colour="red")+
  theme(legend.position = 'top') + theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()) + 
    theme(legend.title=element_blank())  + 
    theme(legend.position="bottom")



dev.off()
