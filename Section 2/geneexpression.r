
#@author: Romeo Kienzler
#This file is copyright protected - do not redistribute

library(ggplot2)
source("http://www.bioconductor.org/biocLite.R")
biocLite("GEOquery")
library(Biobase)
library(GEOquery)
GDS5093 <- getGEO('GDS5093', destdir="/Users/romeokienzler/Documents/tmp/")
GDS5088 <- getGEO('GDS5088', destdir="/Users/romeokienzler/Documents/tmp/")


dfVirus = Table(GDS5093)
dfMother = Table(GDS5088)
dim(dfVirus)
dim(dfMother)
colnames(dfMother)
dfVirus[,2]
dfMother[,2]
commonGenes = intersect(dfVirus[,2],dfMother[,2])
length(commonGenes)
length(dfVirus[,2])
length(dfMother[,2])
dfVirusFilterMask = (dfVirus$IDENTIFIER %in% commonGenes) & !duplicated(dfVirus$IDENTIFIER)
dfVirusFiltered = dfVirus[dfVirusFilterMask,]
dfMotherFilterMask = dfMother$IDENTIFIER %in% commonGenes & !duplicated(dfMother$IDENTIFIER)
dfMotherFiltered = dfMother[dfMotherFilterMask,]
dim(dfVirusFiltered)
dim(dfMotherFiltered)
#qplot(dfVirusFiltered$IDENTIFIER,dfVirusFiltered$GSM1253056, geom = "point")


meanVirus=colMeans(apply(dfVirusFiltered[,3:dim(dfVirusFiltered)[[2]]],1, as.numeric)) 
meanMother=colMeans(apply(dfMotherFiltered[,3:dim(dfMotherFiltered)[[2]]],1, as.numeric)) 
dfMeanVirus = data.frame(1:length(meanVirus),replicate(length(meanVirus),0),meanVirus)
dfMeanMother = data.frame(1:length(meanMother),replicate(length(meanMother),1),meanMother)
#gene sorting? #kommentare
colnames(dfMeanVirus)
colnames(dfMeanVirus)[1] = "geneid"
colnames(dfMeanVirus)[2] = "sample"
colnames(dfMeanVirus)[3] = "mean"
colnames(dfMeanMother)[1] = "geneid"
colnames(dfMeanMother)[2] = "sample"
colnames(dfMeanMother)[3] = "mean"


result = rbind(dfMeanVirus,dfMeanMother)

qplot(result$geneid,result$mean, color=factor(result$sample))
subresult = result[result$geneid<100,]
qplot(subresult$geneid,subresult$mean, color=factor(subresult$sample))

