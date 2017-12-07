install.packages(c("EGRETci","usgsEGRET","smwrGraphs","reshape2"),
                 repo="file:packages",type="source",
                 dependencies=c("Depends","Imports"), lib='rLibs')

args <- commandArgs(trailingOnly = TRUE)

i <- as.numeric(args[1])+1 #Should be 1

library(EGRETci)
library(usgsEGRET)
library(smwrGraphs)
library(reshape2)

all.eLists <- readRDS('all_eLists.rds')
eList <- all.eLists[[i]]

eList <- modelEstimation(eList, minNumObs = 50, minNumUncen = 10, verbose=TRUE)
tableResultData <- suppressWarnings(tableResults(eList))

yearPoints <- c(1983, 1993, 2003, 2013)
fluxTable <- tableChangeSingle(eList, flux = TRUE, yearPoints = yearPoints)
concTable <- tableChangeSingle(eList, flux = FALSE, yearPoints = yearPoints)

multiPlotDataOverview(eList)
#plotConcQ(eList, logScale = TRUE,legend = FALSE)
#plotConcTime(eList, logScale = TRUE)
plotResidPred(eList)
plotResidQ(eList)
plotResidTime(eList)
plotConcPred(eList, logScale = T)
plotFluxPred(eList, logScale = TRUE)
boxResidMonth(eList, rResid = TRUE)
#boxConcMonth(eList, logScale = TRUE)
plotFluxHist(eList, yearStart = 1979,yearEnd = 2016)
plotConcHist(eList, yearStart = 1979,yearEnd = 2016)
plotQTimeDaily(eList, yearStart = 1979,yearEnd = 2016)

# this one does not work
plot15(eList, 1982, 2016)




