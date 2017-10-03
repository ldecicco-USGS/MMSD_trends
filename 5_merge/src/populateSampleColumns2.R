# function populateSampleColumns from EGRET
# modified to accomodate right-censored data

populateSampleColumns2 <- function (rawData) {
  Sample <- as.data.frame(matrix(ncol = 3, nrow = length(rawData$dateTime)))
  colnames(Sample) <- c("Date", "ConcLow", "ConcHigh")
  Sample$Date <- rawData$dateTime
  Sample$ConcLow <- rawData$ConcLow
  Sample$ConcHigh <- rawData$ConcHigh
  Sample$Uncen <- rawData$Uncen
  Sample$ConcAve <- ifelse(Sample$ConcHigh == 0, Sample$ConcLow*1.5, (Sample$ConcLow + Sample$ConcHigh)/2)
  Sample$ConcLow <- ifelse((rawData$ConcLow == 0 & rawData$Uncen == 0), NA, rawData$ConcLow)
  Sample$ConcHigh <- ifelse((rawData$ConcHigh == 0 & rawData$Uncen == 0), NA, rawData$ConcHigh)
  dateFrame <- populateDateColumns(rawData$dateTime)
  Sample <- cbind(Sample, dateFrame[, -1])
  Sample$SinDY <- sin(2 * pi * Sample$DecYear)
  Sample$CosDY <- cos(2 * pi * Sample$DecYear)
  #Sample2 <- subset(Sample, (!is.na(Sample$ConcHigh)))
  return(Sample)
}
