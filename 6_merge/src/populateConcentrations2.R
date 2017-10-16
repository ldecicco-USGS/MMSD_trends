# populateConcentrations function from EGRET
# modified to accomdate right-censored data

populateConcentrations2 <- function (rawData) {
  concentrationColumns <- as.data.frame(matrix(ncol = 3, nrow = length(rawData$value)))
  colnames(concentrationColumns) <- c("ConcLow", "ConcHigh", 
                                      "Uncen")
  concentrationColumns$ConcLow <- as.numeric(ifelse((rawData$code != 
                                                       "<" | is.na(rawData$code)), rawData$value, 0))
  concentrationColumns$ConcHigh <- as.numeric(ifelse((rawData$code != ">" | is.na(rawData$code)), rawData$value, 0))
  tempConcLow <- ifelse((rawData$code != "<" | is.na(rawData$code)), 
                        rawData$value, 0)
  tempConcHigh <- ifelse((rawData$code != ">" | is.na(rawData$code)), 
                        rawData$value, 0)
  concentrationColumns$Uncen <- ifelse(tempConcLow == 0 | tempConcHigh == 0, 0, 
                                       1)
  return(concentrationColumns)
}
