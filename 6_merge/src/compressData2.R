# compressed data function from EGRET
# that has been modified to handle right-censored data

compressData2 <- function (data, verbose = TRUE, interactive = NULL) {
  if (!is.null(interactive)) {
    warning("The argument 'interactive' is deprecated. Please use 'verbose' instead")
    verbose <- interactive
  }
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  numColumns <- ncol(data)
  numDataColumns <- (numColumns - 1)/2
  lowConcentration <- rep(0, nrow(data))
  highConcentration <- rep(0, nrow(data))
  uncensored <- rep(0, nrow(data))
  i <- 1
  while (i <= numDataColumns) {
    code <- data[2 * i]
    value <- data[2 * i + 1]
    value <- as.numeric(unlist(value))
    value[is.na(value)] <- 0
    returnDataFrame <- as.data.frame(matrix(ncol = 2, nrow = nrow(code)))
    colnames(returnDataFrame) <- c("code", "value")
    returnDataFrame$code <- code[[1]]
    returnDataFrame$code <- ifelse(is.na(returnDataFrame$code), 
                                   "", returnDataFrame$code)
    returnDataFrame$value <- value
    concentrationColumns <- populateConcentrations2(returnDataFrame)
    lowConcentration <- lowConcentration + concentrationColumns$ConcLow
    highConcentration <- highConcentration + concentrationColumns$ConcHigh
    i <- i + 1
  }
  names(data) <- c("dateTime", "code", "value")
  returnDataFrame <- as.data.frame(matrix(ncol = 3, nrow = nrow(data)))
  names(returnDataFrame) <- c("dateTime", "ConcLow", "ConcHigh")
  data$dateTime <- as.character(data$dateTime)
  
  if (dateFormatCheck(data$dateTime)) {
    returnDataFrame$dateTime <- as.Date(data$dateTime)
  } else {
    data$dateTime <- as.Date(data$dateTime, format = "%m/%d/%Y")
    returnDataFrame$dateTime <- as.Date(data$dateTime, format = "%m/%d/%Y")
  }
  returnDataFrame$ConcLow <- as.numeric(lowConcentration)
  returnDataFrame$ConcHigh <- as.numeric(highConcentration)
  # here is where 0 values from ConcHigh could be used to mark censored vals
  Uncen1 <- ifelse(returnDataFrame$ConcLow == returnDataFrame$ConcHigh, 
                   1, 0)
  returnDataFrame$Uncen <- ifelse(is.na(returnDataFrame$ConcLow), 
                                  0, Uncen1)
  flaggedData1 <- returnDataFrame[(returnDataFrame$ConcLow == 
                                     0 & returnDataFrame$ConcHigh == 0), ]
  returnDataFrame <- returnDataFrame[!(returnDataFrame$ConcLow == 
                                         0 & returnDataFrame$ConcHigh == 0), ]
  if (nrow(flaggedData1) > 0) {
    WarningMessage <- paste("Deleted", nrow(flaggedData1), 
                            "rows of data because concentration was reported as 0.0, the program is unable to interpret that result and is therefore deleting it.")
    warning(WarningMessage)
    if (verbose) {
      cat("Deleted Rows:\\n")
      print(flaggedData1)
    }
  }
  # now this can happen because we set ConcHigh = 0 if it is right censored
  #flaggedData2 <- returnDataFrame[(returnDataFrame$ConcLow > 
  #                                   returnDataFrame$ConcHigh), ]
  #returnDataFrame <- returnDataFrame[(returnDataFrame$ConcLow <= 
  #                                      returnDataFrame$ConcHigh), ]
  # if (nrow(flaggedData2) > 0) {
  #   WarningMessage <- paste("Deleted", nrow(flaggedData2), 
  #                           "rows of data because the high concentration was reported lower than the low concentration, the program is unable to interpret that result and is therefore deleting it.")
  #   warning(WarningMessage)
  #   if (verbose) {
  #     cat("Deleted Rows:\\n")
  #     print(flaggedData2)
  #   }
  # }
  return(returnDataFrame)
}
