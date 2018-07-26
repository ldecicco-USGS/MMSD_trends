library(readxl)

get_sample <- function(file){
  
  
  sheet.names <- excel_sheets(file)

  data.full <- data.frame()
  
  for(i in sheet.names){
    data.sheet <- read_excel(file, 
                             sheet = i,col_types = c("text","date",
                                                     rep("text",7)))
    data.full <- rbind(data.full, data.sheet)
  }
  
  return(data.full)
  
}

get_sample_modern <- function(file) {
  dat <- read_excel(file, skip = 1)
  return(dat)
}

get_sites <- function(file){

  site.info <- read_excel(file, sheet = "SUMMARY_wRanks")
  
  return(site.info)
}