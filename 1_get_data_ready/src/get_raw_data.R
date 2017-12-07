library(readxl)

get_sample <- function(ind_file){
  
  gd_get(ind_file)

  file <- as_data_file(ind_file)
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

get_sites <- function(ind_file){

  gd_get(ind_file)
  file <- as_data_file(ind_file)
  
  site.info <- read_excel(file, sheet = "SUMMARY_wRanks")
  
  return(site.info)
}