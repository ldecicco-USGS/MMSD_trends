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
  
  dat <- read_excel(file, skip = 2, 
                    col_types = c('text', 'text', 'date', 
                                  rep(c('text', 'numeric', 'text', 'text', 'text'), 7)),
                    col_names = c('SAMPLE', 'SITE', 'DATE', 
                                  'NH3 (mg/L)', 'MDL_NH3', 'LOQ_NH3', 'Flags_NH3', 'Method_NH3', 
                                  'TP (mg/L)', 'MDL_TP', 'LOQ_TP', 'Flags_TP', 'Method_TP',
                                  'FC (MPN/100mL)', 'MDL_FC_MPN', 'LOQ_FC_MPN', 'Flags_FC_MPN', 'Method_FC_NPM', 
                                  'FC (CFU/100mL)', 'MDL_FC_CFU', 'LOQ_FC_CFU', 'Flags_FC_CFU', 'Method_FC_CFU',
                                  'Total Suspended Solids (mg/L)', 'MDL_Total Suspended Solids', 'LOQ_Total Suspended Solids', 'Flags_Total Suspended Solids', 'Method_Total Suspended Solids',
                                  'BOD5 (mg/L)', 'MDL_BOD5', 'LOQ_BOD5', 'Flags_BOD5', 'Method_BOD5', 
                                  'BOD20 (mg/L)', 'MDL_BOD20', 'LOQ_BOD20', 'Flags_BOD20', 'Method_BOD20'))
  return(dat)
}

get_sites <- function(file){

  site.info <- read_excel(file, sheet = "SUMMARY_wRanks")
  
  return(site.info)
}