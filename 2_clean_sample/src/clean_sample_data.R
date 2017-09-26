library(tidyr)
library(stringr)
library(dplyr)
library(data.table)


clean_sample_data <- function(raw_sample){

  data.long <- raw_sample %>%
    select(-`FC (CFU/100mL)`) %>%
    gather(key = "param", value = "value", -SITE, -DATE) %>%
    mutate(rmk = "")
  
  data.long$rmk[which(str_detect(data.long$value, pattern = "<"))] <- "<"
  data.long$rmk[which(str_detect(data.long$value, pattern = ">"))] <- ">"
  data.long$rmk[which(str_detect(data.long$value, pattern = "M"))] <- "M"
  
  data.long$value.new <- gsub(" ","", data.long$value)
  data.long$value.new <- gsub(">","", data.long$value.new)
  data.long$value.new <- gsub("<","", data.long$value.new)
  data.long$value.new <- gsub("M","", data.long$value.new)
  
  data.long <- data.long %>%
    filter(!is.na(value.new)) %>%
    distinct()

  data.long$value.new <- as.numeric(data.long$value.new)
    

  # Right now, we have data with >1 sample per day, 
  # but no more info than that. 
  # For now...deleting all but the first unique site/date combo:
  
  site_date <- paste(data.long$SITE, data.long$DATE, data.long$param)
  dup.rows <- which(duplicated(site_date))
  
  data.wide <- dcast(setDT(data.long[-dup.rows,]),
                     SITE + DATE ~ param,
                     value.var = c("value.new", "rmk"))
  
  
  
  names(data.wide) <- gsub("value.new_","",names(data.wide))
  return(data.wide)
}
