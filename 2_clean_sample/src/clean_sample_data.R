library(tidyr)
library(stringr)
library(dplyr)
library(data.table)


clean_sample_data <- function(raw_sample){

  data.long <- raw_sample %>%
    select(-`BOD20 (mg/L)`) %>%
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
  
  # get rid of "S" "B" "M" from site codes
  # take mean of duplicated measurements from site/date/param combos
  
  data.long.mean <- data.long %>%
    mutate(SITE = gsub(pattern = "([A-Za-z]{2}-[0-9]{2})([A-Za-z]{1})",replacement = "\\1", data.long$SITE)) %>%
    filter(rmk != 'M') %>%
    group_by(SITE, DATE, param) %>%
    summarize(value.mean = mean(value.new), rmk.mean = paste0(unique(rmk), collapse = '')) %>%
    filter(rmk.mean != '<>')

  
  data.wide <- dcast(setDT(data.long.mean),
                     SITE + DATE ~ param,
                     value.var = c("value.mean", "rmk.mean"))
  
  
  
  names(data.wide) <- gsub("value.mean_","",names(data.wide))
  names(data.wide) <- gsub("rmk.mean_", "rmk_", names(data.wide))

    # combine FC vars into a single var. Will analyze all three. 
  data.wide$FC_combined <- data.wide$`FC (MPN/100mL)`
  data.wide$FC_combined[is.na(data.wide$FC_combined)] <- data.wide$`FC (CFU/100mL)`[is.na(data.wide$FC_combined)]

  data.wide$rmk_FC_combined <- data.wide$`rmk_FC (MPN/100mL)`
  data.wide$rmk_FC_combined[is.na(data.wide$rmk_FC_combined)] <- data.wide$`rmk_FC (CFU/100mL)`[is.na(data.wide$rmk_FC_combined)]
  
  return(data.wide)
}

clean_sample_data_modern <- function(raw_sample) {
  
  # columns have repeating names - same names for different parameters
  # no number = ammonia, 1 = TP, 2 = fecal coliform (MPN/100mL), 3 = fecal coliform (CFU/100 mL), 
  # 4 = TSS, 5 = 5 day BOD, 6 = 20 day BOD
  
  filtered_dat <- select(raw_sample, -SAMPLE, -contains('Method'), -contains('__6')) %>%
    filter(DATE >= as.Date('2016-12-07'))
  
  results_dat <- filtered_dat %>%
    select(SITE, DATE, contains('Result')) %>%
    rename('NH3 (mg/L)' = Result,
           'TP (mg/L)' = Result__1,
           'FC (MPN/100mL)' = Result__2,
           "FC (CFU/100mL)" = Result__3,
           "Total Suspended Solids (mg/L)" = Result__4,
           'BOD5 (mg/L)' = Result__5) %>%
    gather(key = variable, value = raw_value, -SITE, -DATE) %>%
    filter(!is.na(raw_value)) %>%
    rowwise() %>%
    mutate(ramk = case_when(
      any(grep('<', raw_value)) ~ "<",
      any(grep('>', raw_value)) ~ ">")) %>%
    ungroup() %>%
    mutate(value = gsub('<\\s|>\\s', '', raw_value)) %>%
    mutate(value = as.numeric(gsub('\\s\\D.+', '', value)))
    
    
    rename()
  rmk_dat <- filtered_dat %>%
    select(contains(''))
}
