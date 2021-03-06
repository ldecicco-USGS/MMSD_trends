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
  
  filtered_dat <- select(raw_sample, -SAMPLE, -contains('Method'), -contains('BOD20')) %>%
    filter(DATE >= as.Date('2016-12-07'))
  
  results_dat <- filtered_dat %>%
    select(SITE, DATE, contains('mg/L'), contains('100mL')) %>%
    gather(key = variable, value = raw_value, -SITE, -DATE) %>%
    filter(!is.na(raw_value)) %>%
    rowwise() %>%
    mutate(rmk = case_when(
      any(grep('<', raw_value)) ~ "<",
      any(grep('>', raw_value)) ~ ">")) %>%
    ungroup() %>%
    filter(!(raw_value %in% "M mg/L")) %>%
    mutate(value = gsub('<\\s|>\\s', '', raw_value)) %>%
    mutate(value = as.numeric(gsub('\\s\\D.+', '', value))) %>%
    mutate(SITE =  gsub(pattern = "([A-Za-z]{2}-[0-9]{2})([A-Za-z]{1})", replacement = "\\1", SITE))
  
  symbols_before <- unique(gsub('(^\\D*\\s*)(\\d+[[:punct:]]*\\s*\\d*\\s.+$)', replacement = '\\1', results_dat$raw_value))
  
  # take the mean of surface, middle, bottom samples
  # which have the same site/date combo
  results_dat_mean <- group_by(results_dat, SITE, DATE, variable) %>%
    summarize(value.mean = mean(value), rmk.mean = paste0(unique(rmk), collapse = '')) %>%
    mutate(rmk.mean = na_if(rmk.mean, 'NA')) %>%
    filter(!rmk.mean %in% '<>')
  
  rmk <- select(results_dat_mean, SITE, DATE, variable, rmk = rmk.mean) %>%
    mutate(variable = paste0('rmk_', variable)) %>%
    spread(key = variable, value = rmk)
  
  vals <- select(results_dat_mean, SITE, DATE, variable, value = value.mean) %>%
    spread(key = variable, value = value)
  
  # no ifelse statement for picking between FC measures because there were no MPN method
  # measurements in WY 2017 - just pulled over CFU measures in FC_combined
  cleaned_dat <- left_join(vals, rmk) %>%
    mutate(FC_combined = `FC (CFU/100mL)`,
           rmk_FC_combined = `rmk_FC (CFU/100mL)`)
    
  return(cleaned_dat)
}
