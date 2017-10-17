library(EGRET)
library(dplyr)
library(lubridate)

merge_sample_flow <- function(all.samples, site.summary, all.flow, save.eLists.in){

  #min.samples <- 50
  
  dir.create(save.eLists.in, showWarnings = FALSE, recursive = TRUE)
  
  params <- data.frame(name = c("BOD5 (mg/L)","FC (CFU/100mL)","FC (MPN/100mL)","FC_combined","NH3 (mg/L)","TP (mg/L)","Total Suspended Solids (mg/L)"),
                       paramShortName = c("BOD5","FC_CFU","FC_MPN", "FC_combined", "NH3","TP","Total Suspended Solids"),
                       param.units = c("mg/L","CFU/100mL", "MPN/100mL", "CFU-MPN/100mL","mg/L","mg/L","mg/L"),
                       stringsAsFactors = FALSE)

  
  master_list <- data.frame(id = character(),
                            complete = logical(),
                            missing_all_sample = logical(),
                            missing_all_flow = logical(),
                            stringsAsFactors = FALSE,
                            n_samples = numeric(),
                            n_years = numeric(),
                            n_years_consec = numeric(),
                            samples_per_year = numeric(),
                            max_time_gap_days = numeric(),
                            n_before_gap = numeric(),
                            n_after_gap = numeric(),
                            n_flow_gaps = numeric(),
                            prop_censored = numeric())
  
  
  for(i in site.summary$SITE){
    
    sample.data <- filter(all.samples, SITE == i)
    #flow_site <- site.summary$siteID[which(site.summary$SITE == i)]
    flow <- all.flow %>%
      filter(sample_site == i) %>%
      select(-sample_site)
    
    if(nrow(flow) == 0){
      master_list <- bind_rows(master_list, 
                               data.frame(id = paste(i, params$paramShortName, sep="_"),
                                          complete = FALSE,
                                          missing_all_sample = FALSE,
                                          missing_all_flow = TRUE,
                                          stringsAsFactors = FALSE,
                                          n_samples = NA,
                                          n_years = NA,
                                          n_years_consec = NA,
                                          samples_per_year = NA,
                                          max_time_gap_days = NA,
                                          n_before_gap = NA,
                                          n_after_gap = NA,
                                          n_flow_gaps = NA,
                                          prop_censored = NA))
      
      next
    }
    names(flow) <- c('agency', 'site', 'dateTime', 'value', 'code')
    
    Daily <- populateDaily(flow, 35.314667,verbose = FALSE)
    
    Daily <- arrange(Daily, Date)
    # some sites have flow data that does not go back all the way to the beginning
    # of the WQ record. Find NA values in the Daily flow data, and start record
    # after the last NA value of flow
  
    
    for(j in seq_len(nrow(params))){
      
      sample.sub <- sample.data[,c("DATE",params$name[j],
                                   paste("rmk",params$name[j],sep = "_"))]
      names(sample.sub) <- c("dateTime", "value", "code")
      sample.sub <- sample.sub[,c("dateTime", "code","value")]
      sample.sub <- sample.sub[!is.na(sample.sub$value),]
      if(nrow(sample.sub) == 0){
        master_list <- bind_rows(master_list, 
                                 data.frame(id = paste(i, params$paramShortName[j], sep="_"),
                                            complete = FALSE,
                                            missing_all_sample = TRUE,
                                            missing_all_flow = FALSE,
                                            stringsAsFactors = FALSE,
                                            n_samples = 0,
                                            n_years = 0,
                                            n_years_consec = 0,
                                            samples_per_year = 0,
                                            max_time_gap_days = NA,
                                            n_before_gap = NA,
                                            n_after_gap = NA,
                                            n_flow_gaps = NA,
                                            prop_censored = NA))
        
        next
      }
      compressedData <- compressData(sample.sub, verbose=FALSE)
      Sample <- populateSampleColumns(compressedData)
      INFO <- data.frame(paramShortName = params$paramShortName[j],
                         param.units = params$param.units[j],
                         shortName = i,
                         constitAbbrev = paste(params$paramShortName[j],i,sep="_"),
                         staAbbrev = i,
                         paStart = 10,
                         paLong = 12,
                         stringsAsFactors = FALSE)
      
      Sample <- filter(Sample, Date %in% Daily$Date) %>%
        arrange(Date)
      
      max.diff <- which.max(diff(Sample$Date))
      n.gap.before <- which.max(diff(Sample$Date))
      n.gap.after <- nrow(Sample) - which.max(diff(Sample$Date))
      prop.gap.before <- which.max(diff(Sample$Date))/nrow(Sample)
      prop.gap.after <- (nrow(Sample) - which.max(diff(Sample$Date)))/nrow(Sample)
      
      # filter samples that are before or after a large gap, and number of samples
      # before or after that gap make up less than 10% of the data
      
      if (max.diff > 365 & (prop.gap.before < .1 | prop.gap.after < .1)) {
        # distinguish between gaps at end or beginning of record
        # if gap is at beginning, remove points prior to gap
        if (prop.gap.before < .1) {
          Sample <- Sample[(max.diff+1):nrow(Sample), ]
        # if gap is at end, remove points after gap
        } else {
          Sample <- Sample[1:max.diff, ]
        }
      }
      
      # modify Daily to match the start and end dates of Sample
      Daily_mod <- filter(Daily, !is.na(Q)) %>%
        filter(Date >= min(Sample$Date) & Date <= max(Sample$Date))
      
      # find gaps in the flow data
      n.flow.gaps <- length(which(diff(Daily_mod$Date) >1))
      
      # if there are gaps in the flow data, remove data before gaps
      # then modify the sample record to match the flow record
      
      if (n.flow.gaps > 0) {
        row.drop <- which(diff(Daily_mod$Date) >1)
        Daily_mod <- Daily_mod[(row.drop+1):nrow(Daily_mod), ]
        Sample <- filter(Sample, Date >= min(Daily_mod$Date) & Date <= max(Daily_mod$Date))
      }
      
      # now modify and check daily flow data to make sure flow data are continous to 
      # sample data 
      
      e.name <- paste0(i,"_",params$paramShortName[j])
      eList <- mergeReport(INFO,Daily_mod,Sample,verbose = FALSE)
      
      saveRDS(eList, file = file.path(save.eLists.in,paste0(e.name,".rds")))
      
      master_list <- bind_rows(master_list, 
                               data.frame(id = paste(i, params$paramShortName[j], sep="_"),
                                          complete = TRUE,
                                          missing_all_sample = FALSE,
                                          missing_all_flow = FALSE,
                                          stringsAsFactors = FALSE,
                                          n_samples = nrow(eList$Sample),
                                          n_years = length(unique(year(eList$Sample$Date))), 
                                          n_years_consec = length(which(diff(unique(year(eList$Sample$Date)))==1))+1,
                                          samples_per_year = round(nrow(eList$Sample)/length(unique(year(eList$Sample$Date))), 1),
                                          max_time_gap_days = max.diff,
                                          n_before_gap = n.gap.before,
                                          n_after_gap = n.gap.after,
                                          n_flow_gaps = n.flow.gaps, 
                                          prop_censored = 1-round(mean(eList$Sample$Uncen), 2)))
    }
    
  }

  return(master_list)

}


plot_eLists <- function(master_list, merged.path, save.pdf.as) {
  graphics.off()
  pdf(file = save.pdf.as)

  for(id in master_list$id[master_list$complete]){
    eList <- readRDS(file.path(merged.path,paste0(id,".rds")))
    plot(eList)
  }
  dev.off()
  
}
