library(readxl)
library(dplyr)
library(EGRET)

#------------------------------------------#
# Editing ConcHigh for right-censored data #
#------------------------------------------#
#Bring in datasets needed
setwd("C:/Users/ldecicco/DOI/GS-UMid MMSD - Trends")
eList_list <- readRDS("Data_Inputs/eLists/eList_no_dups.rds") 
sitechar <- read_xlsx("Data_Inputs/20190626_QWTrendSiteOutfallMatchups/MMSD_QWTrends_SiteOutfallMatchups.xlsx") 

#Changing Sample tables to data frames

for(i in names(eList_list)){

  eList <- eList_list[[i]]
  
  temp.sample <- eList$Sample
  
  temp.sample <- temp.sample %>% 
    mutate(constituent = substr(!!i, 7, 100),
           SiteID = substr(!!i, 1, 5)) %>%
    filter(SiteID != "LC-04", constituent != "FC_combined") 
  
  if(nrow(temp.sample) == 0){
    next
  }
  
  temp.sample <- temp.sample %>% 
    left_join(sitechar %>%
              select(SiteID, RecentDataOnly),
            by = "SiteID") %>% 
    mutate(ConcHigh.wRCDL = ifelse(is.na(ConcHigh), ConcLow, ConcHigh),
           PrePostwWYCategory = case_when(
                                    waterYear %in% c(1986:1993) ~ 1,
                                    waterYear %in% c(2010:2017) &
                                      !constituent %in% c("FC_CFU","FC_MPN") &
                                      RecentDataOnly == 0 ~ 2,
                                    waterYear %in% c(1995:2002) &
                                      constituent == "FC_MPN" &
                                      RecentDataOnly == 0 ~ 3,
                                    waterYear == 2004 ~ 4,
                                    waterYear==2016 &
                                      constituent=="FC_CFU" ~ 5,
                                    TRUE ~ 0)) %>% 
    group_by(PrePostwWYCategory) %>% 
    mutate(MaxVal = max(ConcHigh.wRCDL, na.rm=TRUE),
           DoubleMaxVal = 2*MaxVal) %>%
    ungroup() %>%
    mutate(MaxVal.POR = max(ConcHigh.wRCDL,na.rm=TRUE),
           DoubleMaxVal.POR = 2*MaxVal.POR,
           ConcHigh = case_when(is.na(ConcHigh) & PrePostwWYCategory > 0 ~ DoubleMaxVal,
                                is.na(ConcHigh) & PrePostwWYCategory == 0 ~ DoubleMaxVal.POR,
                                TRUE ~ ConcHigh))
    
  
  eList$Sample <- temp.sample
  eList <- modelEstimation(eList, minNumUncen = 30)
  eList_list[[i]] <- eList
}

saveRDS(eList_list, file = "eList_int_rt_cen.rds")
