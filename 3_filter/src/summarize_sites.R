library(yaml)
library(dplyr)
library(readxl)

summarize_sites <- function(sample.data, site.info, min.samples) {

  summary.samples <- sample.data %>%
    group_by(SITE) %>%
    summarize(begin = min(DATE,na.rm = TRUE),
              end = max(DATE,na.rm = TRUE),
              count = n()) %>%
    filter(count > min.samples) %>%
    arrange(desc(count)) 
  
  # Note:  S is surface, B bottom and M middle.
  
  summary.samples$main_site <- regmatches(summary.samples$SITE, regexpr("[A-Za-z]{2}-[0-9]{2}", summary.samples$SITE)) 
  
  summary.samples$depth_code <- gsub(pattern = "[A-Za-z]{2}-[0-9]{2}",replacement = "", summary.samples$SITE)
  
  flow.info <- select(site.info, Site, siteID = STAID,
                      Q_1 = `USGS Primary Flow Site to use`,
                      Q_2 = `USGS Secondary Flow Site to use`,
                      Q_3 = `USGS Tertiary Flow Site to use`,
                      DA_scale = `DA scale factor`,
                      adjustment = `Flow adjustments to make`)
  
  summary.sites <- summary.samples %>%
    left_join(flow.info, by=c("main_site"="Site")) %>%
    filter(!is.na(siteID)) 
  
  return(summary.sites)
}
