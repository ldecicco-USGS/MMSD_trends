library(yaml)
library(dplyr)
library(readxl)

summarize_sites <- function(sample.data, site.info) {

  summary.samples <- sample.data %>%
    group_by(SITE) %>%
    summarize(begin = min(DATE,na.rm = TRUE),
              end = max(DATE,na.rm = TRUE),
              count = n()) %>%
    arrange(desc(count)) 
  
flow.info <- select(site.info, Site, siteID = STAID,
                      Q_1 = `USGS Primary Flow Site to use`,
                      Q_2 = `USGS Secondary Flow Site to use`,
                      Q_3 = `USGS Tertiary Flow Site to use`,
                      Q_4 = `USGS Quaternary Flow Site to use`,
                      DA_scale = `DA scale factor`,
                      adjustment = `Flow adjustments to make`)
  summary.sites <- summary.samples %>%
    left_join(flow.info, by=c("SITE"="Site"))%>%
    filter(Q_1 != "--") 
  
  return(summary.sites)
}
