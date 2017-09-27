library(dplyr)

summarize_flow <- function(summary.samples) {
  
  summary.flow <- summary.samples %>%
    select(-siteID) %>%
    gather(siteID, site_gage, -SITE, -begin, -end, -count, -DA_scale, -adjustment) %>%
    filter(site_gage != "--") %>%
    group_by(site_gage) %>%
    summarise(start = min(begin),
              end = max(end))
  
  return(summary.flow)
}
