library(dplyr)

summarize_flow <- function(summary.samples) {
  
  summary.flow <- summary.samples %>%
    select(-siteID) %>%
    gather(site_rank, siteID, -SITE, -begin, -end, -count, -DA_scale, -adjustment) %>%
    filter(siteID != "--") %>%
    group_by(siteID) %>%
    summarise(start = min(begin),
              end = max(end))
  
  return(summary.flow)
}
