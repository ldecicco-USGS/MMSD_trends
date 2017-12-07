library(dataRetrieval)

get_flow <- function(summary_flow){
  
  all_flow <- readNWISdv(summary_flow$siteID,"00060",
                         startDate = min(summary_flow$start),
                         endDate = max(summary_flow$end))
  all_flow <- renameNWISColumns(all_flow)
  
  return(all_flow)
  
}
