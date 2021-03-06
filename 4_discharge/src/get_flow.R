library(dataRetrieval)

get_flow <- function(summary.flow){
  
  all_flow <- readNWISdv(summary.flow$siteID,"00060",
                         startDate = min(summary.flow$start),
                         endDate = max(summary.flow$end))
  all_flow <- renameNWISColumns(all_flow)
  
  return(all_flow)
  
}
