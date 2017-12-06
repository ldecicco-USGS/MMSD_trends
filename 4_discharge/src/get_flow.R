library(dataRetrieval)

get_flow <- function(summary.flow, ind_file){
  
  all_flow <- readNWISdv(summary.flow$siteID,"00060",
                         startDate = min(summary.flow$start),
                         endDate = max(summary.flow$end))
  all_flow <- renameNWISColumns(all_flow)
  
  data_file <- as_data_file(ind_file)
  saveRDS(all_flow, file = data_file)
  
  gd_put(ind_file, data_file)
  
}
