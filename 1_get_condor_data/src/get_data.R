get_data <- function(master_list, ind_file){

  tempFolder  <- tempdir()
  big_eList <- list()
  
  index <- 0
  master_list$model_complete <- FALSE
  
  for(i in 1:length(master_list$id[master_list$complete])){
    file_name <- paste0("1_get_condor_data/raw_condor/trendsCI_",index,".zip.ind")
    data_file <- gd_get(ind_file = file_name)
    
    zipped_files <- unzip(data_file, exdir = tempFolder)
    eList <- readRDS(file.path(tempFolder,"eList.rds"))
    id <- paste0(eList$INFO$shortName,"_",eList$INFO$paramShortName)
    id_index <- which(master_list$id == id)
    master_list$model_complete[id_index] <- TRUE
    
    big_eList[[id]] <- eList
    index <- index + 1
    file.remove(zipped_files)
  }
  
  saveRDS(big_eList, file = as_data_file(ind_file))
  gd_put(ind_file, as_data_file(ind_file))
}