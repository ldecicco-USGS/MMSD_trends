get_data <- function(master_list, master_list_ind, ind_file){

  tempFolder  <- tempdir()
  big_eList <- list()
  
  index <- 0
  master_list$model_complete <- FALSE
  
  for(i in 1:length(master_list$id[master_list$complete])){
    file_name <- paste0("1_get_condor_data/raw_condor/trendsCI_",index,".zip.ind")
    data_file <- gd_get(ind_file = file_name, overwrite = TRUE)
    
    zipped_files <- unzip(data_file, exdir = tempFolder)

    if(any(grepl(pattern = "eList.rds",x = zipped_files))){
      eList <- readRDS(file.path(tempFolder,"eList.rds"))
      id <- paste0(eList$INFO$shortName,"_",eList$INFO$paramShortName)
      id_index <- which(master_list$id == id)
      master_list$model_complete[id_index] <- TRUE
      saveRDS(master_list, file = as_data_file(master_list_ind))
      big_eList[[id]] <- eList
      saveRDS(big_eList, file = as_data_file(ind_file))
      file.remove(zipped_files)
    }
    index <- index + 1
  }
  gd_put(master_list_ind, as_data_file(master_list_ind))
  gd_put(ind_file, as_data_file(ind_file))
}