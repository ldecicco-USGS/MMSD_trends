get_master_list <- function(ind_file){
  master_list <- readRDS(as_data_file(ind_file))
  return(master_list)
}


plot_data <- function(master_list, data_file_ind, file_out){
  eList_lists <- readRDS(as_data_file(data_file_ind))

  pdf(as_data_file(file_out))
  sapply(eList_lists, plot)
  dev.off()
  
  gd_put(file_out, local_source = file_out)
}

plot_conc_flux <- function(master_list, data_file_ind, file_out){
  eList_lists <- readRDS(as_data_file(data_file_ind))
  
  pdf(as_data_file(file_out))
  par(mfcol = c(1,2),oma = c(0, 0, 2, 0))
  sapply(eList_lists, function(x){
    plotConcHist(x, printTitle = FALSE)
    plotFluxHist(x, printTitle = FALSE)
    mtext(paste(x$INFO$shortName, x$INFO$paramShortName), outer = TRUE, cex = 1.5)
  })
  dev.off()
  gd_put(file_out, local_source = file_out)
}

plot_bias <- function(master_list, data_file_ind, file_out){
  eList_lists <- readRDS(as_data_file(data_file_ind))
  
  pdf(as_data_file(file_out))
  sapply(eList_lists, fluxBiasMulti)
  dev.off()
  gd_put(file_out, local_source = file_out)
}
