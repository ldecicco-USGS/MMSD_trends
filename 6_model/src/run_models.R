library(EGRET)

run_models <- function(master.list, merge.path, save.models.in){

  master.list$model_complete <- FALSE
  master.list$model_path <- NA_character_
  
  dir.create(save.models.in, showWarnings = FALSE, recursive = TRUE)
  
  
  for(i in master.list$id[master.list$complete]){
    eList <- readRDS(file.path(merge.path,paste0(i, ".rds")))
    Sample <- getSample(eList)
    row.j <- master.list$id == i
    lm.out <- lm(formula = log(ConcAve) ~ DecYear+LogQ+SinDY+CosDY,data=Sample)
    
    master.list$model_complete[row.j] <- TRUE
    master.list$model_path[row.j] <- file.path(save.models.in, paste0(i,"_lm.rds"))

    saveRDS(lm.out, file = master.list$model_path[row.j])
  }
  
  return(master.list)
}
