
plot_models <- function(status, save.pdf.as) {


  graphics.off()
  pdf(file = save.pdf.as)
  
  for(j in 1:nrow(status)){
    if (status$model_complete[j]){
      
      lm.out <- readRDS(file = status$model_path[j])
      
      par(mfrow=c(2,2), oma = c(1,1,1,1))
      for(i in 1:4){
        plot(lm.out, which=i)
        title(status$id[j])
      }
    }
    
  }
  
  dev.off()
}
