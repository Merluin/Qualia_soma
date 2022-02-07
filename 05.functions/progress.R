progress <- function(niter, index){
  step <- niter/10
  
  if(i %% step == 0){
    pr <- paste0(rep("====", index/step), collapse = "")
    cat("\r", paste0(index/step * 10, "|"), pr, sep = "")
    utils::flush.console()
    if(index/step == 10){
      cat(" ")
      cli::cli_alert_success("DONE!")
    }
  }
}