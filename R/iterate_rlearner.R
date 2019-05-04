#' Iterate the R-Learner over a set of hyperparameters
#'
#' @param gamma_omega 
#' @param var_in 
#'
#' @return
#' @export
#'
#' @examples
iterate_rlearner <- function(gamma_omega, var_in){
  params <- gamma_omega %>% str_split(pattern = "_") %>% unlist() %>% as.numeric()
  gamma <- params[1]
  omega <- params[2]
  
  result <- re_allocate(w, epsilon = 0, gamma = gamma, omega = omega, ctrl = "cisplatin") %>% 
    feed_rlearner(., X, y_complete, var = var_in, ctrl = "cisplatin") %>% 
    train_rlearner(., estimate_phat = FALSE) 
  
  result %>%
    saveRDS(here(paste0("data/", gamma_omega, "_", var_in, "_", Sys.time(), ".Rds")))
  
  return(result)
}