#' Train R-Learner
#'
#' @param list 
#' @param estimate_phat 
#'
#' @return
#' @export
#'
#' @examples
train_rlearner <- function(list, estimate_phat = TRUE){
  if(estimate_phat == TRUE){
    # running the rlearner
    list$rlasso_fit = rlasso(list$x[list$index,], list$w[list$index], list$y[list$index])
    list$rlasso_est = predict(list$rlasso_fit, list$x[-list$index,])
    
    list$rboost_fit = rboost(list$x[list$index,], list$w[list$index], list$y[list$index])
    list$rboost_est = predict(list$rboost_fit, list$x[-list$index,])
    
    return(list)
  } else if(estimate_phat == FALSE){
    # running the rlearner
    list$rlasso_fit = rlasso(list$x[list$index,], list$w[list$index], list$y[list$index], p_hat = list$p[list$index])
    list$rlasso_est = predict(list$rlasso_fit, list$x[-list$index,])
    
    list$rboost_fit = rboost(list$x[list$index,], list$w[list$index], list$y[list$index], p_hat = list$p[list$index])
    list$rboost_est = predict(list$rboost_fit, list$x[-list$index,])
    
    return(list)
  }
}
