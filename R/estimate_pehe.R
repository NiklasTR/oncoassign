#' Estimate PEHE after training the R-Learner
#'
#' @param rlearner_model 
#' @param response 
#'
#' @return
#' @export
#'
#' @examples
estimate_pehe <- function(rlearner_model, response = y_complete){
  var_q <- sym(rlearner_model$var)
  ctrl_q <- sym(rlearner_model$ctrl)
  
  rlearner_model$df[-rlearner_model$index,] %>% 
    left_join(response, by = "cosmic_id") %>%
    dplyr::select(cosmic_id, !!var_q, !!ctrl_q, assignment, re_assignment, logical) %>%
    mutate(tau = !!var_q-!!ctrl_q) %>% 
    mutate(tau_hat_lasso = rlearner_model$rlasso_est) %>% 
    mutate(tau_hat_boost = rlearner_model$rboost_est) %>% 
    mutate(pehe_lasso = (tau_hat_lasso - tau)^2,
           pehe_boost = (tau_hat_boost - tau)^2) %>% 
    mutate(var = rlearner_model$var,
           ctrl = rlearner_model$ctrl) %>%
    return()
}