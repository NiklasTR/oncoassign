#' Return protocol regret based on ln(IC50)
#'
#' @param rlearner_model 
#' @param response 
#'
#' @return
#' @export
#'
#' @examples
return_protocol_regret <- function(rlearner_model, response = y_complete){
  
  var_q <- sym(rlearner_model$var)
  ctrl_q <- sym(rlearner_model$ctrl)
  
  df <- rlearner_model$df[rlearner_model$index,] %>% 
    left_join(response, by = "cosmic_id") %>%
    dplyr::select(cosmic_id, !!var_q, !!ctrl_q, assignment, re_assignment, logical) %>%
    mutate(tau = !!var_q-!!ctrl_q) %>% 
    mutate(var = rlearner_model$var,
           ctrl = rlearner_model$ctrl) %>%
    mutate(regret = if_else((logical == 0 & tau < 0) | (logical == 1 & tau > 0), tau, 0))
  
  sum(abs(df$regret)) %>% return()
}
