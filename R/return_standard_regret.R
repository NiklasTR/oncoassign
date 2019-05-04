#' Return standard regret based on ln(IC50)
#'
#' @param var 
#' @param ctrl 
#' @param response 
#'
#' @return
#' @export
#'
#' @examples
return_standard_regret <- function(var, ctrl = "cisplatin", response = y_complete){
  
  var_q <- sym(var)
  ctrl_q <- sym(ctrl)
  
  df <- re_allocate(w, epsilon = 0, gamma = 0, omega = 0, ctrl = "cisplatin") %>% 
    feed_rlearner(., X, y_complete, var = var, ctrl = ctrl) %>% 
    .$df %>% 
    left_join(response, by = "cosmic_id") %>%
    dplyr::select(cosmic_id, !!var_q, !!ctrl_q, assignment, re_assignment, logical) %>%
    mutate(tau = !!var_q-!!ctrl_q) %>% 
    mutate(regret = if_else((logical == 0 & tau < 0) | (logical == 1 & tau > 0), tau, 0))
  
  sum(abs(df$regret)) %>% return()
}
