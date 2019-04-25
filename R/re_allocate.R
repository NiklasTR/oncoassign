#' Re-allocate units based on cross-over and cross-in probabilities
#'
#' @param allocation 
#' @param epsilon 
#' @param ctrl_name 
#'
#' @return
#' @export
#'
#' @examples
re_allocate <- function(allocation, epsilon, gamma, omega, ctrl, return_propensity = TRUE){
  list <- list()
  list$cross <- estimate_cross(allocation, epsilon, gamma, omega, ctrl) # %>% knitr::kable()
  
  list$re_allocation <- allocation %>% 
    gather(assignment, drug, -cosmic_id) %>% 
    filter(drug == 1) %>% 
    dplyr::select(- drug) %>% 
    nest(-cosmic_id) %>%
    mutate(new = purrr::map(data, ~ .x %>% mutate(re_assignment = reassign(assignment, ctrl, cross_probs = list$cross)))) %>% 
    unnest(new) %>% 
    dplyr::select(-data) %>% 
    arrange(cosmic_id) %>% 
    mutate(cosmic_id = as.character(cosmic_id))
  
  if(return_propensity == TRUE){
    return(list)
  }
  else if(return_propensity == FALSE){
    return(re_allocation)
  }
}
