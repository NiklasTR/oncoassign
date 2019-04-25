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
re_allocate <- function(allocation, epsilon, ctrl){
  cross <- estimate_cross(allocation, epsilon, ctrl) # %>% knitr::kable()
  
  re_allocation <- allocation %>% 
    gather(assignment, drug, -cosmic_id) %>% 
    filter(drug == 1) %>% 
    dplyr::select(- drug) %>% 
    nest(-cosmic_id) %>%
    mutate(new = purrr::map(data, ~ .x %>% mutate(re_assignment = reassign(assignment, ctrl, cross_probs = cross)))) %>% 
    unnest(new) %>% 
    dplyr::select(-data) %>% 
    arrange(cosmic_id) %>% 
    mutate(cosmic_id = as.character(cosmic_id))
  
  return(re_allocation)
}
