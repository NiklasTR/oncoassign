#' Estimate cross-over and cross-in probabilities from allocations
#'
#' @param allocation 
#' @param epsilon 
#' @param ctrl_name 
#'
#' @return
#' @export
#'
#' @examples
estimate_cross <- function(allocation, 
                           epsilon = 0, # The star schema does not support epsilon > 0
                           gamma = 0,
                           omega = 0, 
                           ctrl_name = "cisplatin"){
  # calculating proportions
  df <- allocation %>% 
    gather(assignment, drug, -cosmic_id) %>% 
    filter(drug == 1) %>% 
    count(assignment) %>% 
    mutate(total = sum(n),
           w = n/total,
           type = if_else(assignment == ctrl_name, "ctrl", "treatment")) %>% 
    dplyr::select(-total)
  
  # calculating proportion of control for scaling
  ctrl_prop <- df %>% filter(type == "ctrl") %>% .$w %>% unique()
  # calculating proportion of treated for scaling
  treat_prop <- df %>% filter(type != "ctrl") %>% .$w %>% sum()
  
  # estimating gamma
  if(is.na(gamma)){
  gamma <- ctrl_prop*epsilon
  }
  
  
  # calculating cross_in and cross_over
  df %>% filter(type != "ctrl")  %>% 
    mutate(gamma = gamma) %>%
    mutate(epsilon = epsilon) %>%
    dplyr::select(-type) %>% 
    mutate(cross_in = gamma*(w/treat_prop),
           cross_over = epsilon*(w/treat_prop),
           cross_out = omega) %>%
    return()
}