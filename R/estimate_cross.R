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
                           epsilon = 0.05,
                           gamma = NA,
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
  
  # estimating gamma
  if(is.na(gamma)){
  gamma <- df %>% 
    filter(type == "ctrl") %>% 
    .$w # in this case 1- the cummulative w
  gamma <- gamma*epsilon
  }
  
  # calculating cross_in and cross_over
  df %>% filter(type != "ctrl")  %>% 
    mutate(gamma = gamma) %>%
    mutate(epsilon = epsilon) %>%
    dplyr::select(-type) %>% 
    mutate(cross_in = gamma*w,
           cross_over = epsilon*w) %>%
    return()
}