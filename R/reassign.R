#' Reassign units to treatments based on a set of crossing probabilities
#'
#' @param var 
#' @param ctrl 
#' @param cross_probs
#'
#' @return
#' @export
#' @import 
#'
#' @examples
reassign <- function(var, ctrl = "cisplatin", cross_probs){
  omega <- cross_probs$cross_out %>% unique()
  
  if(var != ctrl){
    # define probs and variables for cross_over
    probs = c(cross_probs %>% filter(assignment != var) %>% .$cross_over)
    probs = c(1-sum(probs), probs)
    reassignment = c(cross_probs %>% filter(assignment != var) %>%  .$assignment)
    reassignment = c(var, reassignment)
    
    # apply cross_over rate ~ epsilon
    simple_ra(N = 1, prob_each = probs, conditions= reassignment) %>% 
      as.character() %>% 
      # apply cross_out rate ~ omega 
      c(., ctrl) %>% 
      simple_ra(N = 1, prob_each = c(1-omega, omega), conditions= .) %>%
      as.character() %>% 
      return()
  }
  else if(var == ctrl){
    # define probs and variables for cross_in
    probs = c(cross_probs %>% filter(assignment != var) %>% .$cross_in)
    probs = c(1-sum(probs), probs)
    reassignment = c(cross_probs %>% filter(assignment != var) %>%  .$assignment)
    reassignment = c(var, reassignment)
    # apply cross_in rate ~ gamma 
    simple_ra(N = 1, prob_each = probs, conditions= reassignment) %>% 
      as.character() %>% 
      return()
  }
  else {"someting is broken"}
}