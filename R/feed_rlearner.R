#' Prepare list of inputs to train the R-Learner
#'
#' @param allocation_in 
#' @param features 
#' @param response 
#' @param var 
#' @param ctrl 
#' @param split 
#'
#' @return
#' @export
#'
#' @examples
feed_rlearner <- function(allocation_in, features, response, var, ctrl = "cisplatin", split = 0.8){
  # I create a list to organize elements in the function
  list <- list()
  
  list$var = var
  list$ctrl = ctrl 
  
  var_q <- sym(var)
  ctrl_q <- sym(ctrl)
  
  list$df <- allocation_in$re_allocation %>% 
    mutate(cosmic_id = as.character(cosmic_id)) %>%
    semi_join(response, by = "cosmic_id") %>%
    filter(re_assignment == var |  re_assignment == ctrl) %>% 
    # In our current scenario, we can only consider cases that were included in either the treatment of interest of ctrl arm
    filter(assignment == var |  assignment == ctrl) %>% 
    mutate(logical = if_else(re_assignment == ctrl, 0, 1),
           cosmic_id = as.character(cosmic_id))
  
  list$x <- features %>% 
    mutate(cosmic_id = as.character(cosmic_id)) %>%
    semi_join(list$df,  by = "cosmic_id") %>%
    dplyr::select(-cosmic_id) %>%
    mutate_all(funs(as.character)) %>%
    mutate_all(funs(as.numeric)) %>%
    as.matrix()
  
  # getting df into shape
  
  list$df <- list$df %>% 
    semi_join(features %>% 
                mutate(cosmic_id = as.character(cosmic_id)) %>%
                semi_join(list$df,  by = "cosmic_id"), 
              by = "cosmic_id")
  
  list$w <- list$df$logical
  
  list$y <- response %>%
    mutate(cosmic_id = as.character(cosmic_id)) %>%
    left_join(list$df ,.,  by = "cosmic_id") %>%
    dplyr::select_(var, ctrl, "cosmic_id", "re_assignment") %>% 
    mutate(y = if_else(re_assignment == ctrl, !!ctrl_q, !!var_q)) %>% 
    .$y
  
  
  list$p <- list$df %>% mutate(initial_logical = if_else(re_assignment == ctrl, 0, 1)) %>% 
    mutate(p_hat = if_else(initial_logical == 1, 
                           1-(allocation_in$cross %>% filter(assignment == var) %>% .$cross_out), 
                           allocation_in$cross %>% filter(assignment == var) %>% .$cross_in)) %>% 
    .$p_hat
  
  list$index <- c(1:length(list$w)) %>% sample(., size = round(length(.)*split, 1))
  
  return(list)
}
