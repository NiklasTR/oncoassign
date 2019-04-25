#' Create a vector of response data based on assignment
#'
#' @param allocation_in 
#' @param response 
#' @param var 
#' @param ctrl 
#'
#' @return
#' @export
#'
#' @examples
create_y <- function(allocation_in, response, var, ctrl = "cisplatin"){
  response %>%
    mutate(cosmic_id = as.character(cosmic_id)) %>%
    semi_join(list$df,  by = "cosmic_id") %>%
    dplyr::select(-cosmic_id) %>% 
    
    mutate_all(funs(as.character)) %>%
    mutate_all(funs(as.numeric)) %>%
    as.matrix()
  
}