#' Multiple Mann-Kendall test combined to Sen-Theil slope estimation (3 grouping variables)
#' 
#' The input dataframe must contain one value of var_y for each var_group x var_x combination.
#'
#' @param df Dataframe containing the data. Must contain the variables described below.
#' @param var_y Variable to be tested (is its slope significant ?).
#' @param var_x Variable used to order var_y (e.g. time in case of temporal trend test).
#' @param ...  Grouping variables. One slope will be estimated by grouping 
#'     variables combination. 
#'
#' @return A dataframe with the statistics for each group.
#' @export
#' 
#' @importFrom dplyr enquo arrange group_by group_modify pull ungroup mutate case_when quos
#'
#' @examples
#' \dontrun{
#' tester_pente_mk_multi3(df = fbi_metrics_median,
#'   var_x = year,
#'   var_y = p50),
#'   network
#' }
mk_st_by_group <- function(df,
                           var_y,
                           var_x,
                           ...)
  
{
  var_x <- enquo(var_x)
  var_y <- enquo(var_y)
  vars_group <- quos(...)
  
  df <- df %>% 
    filter(!is.na(!!var_y))
  
  kept_groups <- df %>% 
    group_by(!!!vars_group) %>% 
    tally() %>% 
    filter(n > 2) %>%  # MK test requires 3 data mini
    select(!!!vars_group) %>% 
    distinct()
  
  df <- kept_groups %>% 
    left_join(df)    
  
  output <- df %>%
    filter(!is.na(!!var_y)) %>% # avoids error for combinations without enough values
    arrange(!!!vars_group) %>%
    group_by(!!!vars_group) %>% 
    group_modify(~ mann_kendall_sen(.x %>% pull(!!var_y))) %>%
    ungroup() %>%
    mutate(sig = ifelse(mk_pvalue < 0.05, TRUE, FALSE),
           trend = case_when(
             sign(sens_slope) == 1 & sig ~ "Increase",
             sign(sens_slope) == -1 & sig ~ "Decrease",
             TRUE ~ "No trend"))
  
  return(output)
  
}
