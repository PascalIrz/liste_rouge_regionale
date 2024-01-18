############# TESTS ###############

# d'après https://stackoverflow.com/questions/64845777/compute-statistical-tests-groupwise-in-r-using-dplyr


#' Mann-Kendall test combined to Sen-Theil slope estimation on a vector
#'
#' @param x A vector of class "numeric" or a time series object of class "ts".
#' @param ... Arguments to be passed to the function trend::mk.test()
#'
#' @return A dataframe with the test statistics.
#' @export
#' 
#' @importFrom trend mk.test sens.slope
#'
#' @examples
#' vector <- c(0, 3, 2, 5, 7, 6, 9, 8, 13, 16, 12)
#' mann_kendall_sen(vector)
mann_kendall_sen <- function(x, ...) 
{
  mk_pvalue <- mk.test(x, ...)
  sens_slope <- sens.slope(x, ...)
  
  # output
  data.frame(mk_pvalue = mk_pvalue$p.value,
             sens_slope = sens_slope$estimates)
}


#' Multiple Mann-Kendall test combined to Sen-Theil slope estimation (1 grouping variable)
#' 
#' The input dataframe must contain one value of var_y for each var_group x var_x combination.
#' The groups with less than 3 observations are removed
#'
#' @param df Dataframe containing the data. Must contain the variables described below.
#' @param var_groupe Grouping variable. One slope will be estimated by group. 
#' @param var_y Variable to be tested (is its slope significant ?).
#' @param var_x Variable used to order var_y (e.g. time in case of temporal trend test).
#'
#' @return A dataframe with the statistics for each group.
#' @export
#' 
#' @importFrom dplyr enquo arrange group_by group_modify pull ungroup mutate case_when
#'
#' @examples
#' \dontrun{
#' tester_pente_mk_multi(df = fbi_metrics_median,
#'   var_groupe = metric,
#'   var_x = year,
#'   var_y = p50)
#' }
tester_pente_mk_multi <- function(df,
                                  var_groupe,
                                  var_y,
                                  var_x)
  
{
  var_groupe <- enquo(var_groupe)
  var_x <- enquo(var_x)
  var_y <- enquo(var_y)
  
  df <- df %>% 
    filter(!is.na(!!var_y))
  
  kept_groups <- df %>% 
    group_by(!!var_groupe) %>% 
    tally() %>% 
    filter(n > 2) %>%  # MK test requires 3 data mini
    pull(!!var_groupe) %>% 
    unique()
  
  df <- df %>% 
    filter(!!var_groupe %in% kept_groups)           
  
  output <- df %>%
    arrange(!!var_groupe, !!var_x) %>% 
    group_by(!!var_groupe) %>%
    group_modify(~ mann_kendall_sen(.x %>% pull(!!var_y))) %>% 
    ungroup() %>% 
    mutate(sig = ifelse(mk_pvalue < 0.05, TRUE, FALSE),
           trend = case_when(
             sign(sens_slope) == 1 & sig ~ "Increase",
             sign(sens_slope) == -1 & sig ~ "Decrease",
             TRUE ~ "No trend"))
  
  return(output)
  
}

#' Multiple Mann-Kendall test combined to Sen-Theil slope estimation (2 grouping variables)
#' 
#' The input dataframe must contain one value of var_y for each var_group x var_x combination.
#'
#' @param df Dataframe containing the data. Must contain the variables described below.
#' @param var_groupe1,var_groupe2  Grouping variables. One slope will be estimated by grouping 
#'     variables combination. 
#' @param var_y Variable to be tested (is its slope significant ?).
#' @param var_x Variable used to order var_y (e.g. time in case of temporal trend test).
#'
#' @return A dataframe with the statistics for each group.
#' @export
#' 
#' @importFrom dplyr enquo arrange group_by group_modify pull ungroup mutate case_when
#'
#' @examples
#' \dontrun{
#' tester_pente_mk_multi2(df = fbi_metrics_median,
#'   var_groupe1 = metric,
#'   var_groupe1 = network,
#'   var_x = year,
#'   var_y = p50)
#' }
tester_pente_mk_multi2 <- function(df,
                                   var_groupe1 = NULL,
                                   var_groupe2 = NULL,
                                   var_y,
                                   var_x)
  
{
  var_groupe1 <- enquo(var_groupe1)
  var_groupe2 <- enquo(var_groupe2)
  var_x <- enquo(var_x)
  var_y <- enquo(var_y)
  
  df <- df %>% 
    filter(!is.na(!!var_y))
  
  kept_groups <- df %>% 
    group_by(!!var_groupe1, !!var_groupe2) %>% 
    tally() %>% 
    filter(n > 2) %>%  # MK test requires 3 data mini
    select(!!var_groupe1, !!var_groupe2) %>% 
    distinct()
  
  df <- kept_groups %>% 
    left_join(df)    
  
  output <- df %>%
    filter(!is.na(!!var_y)) %>% # avoids error for combinations without enough values
    arrange(!!var_groupe1, !!var_groupe2, !!var_x) %>%
    group_by(!!var_groupe1, !!var_groupe2) %>% 
    group_modify(~ mann_kendall_sen(.x %>% pull(!!var_y))) %>%
    ungroup() %>%
    mutate(sig = ifelse(mk_pvalue < 0.05, TRUE, FALSE),
           trend = case_when(
             sign(sens_slope) == 1 & sig ~ "Increase",
             sign(sens_slope) == -1 & sig ~ "Decrease",
             TRUE ~ "No trend"))
  
  
  
  
  return(output)
  
}

#' Multiple Mann-Kendall test combined to Sen-Theil slope estimation (3 grouping variables)
#' 
#' The input dataframe must contain one value of var_y for each var_group x var_x combination.
#'
#' @param df Dataframe containing the data. Must contain the variables described below.
#' @param var_groupe1,var_groupe2,var_groupe3  Grouping variables. One slope will be estimated by grouping 
#'     variables combination. 
#' @param var_y Variable to be tested (is its slope significant ?).
#' @param var_x Variable used to order var_y (e.g. time in case of temporal trend test).
#'
#' @return A dataframe with the statistics for each group.
#' @export
#' 
#' @importFrom dplyr enquo arrange group_by group_modify pull ungroup mutate case_when
#'
#' @examples
#' \dontrun{
#' tester_pente_mk_multi3(df = fbi_metrics_median,
#'   var_groupe1 = metric,
#'   var_groupe1 = network,
#'   var_groupe3 = pop_id,
#'   var_x = year,
#'   var_y = p50)
#' }
tester_pente_mk_multi3 <- function(df,
                                   var_groupe1 = NULL,
                                   var_groupe2 = NULL,
                                   var_groupe3 = NULL,
                                   var_y,
                                   var_x)
  
{
  var_groupe1 <- enquo(var_groupe1)
  var_groupe2 <- enquo(var_groupe2)
  var_groupe3 <- enquo(var_groupe3)
  var_x <- enquo(var_x)
  var_y <- enquo(var_y)
  
  df <- df %>% 
    filter(!is.na(!!var_y))
  
  kept_groups <- df %>% 
    group_by(!!var_groupe1, !!var_groupe2, !!var_groupe3) %>% 
    tally() %>% 
    filter(n > 2) %>%  # MK test requires 3 data mini
    select(!!var_groupe1, !!var_groupe2, !!var_groupe3) %>% 
    distinct()
  
  df <- kept_groups %>% 
    left_join(df)    
  
  output <- df %>%
    filter(!is.na(!!var_y)) %>% # avoids error for combinations without enough values
    arrange(!!var_groupe1, !!var_groupe2, !!var_groupe3, !!var_x) %>%
    group_by(!!var_groupe1, !!var_groupe2, !!var_groupe3) %>% 
    group_modify(~ mann_kendall_sen(.x %>% pull(!!var_y))) %>%
    ungroup() %>%
    mutate(sig = ifelse(mk_pvalue < 0.05, TRUE, FALSE),
           trend = case_when(
             sign(sens_slope) == 1 & sig ~ "Increase",
             sign(sens_slope) == -1 & sig ~ "Decrease",
             TRUE ~ "No trend"))
  
  
  
  
  return(output)
  
}

############ RECODING ###################

latin_recoding <- function(df, var_latin_name)
{
  var_latin_name <- enquo(var_latin_name)
  
  df <- df %>% 
    mutate(!!var_latin_name := case_when(
      !!var_latin_name == "Carassius" ~ "Carassius spp",
      !!var_latin_name == "Cottus gobio" ~ "Cottus spp",
      !!var_latin_name == "Carassius" ~ "Carassius spp",
      !!var_latin_name == "Leuciscus leuciscus" ~ "Leuciscus spp",
      !!var_latin_name == "Leuciscus cephalus" ~ "Squalius cephalus",
      TRUE ~ !!var_latin_name
      
    )
    )
  
  df
  
}


####### PLOTS #########################

g_temp_metrics <- function(df, var_x, var_y, var_facet, var_sig, scales = "free_y")
  
{
  # quasiquotation / lazy eval
  var_x <- enquo(var_x)
  var_y <- enquo(var_y)
  var_facet <- enquo(var_facet)
  var_sig <- enquo(var_sig)
  
  # plot  
  ggplot(data = df,
         aes(x = !!var_x,
             y = !!var_y)) +
    geom_line() +
    geom_point() +
    geom_smooth(aes(linetype = (!!var_sig)),
                method = lm,
                se = FALSE) +
    labs(y = "Metric value", x = "Year") +
    facet_wrap(vars(!!var_facet), scales = scales) +
    scale_linetype_manual(values = c("dotted", "solid")) +
    theme(legend.position = "none")
  
  
}

# the same as above but with a grouping variable to plot refnet and repnet on same figure
g_temp_metrics2 <- function(df, var_x, var_y, var_group, var_facet, var_sig, scales = "free_y")
  
{
  # quasiquotation / lazy eval
  var_x <- enquo(var_x)
  var_y <- enquo(var_y)
  var_group <- enquo(var_group)
  var_facet <- enquo(var_facet)
  var_sig <- enquo(var_sig)
  
  # plot  
  ggplot(data = df,
         aes(x = !!var_x,
             y = !!var_y,
             col = !!var_group,
             linetype = (!!var_sig))) +
  #  geom_line() +
    geom_point() +
    geom_smooth(method = lm,
                se = FALSE) +
    labs(y = "Metric value", x = "Year") +
    facet_wrap(vars(!!var_facet), scales = scales) +
    scale_linetype_manual(values = c("dotted", "solid"), name = "", labels = NULL) +
   # theme(legend.position = "none") +
    scale_color_manual(
      values = c("green3", "brown"),
      labels = c("REFNET", "REPNET"),
      name = "Network",
      guide = guide_legend(reverse = TRUE)
    ) +
    theme_bw() +
    theme(legend.position = "bottom") +
    guides(linetype = "none")
  
  
}

# the same as above but with a grouping variable to plot refnet and repnet on same figure
# g_temp_metrics3 <- function(df, var_x, var_y, var_group, var_rows, var_cols, var_sig, scales = "free_y")
#   
# {
#   # quasiquotation / lazy eval
#   var_x <- enquo(var_x)
#   var_y <- enquo(var_y)
#   var_group <- enquo(var_group)
#   var_rows <- enquo(var_rows)
#   var_cols <- enquo(var_cols)
#   var_sig <- enquo(var_sig)
#   
#   # plot  
#   ggplot(data = df,
#          aes(x = !!var_x,
#              y = !!var_y,
#              col = !!var_group,
#              linetype = (!!var_sig))) +
#     #  geom_line() +
#     geom_point() +
#     geom_smooth(method = lm,
#                 se = FALSE) +
#     labs(y = "Metric value", x = "Year") +
#     facet_wrap(vars(!!var_facet), scales = scales) +
#     scale_linetype_manual(values = c("dotted", "solid"), name = "", labels = NULL) +
#     # theme(legend.position = "none") +
#     scale_color_manual(
#       values = c("green3", "brown"),
#       labels = c("REFNET", "REPNET"),
#       name = "Network",
#       guide = guide_legend(reverse = TRUE)
#     ) +
#     theme_bw() +
#     theme(legend.position = "bottom") +
#     guides(linetype = "none")
#   
#   
# }


g_temp_species_occurrence <-
  function(df,
           var_x,
           var_y,
           var_facet,
           var_sig,
           scales = "free_y",
           x_lab = "Year",
           y_lab)
    
  {
    # quasiquotation / lazy eval
    var_x <- enquo(var_x)
    var_y <- enquo(var_y)
    var_facet <- enquo(var_facet)
    var_sig <- enquo(var_sig)
    
    # plot
    ggplot(data = df,
           aes(x = !!var_x,
               y = !!var_y)) +
      geom_line() +
      geom_point() +
      geom_smooth(aes(linetype = ((
        !!var_sig == "No trend"
      ))),
      method = lm,
      se = FALSE) +
      labs(y = y_lab, x = x_lab) +
      facet_wrap(vars(!!var_facet),
                 scales = scales,
                 ncol = 5) +
      scale_linetype_manual(values = c("solid", "dotted")) +
      scale_y_continuous(labels = scales::label_percent()#,
                         #limits = c(0, NA)
                         ) +
      theme(legend.position = "none")
    
    
  }


##
g_sp_trends_indicators <-
  function(df,
           var_x,
           var_y,
           var_facet_col,
           var_facet_row,
           var_sig,
           scales = "free_y",
           ncol = 5,
           x_lab = "Year",
           y_lab)
    
  {
    # quasiquotation / lazy eval
    var_x <- enquo(var_x)
    var_y <- enquo(var_y)
    var_facet_col <- enquo(var_facet_col)
    var_facet_row <- enquo(var_facet_row)
    var_sig <- enquo(var_sig)

    # plot
    ggplot(data = df %>% 
             mutate(!!var_facet_row := str_wrap(!!var_facet_row, 15)),
           aes(x = !!var_x,
               y = !!var_y)) +
      geom_line() +
      geom_point() +
      geom_smooth(aes(linetype = ((
        !!var_sig == "No trend"
      ))),
      method = lm,
      se = FALSE) +
      labs(y = y_lab, x = x_lab) +
      facet_wrap(facets = vars(!!var_facet_row, !!var_facet_col),
                 ncol = ncol,
                 scales = scales) +
      scale_linetype_manual(values = c("solid", "dotted")) +
      theme(legend.position = "none",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"),
            strip.text = element_text(size = 6)
              )
    
    
  }

g_sp_trends_indicators2 <-
  function(df,
           var_x,
           var_y,
           var_facet_col,
           var_facet_row,
           var_sig,
           var_col,
           scales = "free_y",
           ncol = 5,
           x_lab = "Year",
           y_lab)
    
  {
    # quasiquotation / lazy eval
    var_x <- enquo(var_x)
    var_y <- enquo(var_y)
    var_col <- enquo(var_col)
    var_facet_col <- enquo(var_facet_col)
    var_facet_row <- enquo(var_facet_row)
    var_sig <- enquo(var_sig)
    
    # plot
    ggplot(data = df %>% 
           mutate(!!var_facet_row := str_wrap(!!var_facet_row, 15)),
           aes(x = !!var_x,
               y = !!var_y,
               col = !!var_col)) +
      geom_point() +
      geom_smooth(aes(linetype = ((
        !!var_sig == "No trend"
      ))),
      method = lm,
      se = FALSE) +
      labs(y = y_lab, x = x_lab) +
      facet_wrap(facets = vars(!!var_facet_row, !!var_facet_col),
                 ncol = ncol,
                 scales = scales) +
      # solid line if var_sig != "No trend"
      scale_linetype_manual(values = setNames(c('solid', 'dashed'), c(FALSE, TRUE)),
                            guide = 'none') +
      theme(legend.position = "bottom",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"),
            strip.text = element_text(size = 6,
                                      margin = margin(.1, 0, .1, 0, "cm"))
      ) +
      scale_color_manual(
        values = c("green3", "brown"),
        labels = c("REFNET", "REPNET"),
        name = "Network",
        guide = guide_legend(reverse = TRUE)
      ) +
      theme_bw() +
      theme(legend.position = "bottom")
    
    
  }
