g_sp_sen_trends_indicators <-
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
    
    # from https://stackoverflow.com/questions/48349858/how-can-i-use-theil-sen-method-with-geom-smooth
    
    sen <- function(..., weights = NULL) {
      mblm::mblm(...)
    }
    
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
      geom_smooth(aes(linetype = ((
        !!var_sig == "No trend"
      ))),
      method = sen,
      se = FALSE,
      col = "red") +
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
