
calcul_glm <- function(df,
                       var_x,
                       var_y,
                       ...) 
  
  
{
  var_x <- enquo(var_x)
  var_y <- enquo(var_y)
  vars_group <- enquos(...)
  

  df <- df %>%
    group_by(!!!vars_group)
  
  model_glm <- glm(formula = !!var_y ~ !!var_x, 
                   family = "gaussian")
  
  return (model_glm)
}
