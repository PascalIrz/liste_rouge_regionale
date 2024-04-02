
calcul_glm <- function(df,
                       var_x,
                       var_y,
                       var_z,
                       log = TRUE, 
                       ...) 
  
{
  var_x <- enquo(var_x)
  var_y <- enquo(var_y)
  var_z <- enquo(var_z)
  vars_group <- quos(...)
  
  
  df <- df %>%
    group_by(!!!vars_group) %>% 
  mutate (valeur_l = ifelse (grepl("densite", indicateur),
                               log(1 + !!var_y),
                               !!var_y))


  ggplot(df,
         aes(x = valeur_l)) +
    geom_histogram()
  
  annees <- c(!!var_x)
  
  
  model <- glm(formula = valeur_l ~ annees,
             data = df,
             family = "gaussian")


 #  dist_cook <- cooks.distance(model)
 #  seuil_cook <- 4 / nrow(df)
 #  
 #  df <- df %>% 
 #    cbind(dist_cook) %>% # ajout de la colonne avec les distances
 #    filter(dist_cook < seuil_cook) # suppression des observations avec distance > 4/N
 #  
 #  
 #  model <- glm(formula = valeur_l ~ !!var_x + !!var_z, 
 #               data = df,
 #               family = "gaussian")
 #  
 #  ggplot(data = df, 
 #         aes(x = !!var_x, 
 #             y = !!var_y)) +
 #    geom_point() +
 #    geom_smooth(method = "lm")
 #  
 # return(model)
 #  
}
  