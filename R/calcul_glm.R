# Fonction pour une combi indicateur x espece x stade
calcul_glm_non_groupe <- function(df) 
  
{
  # premier modele
  model <- glm(formula = valeur_l ~ annee,
               data = df,
               family = "gaussian")
  
  # élimination des outliers
  dist_cook <- cooks.distance(model)
  seuil_cook <- 4 / nrow(df)

  # modele sans les outliers
  df <- df %>%
    cbind(dist_cook) %>% # ajout de la colonne avec les distances
    filter(dist_cook < seuil_cook) # suppression des observations avec distance > 4/N


  model <- glm(formula = valeur_l ~ annee,
               data = df,
               family = "gaussian")

  # récupération des résultats sous forme de dataframe en vue de pouvoir les empiler
  result <- data.frame(
    intercept = model$coefficients[["(Intercept)"]],
    annee = model$coefficients[["annee"]],
    pvalue = as.data.frame(summary(model)$coefficients) %>%
      rownames_to_column() %>%
      filter(rowname == "annee") %>%
      pull(`Pr(>|t|)`)
    )
  
  
  result

}
