# Fonction pour effectuer la régression linéaire et extraire les coefficients

regression_resultats <- function(data) {
  modele <- lm(valeur ~ annee, data = data)
  coef <- coef(modele)
  data.frame(
    intercept = coef[1],
    slope = coef[2]
  )
}
