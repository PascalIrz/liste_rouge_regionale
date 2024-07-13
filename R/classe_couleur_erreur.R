#' Déterminer le degré d'abérrance des valeurs de tailles non comrpises dans les seuils calculés par qtp_seuils (fonction de aspeQual)
#'
#' @param var_taille Données de tailles
#' @param valeur_mini Valeur de tailles minimum calculée par la fonction qtp_seuils d'aspeQual
#' @param var_rouge Valeur de dépassement de seuil considéré comme "très fortement abérrant"
#' @param var_darkorange Valeur de dépassement de seuil considéré comme "très abérrant"
#' @param var_orange Valeur de dépassement de seuil considéré comme "abérrant"
#' @param var_beige Valeur de dépassement de seuil considéré comme "minim"
#' @return Dataframe avec une ligne par mesure de taille et des indications sur son "degré d'abérrance" fixé dans les params
#' @export
#' 
#' @importClassesFrom dplyr if return else if else
#'
#' @examples
#' \dontrun{
#' prov <- classe_couleur_erreur(df = annee_de_donnee,
#' var_id_site = pop_id,
#' var_temp = annee)
#' }

classe_couleur_erreur <- function(var_taille, 
                                  valeur_mini, 
                                  valeur_maxi, 
                                  var_rouge, 
                                  var_darkorange, 
                                  var_orange) 
  {
  # Fonction pour définir les classes de couleurs
  if (var_taille < valeur_mini - var_rouge || var_taille > valeur_maxi + var_rouge) {
    return("red")       # Très fortement aberrant
  } else if (var_taille < valeur_mini - var_darkorange || var_taille > valeur_maxi + var_darkorange) {
    return("darkorange") # Très aberrant
  } else if (var_taille < valeur_mini - var_orange || var_taille > valeur_maxi + var_orange) {
    return("orange")    # Aberrant
  } else {
    return("beige")     # Minim
  }
}
