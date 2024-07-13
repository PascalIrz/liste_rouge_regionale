#' Détecter les tailles de poissons abérantes dans un jeu de données à partir de 
#' données minimales et maximales de tailles issues de la fonction qtf_seuils - AspeQual
#'
#' @param df Dataframe contenant les données de tailles ainsi que les seuils min et max calculés par la fonction qtp_seuils
#'
#' @return Dataframe avec une ligne par taille jugée anormale et valeurs maxi et mini de la taille "standard" de l'espèce
#' @export
#' 
#' @importClassesFrom dplyr filter select if else return print
#'
#' @examples
#' \dontrun{
#' prov <- detecter_erreurs_tailles(df = mei_data)
#' }
detecter_erreurs_tailles <- function(df) {

  erreurs <- filter(df, mei_taille < mini | mei_taille > maxi) %>%   # Filtrer les données où les valeurs de mei_taille sont en dehors des seuils
    select(ope_id, esp_code_alternatif, mei_id, mini, maxi, mei_taille)
  
  if (nrow(erreurs) > 0) {
    message("Les données suivantes ont des valeurs de mei_taille en dehors des seuils :")
    print(erreurs)
    return(erreurs)  # Retourne le dataframe des erreurs
  } else {
    message("Aucune erreur détectée.")
    return(NULL)  # Retourne NULL s'il n'y a pas d'erreur
  }
}
