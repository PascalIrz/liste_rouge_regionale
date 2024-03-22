#' Calculer les taux d'évolution des espèces par annees selon la variable souhaitée
#'
#' @param df Dataframe contenant les données
#' @return Dataframe avec les taux d'évolution par espèce de la variable souhaitée
#' @export
#' 
#' @importClassesFrom dplyr enquo group_by mutate filter summarize ungroup select unique
#'
#' @examples
#' \dontrun{
#' prov <- calcul_taux_evolution(df = ope_indicateur,
#' var_annee = annee,
#' var_statut = statut
#' var_esp_code_alternatif = esp_code_alternatif,
#' var_valeur = valeur)
#' }

calcul_taux_evolution <- function(df,
                             var_annee,
                             var_statut,
                             var_esp_code_alternatif,
                             var_valeur) {
  
  # Convertir les variables en quosures
  var_annee <- enquo(var_annee)
  var_statut <- enquo(var_statut)
  var_esp_code_alternatif <- enquo(var_esp_code_alternatif)
  var_valeur <- enquo(var_valeur)
  
  
  resultats <- list()
  especes <- unique(df[[as_label(var_esp_code_alternatif)]])
  
  # Parcourir chaque espèce
  for (espece in especes) {
    # Filtrer les données pour l'espèce spécifique
    espece_data <- filter(df, !!var_esp_code_alternatif == espece & !!var_statut == "toutes")
    
    # Trier les données par année
    espece_data <- dplyr::arrange(espece_data,!!var_annee)
    
    # Initialiser un vecteur pour stocker les taux d'évolution
    taux_evol <- c(NA)
    
    # Calculer les taux d'évolution pour chaque année
    for (i in 2:nrow(espece_data)) {
      valeur_actuelle <- espece_data[[as_label(var_valeur)]][i]
      valeur_precedente <- espece_data[[as_label(var_valeur)]][i - 1]
      
      # Gérer les valeurs manquantes
      if (is.na(valeur_actuelle) || is.na(valeur_precedente)) {
        if (i > 2 && !is.na(espece_data[[as_label(var_valeur)]][i - 2])) {
          # Si deux années sont manquantes, utiliser la racine cubique de l'année n-2
          taux_evol <- c(taux_evol, ((valeur_actuelle / (espece_data[[as_label(var_valeur)]][i - 2])^(1/3)) - 1) * 100)
        } else if (!is.na(valeur_precedente)) {
          # Si une année est manquante, utiliser la racine carrée de la valeur précédente
          taux_evol <- c(taux_evol, ((valeur_actuelle / sqrt(valeur_precedente)) - 1) * 100)
        } else {
          taux_evol <- c(taux_evol, NA)
        }
      } else {
        # Calculer le taux d'évolution normal
        taux_evol <- c(taux_evol, ((valeur_actuelle / valeur_precedente) - 1) * 100)
      }
    }
    
    # Nom de la colonne pour les résultats
    nom_colonne <- paste("Taux_evol", espece, sep = "_")
    
    # Ajouter les taux d'évolution à la liste de résultats
    resultats[[nom_colonne]] <- taux_evol
  }
  
  # Créer un dataframe à partir des résultats
  resultats_df1 <- as.data.frame(resultats)
  
  # Ajouter la colonne d'année
  longueur_resultats <- nrow(resultats_df1)
  resultats_df1$annee <- espece_data[[as_label(var_annee)]][1:longueur_resultats]
  
  # Retourner le dataframe avec les taux d'évolution
  return(resultats_df1)
}






Travail avec pascal 22/03 : 



mon_espece <- "ANG"
mon_indicateur <- "biomasse"
mon_annee_depart <- 1990

df <- reg_indicateur %>% 
  filter(esp_code_alternatif == mon_espece,
         indicateur == mon_indicateur,
         !annee %in% c(1991, 2000, 2001),
         annee >= mon_annee_depart)

test <- df %>% 
  arrange(annee) %>% 
  ungroup() %>% 
  mutate(diff_annee = annee - dplyr::lag(annee, n = 1),
         taux_annuel = (valeur / dplyr::lag(valeur, n = 1)) ^ (1 / diff_annee)) %>% 
  filter(!is.na(taux_annuel)) %>% 
  pull(taux_annuel)


exp(mean(log(test)))















