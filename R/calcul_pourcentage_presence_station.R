calcul_pourcentage_presence_station <- function(df,
                                                var_esp_code_alternatif,
                                                var_statut,
                                                var_pop_id,
                                                var_annee) 
{
  var_esp_code_alternatif <- enquo(var_esp_code_alternatif)
  var_statut <- enquo(var_statut)
  var_pop_id <- enquo(var_pop_id)
  var_annee <- enquo(var_annee)
  

  resultats <- list()
  
  # Je parcours chaque espèce de poissons
  especes <- unique(df[[as_label(!!var_esp_code_alternatif)]])
  statuts <- unique(df[[as_label(!!var_statut)]])
  

  calcul_pourcentage <- function(donnees_espece_statut, var_pop_id, var_annee) {
    stations_par_annee <- aggregate(!!var_pop_id ~ !!var_annee,
                                    data = donnees_espece_statut,
                                    FUN = function(x) length(unique(x)))
    
    pourcentage_presence <- stations_par_annee[[as_label(var_pop_id)]] / length(unique(donnees_espece_statut[[as_label(var_pop_id)]])) * 100
    return(pourcentage_presence)
  }
  
  # Parcourir les combinaisons d'espèces et de statuts
  for(espece in especes) {
    for(statut in statuts) {
      # Filtrer les données pour l'espèce et le statut en cours
      donnees_espece_statut <- subset(df, !!var_esp_code_alternatif == espece & !!var_statut == statut)
      
      # Calculer le pourcentage de présence
      nom_colonne <- paste("Pourcentage_presence_", espece, "_", statut, sep = "")
      resultats[[nom_colonne]] <- calcul_pourcentage(donnees_espece_statut, var_pop_id, var_annee)
    }
  }
  resultats_df <- as.data.frame(resultats)
  
  # Ajouter une colonne 'annee' au df de résultats
  stations_par_annee <- aggregate(!!var_pop_id ~ !!var_annee, data = df, FUN = function(x) length(unique(x)))
  resultats_df$annee <- stations_par_annee[[as_label(var_annee)]]
  
  return(resultats_df)
}



  