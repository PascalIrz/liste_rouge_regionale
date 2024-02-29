## Création d'une fonction de calcul de biomasse à partir des tailles et des 
# poids_estimées par opération. 


calcul_biomasse <- function(df,
                            var_id_site,
                            var_esp_code_alternatif,
                            var_statut,
                            var_longueur,
                            var_type_longueur,
                            var_poids)

{
    var_id_site <- enquo(var_id_site)
    var_esp_code_alternatif <- enquo (var_esp_code_alternatif)
    var_statut <- enquo(var_statut)
    var_longueur <- enquo(var_longueur)
    var_type_longueur <- enquo(var_type_longueur)
    var_poids <- enquo(var_poids)

  # Utilisation des relations taille - poids du package aspe
  # data("taille_poids")
taille_poids <- aspe::data_taille_poids %>%
    group_by(!!var_esp_code_alternatif,
             !!var_type_longueur) %>%
    arrange(source) %>%
    slice(1)
  
  
# nb : vérifier qu'on a bien les tailles pour tous les poissons avant d'appliquer la taille-poids
# combinaisons espèces - type de longueur absentes de la table de conversion
 
   tp_manquantes <- df %>% 
    select(!!var_esp_code_alternatif, 
           !!var_type_longueur) %>% 
    distinct() %>% 
    left_join(y = taille_poids) %>% 
    filter(is.na(a))
  
  # nb : certaines espèces n'ont pas de relation en longueur fourche
  # on approxime par la longueur totale
 
    esp_tp_manquantes <- tp_manquantes %>% 
      pull(!!var_esp_code_alternatif)
  
  
  df <- df %>%
    mutate(!!as_label(var_type_longueur) := ifelse(!!var_esp_code_alternatif %in% esp_tp_manquantes &
                                !!var_type_longueur == "Fourche",
                                "Totale",
                                !!var_type_longueur))
  
  
  df <- df %>% 
    left_join(y = taille_poids,
              by = c("esp_code_alternatif", "tlo_libelle")) %>%
    mutate(poids_tp = a * ((!!var_longueur / 10) ^ b)) # passage en cm de mei_taille => /10
  
#  if(erreurs == TRUE) {
   df <- df %>% 
      mutate(
        erreur_abs = poids_tp - !!var_poids,
        erreur_rel = erreur_abs / poids_tp) %>% 
     mutate(poids = sum(poids_tp, na.rm = TRUE),
              .groups = "drop")
#   }  
  
  return(list(resultat_bio = df))

}  



