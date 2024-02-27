
calcul_pourcentage_juveniles <- function(df,
                                         var_id_site,
                                         var_esp_code_alternatif,
                                         var_statut,
                                         var_id_taille)
  
{
  var_id_site <- enquo(var_id_site)
  var_esp_code_alternatif <- enquo (var_esp_code_alternatif)
  var_statut <- enquo(var_statut)
  var_id_taille <- enquo(var_id_taille)
  
  ope_pourcentage_juv_eff <- df %>%
    group_by(!!var_id_site,
             !!var_esp_code_alternatif,
             !!var_statut) %>% 
    summarise(effectif=sum(length(!!var_id_taille))) %>% 
    ungroup() %>% 

    
    
    ope_pourcentage_juv_esp <- df %>%
    group_by(!!var_id_site,
             !!var_esp_code_alternatif) %>% 
    summarise(effectif=sum(length(!!var_id_taille))) %>% 
    summarise(total_effectif = mean(sum(effectif))) %>% 
    filter(!!var_statut == "juvénile") %>% 
    group_by(!!var_id_site,
           !!var_esp_code_alternatif) %>% 
  summarise(total_juveniles = mean(sum(effectif)))
   
  
  
  
   select(!!var_id_site,
           !!var_esp_code_alternatif,
           indicateur,
           valeur,
           !!var_statut)

  

  
  left_join(ope_pourcentage_juv_esp_eff, ope_pourcentagejuv_juv_eff, by = c("var_id_site", "var_esp_code_alternatif")) %>%
  mutate(total_juveniles = coalesce(total_juveniles, 0)) %>% # Remplacer les NA par 0
  mutate(valeur = round((total_juveniles / total_effectif) * 100,2)) %>%
  mutate(statut = "toutes", 
         indicateur ="pourcentage_juveniles") %>%
  select(!!var_id_site, 
         !!var_esp_code_alternatif, 
         indicateur, 
         valeur, 
         !!var_statut)


# Ajout des pourcentages de juvéniles aux différents statuts des espèces (juvéniles / adultes) ----
ope_pourcentage_juv_statut <- ope_pourcentage_juv_eff %>% 
  select(!!var_id_site, 
         !!var_esp_code_alternatif, 
         !!var_statut) %>% 
  mutate(indicateur = "pourcentage_juveniles")

ope_pourcentage_juv_esp_select <- ope_pourcentjuv_esp %>% 
  select(ope_id, esp_code_alternatif, valeur)

ope_pourcentjuv_statut <- left_join(ope_pourcentjuv_statut, ope_pourcentjuv_esp_select, 
                                    by = c("ope_id", "esp_code_alternatif"))

# Construction d'un Df avec les pourcentages de juvénils des espèces par opération toutes tailles confondues + des différents statuts ----
ope_pourcentjuv <- bind_rows(ope_pourcentjuv_statut, ope_pourcentjuv_esp)
}
