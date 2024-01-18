library(tidyverse)

# background : sf of Europe
# remotes::install_github("ropensci/rnaturalearthhires")
background <- rnaturalearth::ne_countries(continent = 'europe',
                                          scale = 10) %>%
  sf::st_as_sf() %>%
  sf::st_make_valid() %>%
  sf::st_crop(
    xmin = -5.4,
    xmax = 8.6,
    ymin = 41,
    ymax = 52
  )


# remotes::install_github("pascalirz/tod")
# hydrographic districts limits
district_limits <- tod::wfs_sandre(url_wfs = "https://services.sandre.eaufrance.fr/geo/topage?",
                                   couche = "BassinHydrographique_FXX")


# build map
nice_map <- ggplot(background) +
  geom_sf() +
  scale_fill_brewer(name = '# surveys',
                    palette = "Greens",
                    na.value = "black") +
  geom_sf(data = district_limits,
          size = 0.8,
          col = "darkred",
          alpha = 0) +
  theme(panel.background = element_rect(fill = "lightblue")) +
  coord_sf(xlim = c(-5.4, 8.4),
           ylim = c(42, 51.2),
           expand = FALSE) 

# display map
nice_map


# map
simple_map <- ggplot(background) +
  geom_sf() +
  scale_fill_brewer(name = '# surveys',
                    palette = "Greens",
                    na.value = "black") +
#  theme(panel.background = element_rect(fill = "lightblue")) +
  coord_sf(xlim = c(-5.4, 8.4),
           ylim = c(42, 51.2),
           expand = FALSE) 

simple_map

save(nice_map,
     simple_map,
     background,
     district_limits,
     file = "processed_data/10_maps.RData")
