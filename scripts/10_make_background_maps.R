library(tidyverse)
xmin = -5.4
xmax = -0.8
ymin = 47.4
ymax = 49


# background : sf of Europe
# remotes::install_github("ropensci/rnaturalearthhires")
background <- rnaturalearth::ne_countries(continent = 'europe',
                                          scale = 10) %>%
  sf::st_as_sf() %>%
  sf::st_make_valid() %>%
  sf::st_crop(
    xmin = xmin,
    xmax = xmax,
    ymin = ymin,
    ymax = ymax
  )


# remotes::install_github("MaelTheuliere/COGiter")
depts <- COGiter::departements_metro_geo %>% 
  dplyr::filter(DEP %in% c("22", "29", "35", "56"))

# build map
nice_map <- ggplot(background) +
  geom_sf(linesize = 0, alpha = 1) +
  scale_fill_brewer(name = '# surveys',
                    palette = "Greens",
                    na.value = "black") +
  # geom_sf(data = depts,
  #         size = 0.8,
  #         col = "darkred",
  #         alpha = 0) +
  theme(panel.background = element_rect(fill = "lightblue")) +
  coord_sf(xlim = c(xmin, xmax),
           ylim = c(ymin, ymax),
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
  coord_sf(xlim = c(xmin, xmax),
           ylim = c(ymin, ymax),
           expand = FALSE) 

simple_map

save(nice_map,
     simple_map,
     background,
     district_limits,
     file = "../processed_data/10_maps.RData")
