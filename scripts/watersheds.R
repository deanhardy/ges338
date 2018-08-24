rm(list=ls())

library(tidycensus)
library(leaflet)
library(leaflet.extras)
library(sf)

df <- st_read('data/bay_watersheds.shp') %>%
  st_transform(4326)

pal = colorFactor(rainbow(7), df$Name)

m <- leaflet() %>%
  addTiles(group = 'Open Street Map') %>%
  addProviderTiles(providers$Esri.WorldImagery, group = 'ESRI World Imagery') %>%
  addProviderTiles(providers$Esri.WorldTerrain, group = 'ESRI World Terrain') %>%
  setView(lng = -77.6, lat = 40, zoom = 6) %>%
  addSearchOSM() %>%
  addPolygons(data = df,
              popup = paste('River Basin:', df$Name, '<br>',
                            'Area (KM^2):', df$SUM_AreaSq, '<br>'),
              group = 'Bay River Basins',
              fillColor = ~pal(df$Name),
              fillOpacity = 0.5,
              weight = 1) %>%
  addLayersControl(baseGroups = c('Open Street Map', 'ESRI World Imagery', 'ESRI World Terrain'),
                   overlayGroups = c('Bay River Basins'),
                   options = layersControlOptions(collapsed = TRUE)) %>%
  addLegend('bottomright',
            pal = pal,
            values = df$Name,
            title = 'River Basin') %>%
  addScaleBar('bottomright')
m

## exporting as html file for exploration
library(htmlwidgets)
saveWidget(m, 
           file="C:/r_projects/ges338/docs/bayriverbasins.html",
           title = "Chesapeake Bay River Basins")


  