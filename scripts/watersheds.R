rm(list=ls())

library(leaflet)
library(leaflet.extras)
library(tidycensus)
library(dataRetrieval)
library(sf)

df <- st_read('data/bay_watersheds.shp') %>%
  st_transform(4326)

pal = colorFactor(rainbow(7), df$Name)
# nhd_wms_url <- "https://basemap.nationalmap.gov/arcgis/services/USGSTopo/MapServer/WMSServer?request=GetCapabilities&service=WMS"

GetURL <- function(service, host = "basemap.nationalmap.gov") {
  sprintf("https://%s/arcgis/services/%s/MapServer/WmsServer", host, service)
}

grp <- c("USGS Topo", "USGS Imagery Only", "USGS Imagery Topo",
         "USGS Shaded Relief", "Hydrography")

att <- paste0("<a href='https://www.usgs.gov/'>",
              "U.S. Geological Survey</a> | ",
              "<a href='https://www.usgs.gov/laws/policies_notices.html'>",
              "Policies</a>")

opt <- leaflet::WMSTileOptions(format = "image/png", transparent = TRUE)

m <- leaflet() %>%
  addTiles(group = 'Open Street Map') %>%
  addProviderTiles(providers$Esri.WorldImagery, group = 'ESRI World Imagery') %>%
  addProviderTiles(providers$Stamen.Terrain, group = 'Stamen Terrain') %>%
  addProviderTiles(providers$NASAGIBS.ModisTerraChlorophyll, group = 'MODIS Terra Chlorophyl') %>%
  addWMSTiles(GetURL('USGSTopo'), 
              group = grp[1], attribution = att, layers = "0") %>%
  addWMSTiles(GetURL("USGSHydroCached"),
              group = grp[5], options = opt, layers = "0") %>%
  setView(lng = -77.6, lat = 40, zoom =5.5) %>%
  addSearchOSM() %>%
  addPolygons(data = df,
              popup = paste('River Basin:', df$Name, '<br>',
                            'Area (KM^2):', df$SUM_AreaSq, '<br>'),
              group = 'Bay River Basins',
              fillColor = ~pal(df$Name),
              fillOpacity = 0.5,
              weight = 1) %>%
  addLayersControl(baseGroups = c('Open Street Map', 'ESRI World Imagery', 'Stamen Terrain', 'USGS Topo'),
                   overlayGroups = c('Bay River Basins', 'Hydrography'),
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


  