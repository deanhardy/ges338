rm(list=ls())

library(leaflet)
library(leaflet.extras)
library(tidycensus)
library(dataRetrieval)
library(sf)

df <- st_read('data/bay_riverbasins.shp') %>%
  st_transform(4326)

bay <- st_read('data/bay_watershed.shp') %>%
  st_transform(4326)

pal = colorFactor(rainbow(7), df$Name)
# nhd_wms_url <- "https://basemap.nationalmap.gov/arcgis/services/USGSTopo/MapServer/WMSServer?request=GetCapabilities&service=WMS"

lc_url <- "https://smallscale.nationalmap.gov/arcgis/services/LandCover/MapServer/WMSServer"

GetURL <- function(service, host = "basemap.nationalmap.gov") {
  sprintf("https://%s/arcgis/services/%s/MapServer/WmsServer", host, service)
}

grp <- c("USGS Topo", "USGS Imagery Only", "USGS Imagery Topo",
         "USGS Shaded Relief", "Hydrography", "Landcover")

att <- paste0("<a href='https://www.usgs.gov/'>",
              "U.S. Geological Survey</a> | ",
              "<a href='https://www.usgs.gov/laws/policies_notices.html'>",
              "Policies</a>")

opt <- leaflet::WMSTileOptions(format = "image/png", transparent = TRUE)

m <- leaflet() %>%
  addTiles(group = 'Open Street Map') %>%
  addProviderTiles(providers$Esri.WorldImagery, group = 'ESRI World Imagery') %>%
  addProviderTiles(providers$Esri.WorldPhysical, group = 'ESRI World Physical') %>%
  addProviderTiles(providers$Stamen.Terrain, group = 'Stamen Terrain') %>%
  addProviderTiles(providers$NASAGIBS.ViirsEarthAtNight2012, group = 'NASA Earth at Night') %>%
  addWMSTiles(GetURL('USGSShadedReliefOnly'), 
              group = grp[4], attribution = att, layers = "0") %>%
  addWMSTiles(GetURL("USGSHydroCached"),
              group = grp[5], options = opt, layers = "0") %>%
  addWMSTiles(lc_url, layers = '1', group = grp[6], attribution = att,
              options = WMSTileOptions(format = "image/png", transparent = TRUE)) %>%
  setView(lng = -77.6, lat = 40, zoom =5.5) %>%
  addSearchOSM() %>%
  addPolylines(data = bay,
              popup = paste('Area (KM^2):', round(bay$SUM_AreaSq, 0), '<br>',
                            'Area (MI^2):', round(bay$SUM_AreaSq * 0.386102, 0), '<br>',
                            'States:', 'DC, DE, MD, NY, PA, VA, WV'),
              group = 'Chesapeake Watershed (dark)',
              color = 'black',
              weight = 3) %>%
  addPolylines(data = bay,
               popup = paste('Area (KM^2):', round(bay$SUM_AreaSq, 0), '<br>',
                             'Area (MI^2):', round(bay$SUM_AreaSq * 0.386102, 0), '<br>',
                             'States:', 'DC, DE, MD, NY, PA, VA, WV'),
               group = 'Chesapeake Watershed (light)',
               color = 'yellow',
               weight = 3) %>%
  addPolygons(data = df,
              popup = paste('River Basin:', df$Name, '<br>',
                            'Area (KM^2):', round(df$SUM_AreaSq, 0), '<br>',
                            'Area (MI^2):', round(df$SUM_AreaSq * 0.386102, 0), '<br>',
                            'States:', df$states),
              group = 'Bay River Basins',
              fillColor = ~pal(df$Name),
              fillOpacity = 0.5,
              weight = 1,
              highlightOptions = highlightOptions(color = "black", weight = 2,
                                                  bringToFront = TRUE)) %>%
  addLayersControl(baseGroups = c('Open Street Map', 'ESRI World Imagery', 
                                  'ESRI World Physical', 'USGS Shaded Relief',
                                  'USGS Landcover', 'Stamen Terrain', 
                                  'NASA Earth at Night'),
                   overlayGroups = c('Chesapeake Watershed (dark)', 'Chesapeake Watershed (light)', 
                                     'Bay River Basins', 'Hydrography'),
                   options = layersControlOptions(collapsed = TRUE)) %>%
  # addLegend('bottomright',
  #           pal = pal,
  #           values = df$Name,
  #           title = 'River Basin') %>%
  addScaleBar('bottomright') %>%
  hideGroup(c('Chesapeake Watershed (light)', 'Bay River Basins', 'Hydrography'))
m

## exporting as html file for exploration
library(htmlwidgets)
saveWidget(m, 
           file="C:/r_projects/ges338/docs/bay.html",
           title = "Chesapeake Bay")


  