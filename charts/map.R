library(leaflet)
library(sf)
library(tidyverse)
library(leaflet.extras)
library(htmlwidgets)
library(htmltools)


##read data
clean_dir = "1.cleandata"
charts_dir = "charts"

to_map = read_rds(file.path(clean_dir,"llamadas_shape.rds"))


# setwd("1.cleandata")
# saveWidget(map, "mapa_llamadas.html")



to_map= st_simplify(to_map)
to_map= st_simplify(to_map)
to_map= st_simplify(to_map)

##start_mapping 
bins <- c(1, 10, 15, 20, 30, Inf)
paleton = c('#efbbff', "#d896ff", "#be29ec", "#800080", '#660066')

pal <- colorBin(paleton, to_map$total,
                na.color = "#00000000",
                bins = bins,
                alpha = T)




# pal = colorNumeric("YlOrRd", to_map$total, na.color = "#808080", alpha = FALSE,
#              reverse = FALSE)


##labels
labels <- paste0(
  "<center><strong> <font size=4> CP:",to_map$d_cp,"</font></strong></center><br/>",
  '<strong> <font size=3> Llamadas: </strong>',to_map$total,"</font> <br/>"
  
  
  
)%>% 
  lapply(htmltools::HTML)


##Map
mapa = leaflet(to_map) %>%
  addProviderTiles("CartoDB.Positron", group = "Basemap") %>%
  addPolygons(data = to_map,
              fillColor = ~pal(total),
              color = "purple",
              weight = 1,
              fillOpacity = .4,
              popup = labels,
              label = labels,
              
              highlight = highlightOptions(
                weight = 2,
                color ="black"
              ),
              layerId =  ~ d_cp,
              group = "codigos"
              
  ) %>%
  
  addLegend(            colors =  paleton,
            values = ~total,
            labels = c("1 a 10","","","","+ de 100"),
            opacity = 0.4, title = NULL,
            position = "bottomright"
            )  %>%
  
  addResetMapButton() %>%
  
  addSearchFeatures(
    targetGroups = 'codigos',
    options = searchFeaturesOptions(
      zoom=14, 
      openPopup =  TRUE, 
      firstTipSubmit = TRUE,
      autoCollapse = TRUE, 
      hideMarkerOnCollapse = TRUE,
      textPlaceholder="Buscar",
      autoType=T,
      textErr="La ubicación no existe")) %>%
  
 
  
  addControl("<P><B>Busca por código postal</B></P>",
             position='bottomleft')




#setwd(charts_dir)

#saveWidget(mapa, "mapa_llamadas.html")
#write_rds(mapa, file.path(clean_dir,"mapa_llamadas.rds"))

