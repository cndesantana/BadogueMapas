library(shinydashboard)
library(leaflet)
library(dplyr)
library(curl) # make the jsonlite suggested dependency explicit
library(readxl)
library(maptools)
library(brmap)
library(maptools)
library(rgdal) # ensure rgdal is loaded
library(readxl)
library(tidyverse)

# 1=South, 2=East, 3=West, 4=North
dirColors <-c("1"="#595490", "2"="#527525", "3"="#A93F35", "4"="#BA48AA")

# Download data from the Twin Cities Metro Transit API
# http://svc.metrotransit.org/NexTrip/help
getMetroData <- function(path) {
  url <- paste0("http://svc.metrotransit.org/NexTrip/", path, "?format=json")
  jsonlite::fromJSON(url)
}

# Load static trip and shape data
trips  <- readRDS("metrotransit-data/rds/trips.rds")
shapes <- readRDS("metrotransit-data/rds/shapes.rds")


# Get the shape for a particular route. This isn't perfect. Each route has a
# large number of different trips, and each trip can have a different shape.
# This function simply returns the most commonly-used shape across all trips for
# a particular route.
get_route_shape <- function(route) {
  routeid <- paste0(route, "-75")

  # For this route, get all the shape_ids listed in trips, and a count of how
  # many times each shape is used. We'll just pick the most commonly-used shape.
  shape_counts <- trips %>%
    filter(route_id == routeid) %>%
    group_by(shape_id) %>%
    summarise(n = n()) %>%
    arrange(-n)

  shapeid <- shape_counts$shape_id[1]

  # Get the coordinates for the shape_id
  shapes %>% filter(shape_id == shapeid)
}


function(input, output, session) {

  # Route select input box
  output$routeSelect <- renderUI({
    live_vehicles <- getMetroData("VehicleLocations/0")

    routeNums <- sort(unique(as.numeric(live_vehicles$Route)))
    # Add names, so that we can add all=0
    names(routeNums) <- routeNums
    routeNums <- c(All = 0, routeNums)
    selectInput("routeNum", "Route", choices = routeNums, selected = routeNums[2])
  })

  # Locations of all active vehicles
  vehicleLocations <- reactive({
    input$refresh # Refresh if button clicked

    # Get interval (minimum 30)
    interval <- max(as.numeric(input$interval), 30)
    # Invalidate this reactive after the interval has passed, so that data is
    # fetched again.
    invalidateLater(interval * 1000, session)

    getMetroData("VehicleLocations/0")
  })

  # Locations of vehicles for a particular route
  routeVehicleLocations <- reactive({
    if (is.null(input$routeNum))
      return()

    locations <- vehicleLocations()

    if (as.numeric(input$routeNum) == 0)
      return(locations)

    locations[locations$Route == input$routeNum, ]
  })

  # Get time that vehicles locations were updated
  lastUpdateTime <- reactive({
    vehicleLocations() # Trigger this reactive when vehicles locations are updated
    Sys.time()
  })

  # Number of seconds since last update
  output$timeSinceLastUpdate <- renderUI({
    # Trigger this every 5 seconds
    invalidateLater(5000, session)
    p(
      class = "text-muted",
      "Data refreshed ",
      round(difftime(Sys.time(), lastUpdateTime(), units="secs")),
      " seconds ago."
    )
  })

  output$numVehiclesTable <- renderUI({
    locations <- routeVehicleLocations()
    if (length(locations) == 0 || nrow(locations) == 0)
      return(NULL)

    # Create a Bootstrap-styled table
    tags$table(class = "table",
      tags$thead(tags$tr(
        tags$th("Color"),
        tags$th("Direction"),
        tags$th("Number of vehicles")
      )),
      tags$tbody(
        tags$tr(
          tags$td(span(style = sprintf(
            "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
            dirColors[4]
          ))),
          tags$td("Northbound"),
          tags$td(nrow(locations[locations$Direction == "4",]))
        ),
        tags$tr(
          tags$td(span(style = sprintf(
            "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
            dirColors[1]
          ))),
          tags$td("Southbound"),
          tags$td(nrow(locations[locations$Direction == "1",]))
        ),
        tags$tr(
          tags$td(span(style = sprintf(
            "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
            dirColors[2]
          ))),
          tags$td("Eastbound"),
          tags$td(nrow(locations[locations$Direction == "2",]))
        ),
        tags$tr(
          tags$td(span(style = sprintf(
            "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
            dirColors[3]
          ))),
          tags$td("Westbound"),
          tags$td(nrow(locations[locations$Direction == "3",]))
        ),
        tags$tr(class = "active",
          tags$td(),
          tags$td("Total"),
          tags$td(nrow(locations))
        )
      )
    )
  })

  # Store last zoom button value so we can detect when it's clicked
  lastZoomButtonValue <- NULL

  output$busmap <- renderLeaflet({
     locations <- routeVehicleLocations()
     if (length(locations) == 0)
        return(NULL)
     
     # Show only selected directions
     locations <- filter(locations, Direction %in% as.numeric(input$directions))
     
     # Four possible directions for bus routes
     dirPal <- colorFactor(dirColors, names(dirColors))
     
     map <- leaflet(locations) %>%
        addTiles('http://{s}.tile.thunderforest.com/transport/{z}/{x}/{y}.png') %>%
        addCircleMarkers(
           ~VehicleLongitude,
           ~VehicleLatitude,
           color = ~dirPal(Direction),
           opacity = 0.8,
           radius = 8
        )
     
     if (as.numeric(input$routeNum) != 0) {
        route_shape <- get_route_shape(input$routeNum)
        
        map <- addPolylines(map,
                            route_shape$shape_pt_lon,
                            route_shape$shape_pt_lat,
                            fill = FALSE
        )
     }
     
     rezoom <- "first"
     # If zoom button was clicked this time, and store the value, and rezoom
     if (!identical(lastZoomButtonValue, input$zoomButton)) {
        lastZoomButtonValue <<- input$zoomButton
        rezoom <- "always"
     }
     
     map <- map %>% mapOptions(zoomToLimits = rezoom)
     
     map
  })
  
  output$busmap2 <- renderLeaflet({
     rootdir <- "/home/cdesantana/DataSCOUT/Objectiva/NucleoPolitico/Twitter/Mapas/mapaIS/"
    dados_cidades_populacao_regiao <- read_xlsx(paste(rootdir,"Cidades_Por_Regiao_Populacao.xlsx",sep=""))
    names(dados_cidades_populacao_regiao) <- c("municipio","pop","reg")
    new_brmap_municipio_pop_reg <- brmap_municipio %>% 
       filter(estado == "Bahia") %>% 
       left_join(as_tibble(dados_cidades_populacao_regiao))
    corpositivo <- "#20B2AA";
    cornegativo <- "#c00000";
    corneutro <- "#FFA500";
    filepath <- input$file$datapath
    if(nchar(filepath) > 0){
       dfGruposDataGrpNumWeek <- read_xlsx(filepath)
       names(dfGruposDataGrpNumWeek) <- c("reg","is")
       mymap <- left_join(new_brmap_municipio_pop_reg, dfGruposDataGrpNumWeek, by = 'reg')

       if(input$tipolegenda == "positivasnegativas"){
          colfunc <- colorRampPalette(c(corpositivo, corneutro, cornegativo))
          palette_is <- rev(colfunc(201))
          posColorIS <- round((((mymap$is)+1)*100)) + 1
          legenda_colors <- palette_is[c(1,26,51,76,101,126,151,176,201)]
          legenda_labels <- c(-1, -0.75, -0.5, -0.25, 0,0.25,0.5,0.75, 1)
          legenda_titulo = paste("Índice de Sentimento - ",input$mes)
       }
       else if (input$tipolegenda == "corespos"){
          colfunc <- colorRampPalette(c(corpositivo, corneutro))
          palette_is <- c(rev(colfunc(201)),"#AAAAAA")
          posColorIS <- round(mymap$is/max(mymap$is, na.rm=TRUE)*200) + 1
          posColorIS[is.na(posColorIS)] <- 202
          legenda_colors <- palette_is[c(1,26,51,76,101,126,151,176,201,202)]
          legenda_labels <- c(as.character(round(quantile(seq(0,max(mymap$is),by=1),probs = seq(0,1,by=0.125)))), NA)
          legenda_titulo = paste("Número de Comentários Positivos - ",input$mes)
          
       }
       else if (input$tipolegenda == "coresneg"){
          colfunc <- colorRampPalette(c(cornegativo,corneutro))
          palette_is <- c(rev(colfunc(201)),"#AAAAAA")
          posColorIS <- round(mymap$is/max(mymap$is, na.rm=TRUE)*200) + 1
          posColorIS[is.na(posColorIS)] <- 202
          legenda_colors <- palette_is[c(1,26,51,76,101,126,151,176,201,202)]
          legenda_labels <- c(as.character(round(quantile(seq(0,max(mymap$is),by=1),probs = seq(0,1,by=0.125)))), NA)
          legenda_titulo = paste("Número de Comentários Negativos - ",input$mes)
          
       }
       else if (input$tipolegenda == "coresneu"){
          colfunc <- colorRampPalette(c("#FFB732",corneutro,"#CC8400"))
          palette_is <- c(rev(colfunc(201)),"#AAAAAA")
          posColorIS <- round(mymap$is/max(mymap$is, na.rm=TRUE)*200) + 1
          posColorIS[is.na(posColorIS)] <- 202
          legenda_colors <- palette_is[c(1,26,51,76,101,126,151,176,201,202)]
          legenda_labels <- c(as.character(round(quantile(seq(0,max(mymap$is),by=1),probs = seq(0,1,by=0.125)))), NA)
          legenda_titulo = paste("Número de Comentários Neutros - ",input$mes)
          
       }
       else if (input$tipolegenda == "aumentadiminui"){
          palette_is <- c("red","blue","gray")
          posColorIS <- if_else(mymap$is == 0, 3, if_else(mymap$is > 0, 2, 1))
          legenda_colors = palette_is[c(1,3,2)]
          legenda_labels = c("Variação Negativa","Estável","Variação Positiva")
          legenda_titulo = paste("Variação com o mês anterior - ",input$mes)
       }
   
       map <- leaflet() %>%
          addPolygons(data = mymap,
                      color = palette_is[posColorIS],
                      fillColor = palette_is[posColorIS], 
                      opacity = 1.0, fillOpacity = 1, 
                      popup = paste(mymap$municipio," / Região ",mymap$reg, " / IS = ",mymap$is,sep="")) %>%
          addLegend(position="bottomright", colors=legenda_colors, labels=legenda_labels, opacity = 1.0, title=legenda_titulo)
    }
    else{
       mymap <- new_brmap_municipio_pop_reg 
       map <- leaflet() %>%
          addPolygons(data = mymap,
                      color = "black",
                      fillColor = "white", 
                      popup = paste(mymap$municipio,sep=""))
    }

    rezoom <- "first"
    # If zoom button was clicked this time, and store the value, and rezoom
    if (!identical(lastZoomButtonValue, input$zoomButton)) {
      lastZoomButtonValue <<- input$zoomButton
      rezoom <- "always"
    }

    map <- map %>% mapOptions(zoomToLimits = rezoom)

    map
  })
}
