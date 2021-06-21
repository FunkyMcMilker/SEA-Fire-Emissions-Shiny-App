library(ggthemes)
library(tidyverse)
library(leaflet)
library(RColorBrewer)
library(xts)
library(rgdal)


#helper function for choropleth animation
setShapeStyle <- function( map, data = getMapData(map), layerId,
                           stroke = NULL, color = NULL,
                           weight = NULL, opacity = NULL,
                           fill = NULL, fillColor = NULL,
                           fillOpacity = NULL, dashArray = NULL,
                           smoothFactor = NULL, noClip = NULL, label = NULL,
                           options = NULL){
  
  options <- c(list(layerId = layerId),
               options,
               filterNULL(list(stroke = stroke, color = color,
                               weight = weight, opacity = opacity,
                               fill = fill, fillColor = fillColor,
                               fillOpacity = fillOpacity, dashArray = dashArray,
                               smoothFactor = smoothFactor, noClip = noClip, label = label
               )))
  
  options <- evalFormula(options, data = data)
  options <- do.call(data.frame, c(options, list(stringsAsFactors=FALSE)))
  
  layerId <- options[[1]]
  style <- options[-1]
  if("label" %in% colnames(style)){
    labelData = style[,"label", FALSE]
    style = style[,-which(colnames(style)=="label"), FALSE]
    leaflet::invokeMethod(map, data, "setLabel", "shape", layerId, label)
  }
  leaflet::invokeMethod(map, data, "setStyle", "shape", layerId, style);
}

leafletjs <-  tags$head(
  tags$script(HTML('
  
window.LeafletWidget.methods.setStyle = function(category, layerId, style){
  var map = this;
  if (!layerId){
    return;
  } else if (!(typeof(layerId) === "object" && layerId.length)){
    layerId = [layerId];
  }
  style = HTMLWidgets.dataframeToD3(style);
  layerId.forEach(function(d,i){
    var layer = map.layerManager.getLayer(category, d);
    if (layer){
      layer.setStyle(style[i]);
    }
  });
};
window.LeafletWidget.methods.setLabel = function(category, layerId, label){
  var map = this;
  if (!layerId){
    return;
  } else if (!(typeof(layerId) === "object" && layerId.length)){
    layerId = [layerId];
  }
  layerId.forEach(function(d,i){
    var layer = map.layerManager.getLayer(category, d);
    if (layer){
      layer.unbindTooltip();
      layer.bindTooltip(label[i])
    }
  });
};
'
  ))
)


#load data

FRP_DATA <- read.csv("data/April_FRP_sumations_by_nation.csv")
PM25_DATA <- read.csv("data/April_PM25_sumation_by_nation.csv")
CO2_DATA <- read.csv("data/April_CO2_Sumations.csv")

CO2_DATA$X<-1:nrow(CO2_DATA)


names(FRP_DATA)[3] = "FRP"

totals <- merge(FRP_DATA, PM25_DATA, by = "X")

totals <- merge(totals, CO2_DATA, by = "X")

totals$Day.y <- NULL

totals$Day.x <- NULL

totals$aISO3.y <- NULL

totals$aISO3.x <- NULL

#choropleth loading spatial data

world_spdf <- readOGR( 
  dsn =  "data/" , 
  layer ="TM_WORLD_BORDERS_SIMPL-0.3",
  verbose=FALSE
)

one_day_total_pm <- PM25_DATA[PM25_DATA$Day == 1, ]

world_spdf$PM25 <- one_day_total_pm$PM25[match(world_spdf$ISO3, one_day_total_pm$aISO3)]

world_spdf@data$LabelText <- paste0(
  "<b>Country :</b> ", world_spdf@data$NAME,"<br>", 
  "<b>PM 2.5 :</b> ", format(world_spdf@data$PM25, nsmall=0, big.mark=","))


paletteBins <- c(1e-09, 1e-08, 5e-08, 1e-07, 5e-07, 1e-06, 5e-06, 1e-05, 5e-05  )
colorPalette <- colorBin(palette = "YlOrBr", domain = one_day_total_pm$PM25, na.color = "transparent", bins = paletteBins)


#cluster map loading spatial matrix

April_spatial_data <- load("data/April_Spatial.RData")

color_pal_pm2p5 <- colorFactor("Reds", domain = c(0, 1e-12, 1e-11, 1e-10, 1e-09, 1e-08, 1e-07, 1e-06, 1  ) )
popup_pm2p5 <- paste0("<b>","PM2.5 Level : ","</b>",
                      "<b>",spatial_data_over_month[[1,1]]$PM25,"</b><br>",
                      "<b>At : ",spatial_data_over_month[[1,1]]$LAT, " x ", spatial_data_over_month[[1,1]]$LON,"</b>")

color_pal_co2fire <- colorFactor("Blue", domain = c(0, 5e-11, 1e-10, 1e-09, 1e-08, 1e-07, 1e-06, 1e-05, 1e-04, 1  ))
popup_co2fire <- paste0("<b>","CO2 Level : ","</b>",
                        "<b>",spatial_data_over_month[[1,3]]$CO2,"</b><br>",
                        "<b>At : ",spatial_data_over_month[[1,3]]$LAT, " x ", spatial_data_over_month[[1,3]]$LON,"</b>")

color_pal_frpfire <- colorFactor("Green", c(0, 5, 10, 15, 20, 25, 30, 40, 45, 50))
popup_frpfire <- paste0("<b>","FRP Level : ","</b>",
                        "<b>",spatial_data_over_month[[1,2]]$FRP,"</b><br>",
                        "<b>At : ",spatial_data_over_month[[1,2]]$LAT, " x ", spatial_data_over_month[[1,2]]$LON,"</b>")


function(input, output, session){
  
  
  #update input parameters for line plt display
  updateSelectInput(session, 
                    "select_parameters_with_updateSelectInput",
                    choices = c("PM25", "CO2", "FRP"))
  
  #update input for corelation pramerters 
  updateSelectInput(session, 
                    "select_parameters_with_updateSelectInput_corelation_x",
                    choices = c("PM25", "CO2", "FRP"))
  
  updateSelectInput(session, 
                    "select_parameters_with_updateSelectInput_corelation_y",
                    choices = c("PM25", "CO2", "FRP"),
                    selected = "FRP")
  
  #update country fo line plot display
  
  checkbox_select_country <- reactive({ 
    dplyr::select(filter(totals, aISO3 == input$checkbox_select_country ), c(X,Day,FRP,aISO3, PM25, CO2))
  })
  
  updateCheckboxGroupInput(session,
                           "checkbox_select_country",
                           choices = (c("THA", "MMR", "VNM","KHM","LAO")),
                           selected = list("THA", "MMR","KHM","LAO"))
  
  #brushing line plot for values 
  selected_points <- reactiveVal()
  
  observeEvent(input$PM25_line_brush, {
    selected_points( brushedPoints(totals, input$PM25_line_brush))
  })
  
  observeEvent(selected_points(), ignoreInit=T, ignoreNULL= T, {
    if(nrow(selected_points()) > 0){
      showModal(modalDialog(
      title = "Selecting data points :",
      paste0(paste0(rownames(selected_points()), collapse = ", ")),
      easyClose = TRUE
      ))
    }
  })
  
  output$brush_info <- renderPrint({
    selected_points()
  })
  
  output$click_info <- renderPrint({
    nearPoints(totals, input$PM25_line_click, addDist = TRUE)
  })
    
    output$PM25_line_checkbox <- renderPlot({
      checkbox_select_country() %>% 
        ggplot(aes(x = Day, y=PM25, color = aISO3)) +
        geom_line(size = 1, alpha = 0.8) +
        labs(title = "PM 2.5 by Nation over April, 2021",
             subtitle = "Where is PM 2.5 occuring over time?",
             x = "Days on April, 2021",
             y = "PM 2.5 Concentration") +
        #this removes axis titles by default
        theme_fivethirtyeight() +   
        theme(axis.title = element_text())
       })
    
    output$FRP_line_checkbox <- renderPlot({
      checkbox_select_country() %>% 
        ggplot(aes(x = Day, y=FRP, color = aISO3)) +
        geom_line(size = 1, alpha = 0.8) +
        labs(title = "FRP by Nation over April, 2021",
             subtitle = "Where is FRP occuring over time?",
             x = "Days on April, 2021",
             y = "Total FRP") +
        #this removes axis titles by default
        theme_fivethirtyeight() +   
        theme(axis.title = element_text())
    })
    
    output$CO2_line_checkbox <- renderPlot({
      checkbox_select_country() %>% 
        ggplot(aes(x = Day, y=CO2, color = aISO3)) +
        geom_line(size = 1, alpha = 0.8) +
        labs(title = "CO2 by Nation over April, 2021",
             subtitle = "Where is CO2 occuring over time?",
             x = "Days on April, 2021",
             y = "Total CO2") +
        #this removes axis titles by default
        theme_fivethirtyeight() +   
        theme(axis.title = element_text())
    })
      
    #corelations plot
      
      output$corelations_PM25_FRP <- renderPlot({
        totals %>%
          ggplot(aes(x = PM25, y = FRP, frame = Day)) +
          geom_point() +
          geom_smooth( aes( group = Day),
                       method = "lm",
                       show.legend = FALSE ) +
          facet_wrap(~aISO3, scales = "free") +
          labs(title = "Corelation Between FRP and PM 2.5",
               subtitle = "Interaction between FRP and PM 2.5",
               x = " Total FRP Output per Day",
               y = "Total PM 2.5 per Day") +
          theme_fivethirtyeight() +   
          theme(axis.title = element_text())
      })
      
      #choropleth mapping and reactive 
      
      observe({
        
        eligibleDates <- unique(totals$Day)
        
        if(input$frequency == "weeks"){
          stepSize = 7
        }else{
          stepSize = 1
        }
        
        output$dateUI <- renderUI({
          sliderInput("dateSel", "Date",
                      min = min(eligibleDates),
                      max = max(eligibleDates),
                      value = min(eligibleDates),
                      step = stepSize,
                      #timeFormat = "%d %b %y",
                      animate = animationOptions(interval = 500, loop = FALSE)
          )
        })
      })
        
        
    #filter data depending on selected date
      
        filteredData <- reactive({
          req(input$dateSel)
          totals[totals$Day == input$dateSel, ]
        })
      
      output$map <- renderLeaflet({
        leaflet(world_spdf) %>% 
          addTiles()  %>% 
          setView(lat = 10, lng = 100, zoom=3) %>%
          addPolygons( 
            layerId = ~ISO3,
            fillColor = ~colorPalette(PM25),
            stroke = TRUE, 
            fillOpacity = 1, 
            color = "white", 
            weight = 1,
            label = ~lapply(LabelText, htmltools::HTML)) %>%
          
          leaflet::addLegend(pal = colorPalette, values = ( (totals$PM25 * 100000000 ) ), 
                    opacity=0.7, title = "PM 2.5", position = "bottomleft")
      })
      
      #prepare data depending on selected date and draw either markers or update polygons depending on the selected map type
      observe({
        
        one_day_total_pm <- PM25_DATA[PM25_DATA$Day == input$dateSel, ]
        
        world_spdf$PM25 <- one_day_total_pm$PM25[match(world_spdf$ISO3, one_day_total_pm$aISO3)]
        
        #world_spdf$PM25 <- filteredData()$PM25[match(world_spdf$ISO3, filteredData()$Country_code)]
        
        world_spdf@data$LabelText <- paste0(
          "<b>Country:</b> ", world_spdf@data$NAME,"<br>", 
          "<b>PM25:</b> ", format(world_spdf@data$PM25, nsmall=0, big.mark=","))
        
        if(input$mapType == "Markers"){
          
          leafletProxy("map", data = world_spdf) %>%
            clearMarkers() %>%
            setShapeStyle(layerId = ~ISO3, fillColor = "lightgray") %>%
            addCircleMarkers(lng = ~LON,
                             lat = ~LAT,
                             radius = ~PM25 * 15000000,
                             weight = 1,
                             opacity = 1,
                             color = ~ifelse(PM25 > 0, "black", "transparent"),
                             fillColor = ~ifelse(PM25 > 0, colorPalette(PM25), "transparent"),
                             fillOpacity = 0.8,
                             label = ~lapply(LabelText, htmltools::HTML))
          
        }else if(input$mapType == "Choropleth"){
          
          leafletProxy("map", data = world_spdf) %>%
            clearMarkers() %>%
            setShapeStyle(layerId = ~ISO3, fillColor = ~ifelse(PM25 > 0, colorPalette(PM25), "lightgray"), label = world_spdf$LabelText)
          
        }
      })
     
  #cluster mapping and reactive animation 
      
      observe({
        
        eligibleDates <- c(1,30)
        
        if(input$frequency_cluster == "weeks"){
          stepSize = 7
        }else{
          stepSize = 1
        }
        
        output$dateUI_Cluster <- renderUI({
          sliderInput("dateSel_Cluster", "Date",
                      min = min(eligibleDates),
                      max = max(eligibleDates),
                      value = min(eligibleDates),
                      step = stepSize,
                      #timeFormat = "%d %b %y",
                      animate = animationOptions(interval = 1500, loop = FALSE)
          )
        })
      })
      
      filteredData_Cluster <- reactive({
        req(input$dateSel_Cluster)
        spatial_data_over_month[input$dateSel_Cluster, ]
      })
      
      output$map2 <- renderLeaflet({
        leaflet::leaflet() %>% addTiles() %>%
          #leaflet::addRasterImage(pm2p5_LAO) %>%
          leaflet::addCircleMarkers(lat = ~LAT, lng = ~LON, 
                                    clusterOptions = markerClusterOptions(),
                                    color = color_pal_co2fire,
                                    popup = ~popup_co2fire,
                                    group = "CO2",
                                    data = spatial_data_over_month[[1,3]] ) %>%
          leaflet::addCircleMarkers(lat = ~LAT, lng =  ~LON, 
                                    clusterOptions = markerClusterOptions(),
                                    color = color_pal_pm2p5,
                                    popup = ~popup_pm2p5,
                                    group = "PM2.5",
                                    data = spatial_data_over_month[[1,1]] ) %>%
          leaflet::addCircleMarkers(lat = ~LAT, lng =  ~LON, 
                                    clusterOptions = markerClusterOptions(),
                                    color = color_pal_frpfire,
                                    popup = ~popup_frpfire,
                                    group = "FRP",
                                    data = spatial_data_over_month[[1,2]] ) %>%
          leaflet::addLayersControl(
            overlayGroups = c("PM2.5", "CO2", "FRP"),  # add these layers
            options = layersControlOptions(collapsed = FALSE)  # expand on hover?
          ) %>% 
          hideGroup(c("CO2", "FRP"))
      })
      
      observe({
        #world_spdf$PM25 <- filteredData()$PM25[match(world_spdf$ISO3, filteredData()$Country_code)]

        spatial_data_over_month <- filteredData_Cluster()
        
        leafletProxy("map2") %>% 
          clearMarkerClusters() %>%
          clearMarkers() %>%
          addTiles() %>%
          #leaflet::addRasterImage(pm2p5_LAO) %>%
          leaflet::addCircleMarkers(lat = spatial_data_over_month[[3]]$LAT, lng = spatial_data_over_month[[3]]$LON, 
                                    clusterOptions = markerClusterOptions(),
                                    color = color_pal_co2fire, 
                                    popup = ~popup_co2fire,
                                    group = "CO2",
                                    data = spatial_data_over_month[[3]] ) %>%
          leaflet::addCircleMarkers(lat = spatial_data_over_month[[1]]$LAT, lng =  spatial_data_over_month[[1]]$LON, 
                                    clusterOptions = markerClusterOptions(),
                                    color = color_pal_pm2p5,
                                    popup = ~popup_pm2p5,
                                    group = "PM2.5",
                                    data = spatial_data_over_month[[1]] ) %>%
          leaflet::addCircleMarkers(lat = spatial_data_over_month[[2]]$LAT, lng =  spatial_data_over_month[[2]]$LON, 
                                    clusterOptions = markerClusterOptions(),
                                    color = color_pal_frpfire,
                                    popup = ~popup_frpfire,
                                    group = "FRP",
                                    data = spatial_data_over_month[[2]] ) %>%
          leaflet::addLayersControl(
            overlayGroups = c("PM2.5", "CO2", "FRP"),  # add these layers
            options = layersControlOptions(collapsed = FALSE)  # expand on hover?
          ) 
        #%>% 
          #hideGroup(c("CO2", "FRP"))
      })
      
      #ploting line graph based on parameter input
      
     observe({
       if(input$select_parameters_with_updateSelectInput == "PM25"){
         output$line_checkbox_plot <- renderPlot({
           checkbox_select_country() %>% 
             ggplot(aes(x = Day, y=PM25, color = aISO3)) +
             geom_line(size = 1, alpha = 0.8) +
             labs(title = "PM 2.5 by Nation over April, 2021",
                  subtitle = "Where is PM 2.5 occuring over time?",
                  x = "Days on April, 2021",
                  y = "PM 2.5 Concentration") +
             #this removes axis titles by default
             theme_fivethirtyeight() +   
             theme(axis.title = element_text())
         })
       }
       if(input$select_parameters_with_updateSelectInput == "FRP"){
         output$line_checkbox_plot <- renderPlot({
           checkbox_select_country() %>% 
             ggplot(aes(x = Day, y=FRP, color = aISO3)) +
             geom_line(size = 1, alpha = 0.8) +
             labs(title = "FRP by Nation over April, 2021",
                  subtitle = "Where is FRP occuring over time?",
                  x = "Days on April, 2021",
                  y = "Total FRP") +
             #this removes axis titles by default
             theme_fivethirtyeight() +   
             theme(axis.title = element_text())
         })
         
       }
       if(input$select_parameters_with_updateSelectInput == "CO2"){
         output$line_checkbox_plot <- renderPlot({
           checkbox_select_country() %>% 
             ggplot(aes(x = Day, y=CO2, color = aISO3)) +
             geom_line(size = 1, alpha = 0.8) +
             labs(title = "CO2 by Nation over April, 2021",
                  subtitle = "Where is CO2 occuring over time?",
                  x = "Days on April, 2021",
                  y = "Total CO2") +
             #this removes axis titles by default
             theme_fivethirtyeight() +   
             theme(axis.title = element_text())
         })
       }
     })
     
     #ploting corelation based on input parameters 
     observe({
       
       filter_co_data <- reactive({
           dplyr::select(filter(totals), c(X,Day,aISO3, input$select_parameters_with_updateSelectInput_corelation_x, input$select_parameters_with_updateSelectInput_corelation_y))
        
       })
       
       output$corelations_PM25_FRP <- renderPlot ({
         filter_co_data() %>%
           ggplot(aes(x = filter_co_data()[ , 4], y = filter_co_data()[ , 5] , frame = Day)) +
           geom_point() +
           geom_smooth( aes( group = Day),
                        method = "lm",
                        show.legend = FALSE ) +
           facet_wrap(~aISO3, scales = "free") +
           labs(title = "Corelation Between Selected Parameters",
                subtitle = "What Parameters are Dependent?",
                x = input$select_parameters_with_updateSelectInput_corelation_x,
                y = input$select_parameters_with_updateSelectInput_corelation_y ) +
           theme_fivethirtyeight() +   
           theme(axis.title = element_text())
       })
     })
    
}