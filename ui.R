library(leaflet)
library(xts)
library(rgdal)
library(shinythemes)


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

#helper function in JS for choropleth animation
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




    fluidPage( theme = shinytheme("yeti"),
      includeCSS("www/styles.css"),
      leafletjs,
      fluidRow( 
        h1("Fire Emisions in Upper South East Asia"),
        h2("April, 2021"),
        h3("An interactive data visualization.")
      ),
      
      #Info about data
      fluidRow( class = "other-row", titlePanel("What are we measuring?"),
                column(4, 
                       h3("Fire Related Power"), 
                       p("Fire Radiative Power is the rate of emitted radiative energy by the fire at the time of the observation. The fire intensity can vary mainly in relation to the moisture content of the combustible material, the wind, air temperature, humidity and slope. Fire Radiative Power is expressed in units of power,such as Watts (W) and the unit of measurement for FRP is kg m-2.")
                ),
                column(4,
                       h3("PM 2.5"),
                       h4("What is PM 2.5 and how does it get into air?"),
                       p("PM stands for particulate matter the term for a mixture of solid particles and liquid droplets found in the air. Some particles, such as dust, dirt, soot, or smoke, are large or dark enough to be seen with the naked eye. Others are so small they can only be detected using an electron microscope. Particle pollution includes:
1) PM10 : inhalable particles, with diameters that are generally 10 micrometers and smaller
2) PM2.5 : fine inhalable particles, with diameters that are generally 2.5 micrometers and smaller."),
                       h4("How small is 2.5 micrometers?"),
                       p("Think about a single hair from your head. The average human hair is about 70 micrometers in diameter –making it 30 times larger than the largest fine particle.")
                ),
                column(4,
                       h3("Carbon Dioxide"),
                       h4("What is Carbon Dioxide?"),
                       p("Carbon Dioxide or CO2 is a greenhouse gas that is natural and harmless in small quantities, but as levels rise it can affect productivity and sleep. Most commonly produced indoors by the air we exhale, CO2 levels concentrate indoors with less ventilation. Forest fire emissions are a significant source of carbon dioxide (CO2), not only affecting its interannual variability but also biogeochemical cycles with consequences for climate.")
                )
      ),
      
      fluidRow(
        column(width = 4,
               titlePanel("Tracking Fire Emisions Clusters"),
               h3("Days of April"),
               radioButtons(inputId = "frequency_cluster",
                            label = "Select Data Frequency",
                            choices = c("days", "weeks"),
                            selected = "weeks",
                            inline = TRUE),
               uiOutput("dateUI_Cluster"),
               p("*Note : click on cluster indicators to zoom in and view exact locations of recorded fire emisions.")
        ),
        column(width = 8,
               leafletOutput("map2", width = "100%", height = "500px")
        )
      ),
      
      fluidRow( class = "other-row",
                column(8,
                       plotOutput("line_checkbox_plot", width = "100%", 
                                  click = "PM25_line_click",
                                  brush = brushOpts(
                                    id = "PM25_line_brush")
                       )
                ),
                
                column(4,
                       titlePanel("Compairing Values by Nation"),
                       selectInput("select_parameters_with_updateSelectInput", "Choose a Parameter:",
                                   choices = list("PM25", "CO2", "FRP") 
                       ),
                       checkboxGroupInput("checkbox_select_country", "Choose Country(s):",
                                          choiceNames = list(icon("THA"), icon("MMR"), icon("LAO"), 
                                                             icon("KHM"), icon("VNM")),
                                          choiceValues = list("Thailand", "Myanmar", "Laos", "Cambodia", "Vietnam"),
                                          selected = list ("Thailand", "Myanmar", "Laos", "Cambodia", "Vietnam")
                                          
                       ),
                       
                       verbatimTextOutput("brush_info"),
                       p("*Note : Click and drag on an area of the line plot to display values in the selected area.")
                )
                
      ),
      
      fluidRow(
        
        column(4, 
               titlePanel("Total PM 2.5 Accumulation by Nation Over April"),
               radioButtons(inputId = "mapType",
                            label = "Select Map Type",
                            choices = c("Markers", "Choropleth"),
                            selected = "Markers",
                            inline = TRUE),
               
               radioButtons(inputId = "frequency",
                            label = "Select Data Frequency",
                            choices = c("days", "weeks"),
                            selected = "weeks",
                            inline = TRUE),
               uiOutput("dateUI")
        ),
        column(8,
               leafletOutput("map", width = "100%", height = "500px")
        )
      ),
      
      fluidRow( class = "other-row",
        column(8, 
               plotOutput("corelations_PM25_FRP", width = "100%")
               ),
        column(4, 
               h2("Correlation between Parameters"),
               selectInput("select_parameters_with_updateSelectInput_corelation_x", 
                           "Choose a x - axis Parameter:",
                           choices = list("PM25", "CO2", "FRP"),
                           selected = "FRP"),
               
              selectInput("select_parameters_with_updateSelectInput_corelation_y", 
                    "Choose a y - axis Parameter:",
                    choices = list("PM25", "CO2", "FRP")
                    )
        ),
      ),
      
      fluidRow( 
        column(4,
               h3("Data Source"),
               h4("ECMWF is the European Centre for Medium-Range Weather Forecasts.")
               ),
        column(8,
               p("They are both a research institute and a 24/7 operational service, producing global numerical weather predictions and other data for our Member and Co-operating States and the broader community. The Centre has one of the largest supercomputer facilities and meteorological data archives in the world. Other strategic activities include delivering advanced training and assisting the WMO in implementing its programmes. They operate two services from the EU’s Copernicus Earth observation programme, the Copernicus Atmosphere Monitoring Service (CAMS) and the Copernicus Climate Change Service (C3S). They also contribute to the Copernicus Emergency Management Service (CEMS).")),
      )
      
    )
   