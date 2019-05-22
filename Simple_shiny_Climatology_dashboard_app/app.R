# Exploratory script for climatologies

library(tidyverse)
library(plotly)
library(lubridate)
library(zoo) 
library(raster) #

# change local to True when developing locally
local = system("uname -n", intern = T) == "matht250"#T

if (local) {
  basePath <- "~/Documents/GIT_REPOS/NSW-IMOS_data_viz_app/Simple_shiny_Climatology_dashboard_app/data/"#"~/ownCloud/Working_Directory/Postdoc-SchoolOfMathsStats/Scripts/Simple_shiny_Climatology_dashboard_app/data/"
              #"~/sci-maths-ocean/shared/PEOPLE/Steefan/climatology/data/"
} else {
  basePath <- "./data/"
}

load(paste0(basePath, "prerundata_21052019.RData"))
load(paste0(basePath, "latestSST.Rdata"))

# Plot climatology
plot_temp_ts <- function(mean = T, depth = pressures[1], smooth = 1) {
  nonNA_days <- which(!is.na(Temp_clim_P90[paste(depth),]) & !is.na(Temp_clim_P10[paste(depth),]) & !is.na(Temp_clim_mean[paste(depth),]))
  yday_noNA <- yday(dates)[nonNA_days]
  ylim <- range(Temp_clim_mean, Temp_clim_med, Temp_clim_P10, Temp_clim_P90, na.rm = T)
  p <- plot_ly(y = ~Temp_clim_P90[paste(depth),yday_noNA], type = 'scatter', mode = 'lines',
               showlegend = F, name = "90th pc of climatology",
               line = list(color = "transparent"),
               hoverlabel = list(namelength = -1)) %>%
    layout(title = paste("Pressure:", depth, "mbar"),
           xaxis = list(title = "Day of the year", range = c(1,365)),
           yaxis = list(title = paste(ifelse(mean, "Mean", "Median"), "temperature (degrees celcius)"),
                        range = ylim),
           paper_bgcolor = 'rgba(236,239,244,0)',
           plot_bgcolor = 'rgba(236,239,244,0)') %>%
    add_trace(y = ~Temp_clim_P10[paste(depth),yday_noNA], type = 'scatter', mode = "lines",
              line = list(color = "transparent", name = "10th pc of climatology"),
              fill = "tonexty", fillcolor = "rgba(255,0,0,0.4)", hoverinfo = "none") %>%
    add_trace(y = ~Temp_clim_P10[paste(depth),], type = 'scatter', mode = "lines",
              line = list(color = "transparent"), name = "10th pc of climatology")
  if(mean){
    smoothTS <- rollmean(x = Temp_clim_mean[paste(depth),], k = as.numeric(smooth), fill = NA)
    p <- p %>% add_trace(y = ~smoothTS, type = "scatter", mode = "lines",
                             line = list(color = "black"), name = "Mean climatology")
  } else {
    smoothTS <- rollmean(x = Temp_clim_med[paste(depth),], k = as.numeric(smooth), fill = NA)
    p <- p %>% add_trace(y = ~smoothTS, type = "scatter", mode = "lines",
                         line = list(color = "black"), name = "Median climatology")
  }
  return(p)
}


library(shiny)
library(shinydashboard)
library(leaflet)

ui <- 
  # fluidPage(
  # titlePanel(title = "TEMP climatology at the PH100/NRSPHB site"),
  # br(),
  dashboardPage(#theme = shinythemes::shinytheme("flatly"),
             dashboardHeader(title = "NSW-IMOS Observing Programme", titleWidth = 400),
             dashboardSidebar(width = 300,
               sidebarMenu(
                 menuItem("Home", tabName = "Home"),
                 menuItem("Port Hacking PH100/NRSPHB Mooring",
                          menuSubItem("Climatology", tabName = "PH100_Clim"),
                          menuSubItem("Marine Temperature Anomalies", tabName = "PH100_MHW")),
                 menuItem("Coffs Harbour CH100 Mooring", tabname = "CH100"),
                 menuItem("About", tabName = "About")
               )
             ),
             dashboardBody(
               tags$script(HTML("
        var openTab = function(tabName){
          $('a', $('.sidebar')).each(function() {
            if(this.getAttribute('data-value') == tabName) {
              this.click()
            };
          });
        }
      ")),
               tabItems(
                 tabItem("Home",
                         titlePanel("At a glance"),
                         br(),
                         # fluidRow(
                         #   infoBox(title = "Port Hacking Mooring",subtitle = "Climatology Temperature Today", 22, icon = icon("fire"),
                         #           color = "yellow", fill = T),
                         #   infoBox(title = "Coffs Harbour Mooring",subtitle = "Climatology Temperature Today", 19, icon = icon("fire"),
                         #           color = "green", fill = T),
                         #   infoBox(title = "Batemans Marine Park Mooring",subtitle = "Climatology Temperature Today", 26, icon = icon("fire"),
                         #           color = "red", fill = T),
                         #   infoBox(title = "Sydney Moorings",subtitle = "Climatology Temperature Today", 16, icon = icon("fire"),
                         #           color = "blue", fill = T)
                         # ),
                         fluidRow(column(12, align = "center", leafletOutput("stationMap_Home", height = 600))),
                         helpText(HTML("Colour of the labels indicate whether temperatures today are anomalous. A green label indicates the current temperatures are within the 10<sup>th</sup>",
                                  "and 90<sup>th</sup> percentiles of the climatology, a red label indicates the current temperatures are greater than the 90<sup>th</sup> percentile, and",
                                  "a blue label indicates the current temperatures are less than the 10<sup>th</sup> percentile."))
                 ),
                 tabItem("PH100_Clim",
                          sidebarLayout(
                            
                            sidebarPanel(
                              selectInput(inputId = "Mean",
                                          label = "Mean or Median",
                                          choices = c("Mean", "Median")),
                              radioButtons(inputId = "Smooth",
                                           label = "Number of days over which to smooth the climatology",
                                           choices =  c(1,4,7,14,28),
                                           selected = 1, inline = T),  
                              selectInput(inputId = "Pressure",
                                          label = "Select pressure level in dbar.",
                                          choices = pressures),
                              sliderInput(inputId = "Year",
                                          label = "Yearly data to be displayed over the climatology",
                                          min = 1954, max = 2018,
                                          value = 1954, animate = animationOptions(interval = 500)),
                              helpText("TOP: Temperature Climatology: mean or median of all temperature observations for a day of the year over all available years (black solid line). The pink shaded region represents the region enclosed by the 90th percentile and the 10th percentile of the termperature observations. Solid circles plotted over the climatology represent the daily averages for a specified year. Black, red and blue fills represent temperatures within the 90th and 10th percentiles, temperatures higher than the 90th percentile, and temperatures lower than the 10th percentiles respectively.
                                    BOTTOM: The total number of observations for each day of the year. These include observations from multiple data sources and over multiple years.")
                            ),
                            mainPanel(plotlyOutput(outputId = "clim_plot"),
                                      br(),
                                      plotlyOutput(outputId = "numObs_for_clim"))
                          )
                 ),
                 tabItem("PH100_MHW",
                          sidebarLayout(
                            
                            sidebarPanel(
                              selectInput(inputId = "Pressure2",
                                          label = "Select pressure level in dbar.",
                                          choices = pressures),
                              sliderInput(inputId = "MHWLen",
                                          label = "Minimum number of consecutive days of anomalous temperatures that can be defined as a heatwave",
                                          min = 2, max = 5,
                                          value = 2),
                              helpText("TOP: Number of marine temperature anomalies each year.
                                    BOTTOM: The maximum possible number of heat/coldwaves of specified length that can be detected based on the number of missing values in the data. As an example, a value of 80 on the log scale represents 10^80 possibilities for a heat/coldwave.")
                            ),
                            mainPanel(plotlyOutput(outputId = "num_MCW"),
                                      plotlyOutput(outputId = "num_complete_runs"))
                          )
                 ),
                 tabItem("About",
                         verticalLayout(
                           tags$head(HTML('<script async defer src="https://maps.googleapis.com/maps/api/js?key=AIzaSyBERuc7VFPIDMhCoUTMcE-z59CvoIe5wlU&sensor=true&callback=initMap"></script>
                                          <script type="text/javascript" src="imos_map.js"></script>')),
                           #<script type="text/javascript" src="http://maps.googleapis.com/maps/api/js?key=AIzaSyBERuc7VFPIDMhCoUTMcE-z59CvoIe5wlU&sensor=true"></script>,
                           tags$body(onload="initialize()",
                                     titlePanel("About"),
                                     p("The broad geographical reach of the East Australian Current (EAC) influences the climate and marine economies of nearly
                             half the Australian population, from Brisbane to Sydney, Melbourne, and Hobart. The poleward flowing EAC brings warm
                             water down the New South Wales (NSW) coast modulating the region’s climate as well as the composition, organisation and function
                             of marine ecosystems."),
                                     p("Since 2009, the 2000 km long coastline of New South Wales has been instrumented with a network of moored, remote sensing, and
                             in-situ observing platforms to monitor the EAC and its impact on waters of the highly populated east coast of Australia. This
                             network forms part of the larger national-level  Integrated Marine Observing System (IMOS - link to IMOS site.)"),
                                     p("NSW-IMOS have maintained a network of 8 shelf moorings at 3 latitudes (30°S, 34°S and 36°S) that measure temperature and
                             velocity every 5 mins at 8m depth  intervals throughout the water column. In addition  more than 40 autonomous glider missions
                             have been conducted along the east coast from 29°S to 34°S. Two high frequency coastal radar arrays measure surface velocities
                             over the core of the EAC and the EAC eddy field downstream of the separation point."),
                                     p("The key aims of NSW-IMOS are:"),
                                     tags$ul(tags$li("To contribute to national observations of decadal changes and climate variability of the EAC using common platforms and metrics."),
                                             tags$li("To investigate the EAC, its separation from the coast and the resultant eddy field along the coast of SE Australia."),
                                             tags$li("To quantify oceanographic processes on the continental shelf and slope off eastern Australia south of the Great Barrier Reef."),
                                             tags$li("To integrate the ecosystem response with oceanographic processes.")),
                                     h3("Moorings"),
                                     p("The following map shows the locations of all the moorings maintained by the NSW node of IMOS."),
                                     # leafletOutput("stationMap_About", height = 600),
                           HTML('<div id="map_canvas" style="margin-left:30px;width:90%; height:600px;">'))
                         )
                           
                           # htmlOutput("test")
                         )
                         
               )
             )
)


server <- function(input, output){
  
  output$clim_plot <- renderPlotly({
    pressure = input$Pressure
    mean = input$Mean == "Mean"
    smooth = input$Smooth
    p <- plot_temp_ts(mean = mean, depth = pressure, smooth = smooth)
    year = input$Year
    Temp_Avg <- yearly_data[[paste(year)]]
    hotPts <- which(Temp_Avg[paste(pressure),] > Temp_clim_P90[paste(pressure),])
    coldPts <- which(Temp_Avg[paste(pressure),] < Temp_clim_P10[paste(pressure),])
    avgPts <- which(Temp_Avg[paste(pressure),] <= Temp_clim_P90[paste(pressure),] & Temp_Avg[paste(pressure),] >= Temp_clim_P10[paste(pressure),])
    if (any(avgPts)) {
      p <- p %>% add_trace(x = ~avgPts, y = ~Temp_Avg[paste(pressure),avgPts], type = "scatter", mode = "markers",
                    name = paste(year, "yearly data"),
                    marker = list(size = 12,
                                  color = "rgba(0,0,0,0.4)",
                                  line = list(color = "rgba(43,140,190,1)",
                                              width = 2)))
    }
    if (any(hotPts)) {
     p <- p %>% add_trace(x = ~hotPts, y = ~Temp_Avg[paste(pressure),hotPts], type = "scatter", mode = "markers",
                name = paste(year, "yearly data"),
                marker = list(size = 12,
                              color = "rgba(255,0,0,1)",
                              line = list(color = "rgba(43,140,190,1)",
                                          width = 2)))
    }
    if (any(coldPts)) {
      p <- p %>% add_trace(x = ~coldPts, y = ~Temp_Avg[paste(pressure),coldPts], type = "scatter", mode = "markers",
                name = paste(year, "yearly data"),
                marker = list(size = 12,
                              color = "rgba(0,0,255,1)",
                              line = list(color = "rgba(43,140,190,1)",
                                          width = 2)))
    }
    p
  })
  
  output$numObs_for_clim <- renderPlotly({
    pressure = input$Pressure
    par(lwd = 0.5)
    # Number of observations barplot using plotly for the climatology page
    text = paste("Number of yrs per day:", Temp_clim_nyrs[paste(pressure),])
    plot_ly(x=1:365,y=Temp_clim_nobs[paste(pressure),],type = "bar", name = "Total observations per day", text = text) %>%
      layout(title = "Number of observations per day \nused to create climatology",
             paper_bgcolor = 'rgba(236,239,244,0)',
             plot_bgcolor = 'rgba(236,239,244,0)')
  })
  
  output$num_MCW <- renderPlotly({
    pressure = input$Pressure2
    runLen = input$MHWLen
    plot_ly(x=years,y=get(paste0("num.heatwaves.RL", runLen))[[which(pressures == pressure)]], name = "Marine coldwave", type = 'bar') %>%
      add_trace(y=get(paste0("num.coldwaves.RL", runLen))[[which(pressures == pressure)]], name = "Marine heatwave") %>%
      layout(xaxis = list(title = "Years"),
             yaxis = list(title = "Number of events"),
             barmode = "group",
             legend = list(x = 0, y = 0.99),
             paper_bgcolor = 'rgba(236,239,244,0)',
             plot_bgcolor = 'rgba(236,239,244,0)')
    
  })
  
  output$num_complete_runs <- renderPlotly({
    pressure = input$Pressure2
    runLen = input$MHWLen
    totalPossibleRuns_TS = get(paste0("num.complete.runs.TS.RL", runLen))[[which(pressures == pressure)]]
    ylab = "Maximum possible number of heat/coldwaves\n based on data availability"
    if (max(totalPossibleRuns_TS) > 200) {
      totalPossibleRuns_TS = ifelse(totalPossibleRuns_TS == 0, 0, log(totalPossibleRuns_TS))
      ylab = paste(ylab, "(log scale)")
    }
    plot_ly(x=years,y=totalPossibleRuns_TS, name = paste("Total possible events of length", runLen),
            type = 'bar') %>%
      layout(xaxis = list(title = "Years"),
             yaxis = list(title = ylab),
             paper_bgcolor = 'rgba(236,239,244,0)',
             plot_bgcolor = 'rgba(236,239,244,0)')
  })
  
  output$Caption_tab1 <- renderText("TOP: Temperature Climatology: mean or median of all temperature observations for a day of the year over all available years (black solid line). The pink shaded region represents the region enclosed by the 90th percentile and the 10th percentile of the termperature observations. Solid circles plotted over the climatology represent the daily averages for a specified year. Black, red and blue fills represent temperatures within the 90th and 10th percentiles, temperatures higher than the 90th percentile, and temperatures lower than the 10th percentiles respectively.
                                    BOTTOM: The total number of observations for each day of the year. These include observations from multiple data sources and over multiple years.")
  
  output$Caption_tab2 <- renderText("TOP: Number of marine temperature anomalies each year.
                                    BOTTOM: The maximum possible number of heat/coldwaves of specified length that can be detected based on the number of missing values in the data. As an example, a value of 80 on the log scale represents 10^80 possibilities for a heat/coldwave.")
  
  output$stationMap_Home <- renderLeaflet({
    colnames(stationLocs) <- c("site_code", "avg_lat", "avg_lon")
    stationLocs <- stationLocs %>% slice(c(3,6,8,9))
    
    # generate random temperatures based on climatology distributions
    doy  = yday(Sys.Date())
    sd = max((Temp_clim_P90[1,doy] - Temp_clim_mean[1,doy])/1.28, (Temp_clim_mean[1,doy] - Temp_clim_P10[1,doy])/1.28, na.rm = T)
    rTemps <- rnorm(4, mean = Temp_clim_mean[1,doy], sd = sd)
    
    # determine background-color based on climatology and rTemps
    rBG <- ifelse(rTemps < Temp_clim_P10[1, doy], "rgba(0,0,255,0.5)", 
                  ifelse(rTemps <= Temp_clim_P90[1, doy], "rgba(0,255,0,0.5)", "rgba(255,0,0,0.5)"))
    
    # determine colourmapping for sst raster image
    pal <- colorNumeric(
      palette = "magma",
      domain = values(sst))
    
    leaflet() %>% addTiles() %>% addMarkers(data = stationLocs %>% filter(site_code == "CH100"), lat = ~avg_lat, lng = ~avg_lon, 
                                            label = HTML(paste(sep = "<br/>", stationLocs %>% dplyr::filter(site_code == "CH100") %>% dplyr::select(site_code), paste(round(rTemps[1],1), "degrees"))),
                                            labelOptions = labelOptions(noHide = T, direction = "bottom", textsize = "15px",
                                                                        style = list("background-color" = rBG[1])),
                                            group = "Moorings") %>%
      addMarkers(data = stationLocs %>% dplyr::filter(site_code == "SYD100"), lat = ~avg_lat, lng = ~avg_lon, 
                 label = HTML(paste(sep = "<br/>", stationLocs %>% filter(site_code == "SYD100") %>% dplyr::select(site_code), paste(round(rTemps[2],1), "degrees"))),
                 labelOptions = labelOptions(noHide = T, direction = "right", textsize = "15px",
                                             style = list("background-color" = rBG[2])),
                 group = "Moorings") %>%
      addMarkers(data = stationLocs %>% dplyr::filter(site_code == "PH100"), lat = ~avg_lat, lng = ~avg_lon, 
                 label = HTML(paste(sep = "<br/>", 
                                    a(paste(stationLocs %>% dplyr::filter(site_code == "PH100") %>% dplyr::select(site_code)), onclick = "openTab('PH100_Clim')", href="#"),
                                    paste(round(rTemps[3],1), "degrees"))),
                 labelOptions = labelOptions(noHide = T, direction = "bottom", textsize = "15px",
                                             style = list("background-color" = rBG[3],
                                                          "pointer-events" = "auto")),
                 group = "Moorings") %>%
      addMarkers(data = stationLocs %>% dplyr::filter(site_code == "BMP120"), lat = ~avg_lat, lng = ~avg_lon, 
                 label = HTML(paste(sep = "<br/>", stationLocs %>% dplyr::filter(site_code == "BMP120") %>% dplyr::select(site_code), paste(round(rTemps[4],1), "degrees"))),
                 labelOptions = labelOptions(noHide = T, direction = "bottom", textsize = "15px",
                                             style = list("background-color" = rBG[4])),
                 group = "Moorings") %>%
      addRasterImage(sst, colors = pal, group = "SST") %>% addLegend(pal = pal, values = values(sst),
                                                      title = "Surface temp", group = "SST")
      # Layers control
      addLayersControl(
        overlayGroups = c("Moorings"),
        options = layersControlOptions(collapsed = FALSE),
        position = "topleft"
      )
  })
  
  output$stationMap_About <- renderLeaflet({
    stationLocs <- read_ods(paste0(basePath, "NSW-IMOS_assets_2018-10-30.ods"))
    colnames(stationLocs) <- c("site_code", "avg_lat", "avg_lon")
    leaflet() %>% addTiles() %>% addMarkers(data = stationLocs, lat = ~avg_lat, lng = ~avg_lon, popup = ~site_code)
  })
}

shinyApp(ui = ui, server = server)

# library(pracma)
# 
# y1 <- Temp_clim_nobs[1,]
# y2 <- Temp_clim_nyrs[1,]
# 
# plotyy(yearday, y1, yearday, y2, gridp = F)
# 
