# Exploratory script for climatologies

library(tidyverse)
library(plotly)
library(lubridate)
library(zoo) 
library(raster) #
library(shiny)
library(shinydashboard)
library(leaflet)
library(leaflet.minicharts)
library(htmltools)
library(htmlwidgets)

# change local to True when developing locally
local = system("uname -n", intern = T) == "MacBook-Pro-98.local" | system("uname -n", intern = T) == "matht250"#T

if (local) {
  basePath <- paste0(normalizePath("./data/"),"/")#"~/ownCloud/Working_Directory/Postdoc-SchoolOfMathsStats/Scripts/Simple_shiny_Climatology_dashboard_app/data/"
  #"~/sci-maths-ocean/shared/PEOPLE/Steefan/climatology/data/"
} else {
  basePath <- "/srv/shiny-server/data/"
}

prerundata.file <- sort(list.files(basePath, pattern = glob2rx("prerundata_*.RData")), decreasing = T)[1]
load(paste0(basePath, prerundata.file))
load(paste0(basePath, "SST/processedSSTandOC.Rdata"))
load(paste0(basePath, "HFRadar/HFRadar.RData"))

# Plot climatology
plot_temp_ts <- function(mean = T, depth = pressures[1], smooth = 1) {
  nonNA_days <- which(!is.na(Temp_clim_P90[paste(depth),]) & !is.na(Temp_clim_P10[paste(depth),]) & !is.na(Temp_clim_mean[paste(depth),]))
  yday_noNA <- yday(dates)[nonNA_days]
  ylim <- range(Temp_clim_mean, Temp_clim_med, Temp_clim_P10, Temp_clim_P90, na.rm = T)
  smoothP90 <- rollmean(x = Temp_clim_P90[paste(depth),], k = as.numeric(smooth), fill = NA)
  smoothP10 <- rollmean(x = Temp_clim_P10[paste(depth),], k = as.numeric(smooth), fill = NA)
  smoothP90 <- smoothP90[paste(1:365)]
  smoothP10 <- smoothP10[paste(1:365)]
  p <- plot_ly(x = as.integer(names(smoothP90)), y = ~smoothP90, type = 'scatter', mode = 'lines',
               showlegend = F, name = "90th pc of climatology",
               line = list(color = "transparent"),
               hoverlabel = list(namelength = -1)) %>%
    add_trace(x = as.integer(names(smoothP10)), y = ~smoothP10, type = 'scatter', mode = "lines",
              line = list(color = "transparent", name = "10th pc of climatology"),
              fill = "tonexty", fillcolor = "rgba(255,0,0,0.4)", hoverinfo = "none") %>%
    add_trace(x = as.integer(names(smoothP10)), y = ~smoothP10, type = 'scatter', mode = "lines",
              line = list(color = "transparent"), name = "10th pc of climatology")
  if(mean){
    smoothTS <- rollmean(x = Temp_clim_mean[paste(depth),], k = as.numeric(smooth), fill = NA)
    smoothTS <- smoothTS[paste(1:365)]
    p <- p %>% add_trace(x = as.integer(names(smoothTS)), y = ~smoothTS, type = "scatter", mode = "lines",
                         line = list(color = "black"), name = "Mean climatology")
  } else {
    smoothTS <- rollmean(x = Temp_clim_med[paste(depth),], k = as.numeric(smooth), fill = NA)
    smoothTS <- smoothTS[paste(1:365)]
    p <- p %>% add_trace(x = as.integer(names(smoothTS)), y = ~smoothTS, type = "scatter", mode = "lines",
                         line = list(color = "black"), name = "Median climatology")
  }
  p <- p %>% layout(title = paste("Pressure:", depth, "mbar"),
                    xaxis = list(title = "Day of the year", range = c(1,365)),
                    yaxis = list(title = paste(ifelse(mean, "Mean", "Median"), "temperature (degrees celcius)"),
                                 range = ylim),
                    paper_bgcolor = 'rgba(236,239,244,0)',
                    plot_bgcolor = 'rgba(236,239,244,0)')
  return(p)
}


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
                         # htmlOutput("homeframe"),
                         fluidRow(tags$div(class = "col-xs-1", align = "center", strong(h4("Date: "))),
                                  tags$div(class = "col-xs-1", align = "center", style = "padding-right:0", actionButton("prevDate",
                                                                                              label = HTML("<span class='small'><i class='glyphicon glyphicon-arrow-left'></i></span>"))),
                                  tags$div(class = "col-xs-3", align = "center", style = "padding:0", dateInput("date", NULL, min = min(df$date), max = max(df$date), 
                                                                        value = max(df$date), width = "100%") ),
                                  tags$div(class = "col-xs-1", align = "left", style = "padding-left:0",actionButton("nextDate",
                                                                                                       label = HTML("<span class='small'><i class='glyphicon glyphicon-arrow-right'></i></span>")))),
                         fluidRow(column(12, align = "center", leafletOutput("stationMap_Home", height = 700))),
                         # div(style="padding-left: 10px", absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                         #               draggable = TRUE, top = 580, left = "auto", width = 100, 
                                       # div(style = "padding-left: 5px", checkboxInput(inputId = "HFRadar_checkbox",label = "HF Radar", value = T)))),
                         helpText(HTML(paste(sep="</br>",
                                             "<strong>Description of data:</strong>",
                                             "<b>SST</b>: The Regional Australian Multi-Sensor SST Analysis (RAMSSA) v1.0 L4 SST product produced by the Australian Bureau of Meteorology (BOM) by optimally blending infrared and microwave SST data 
                                             from various satellites, with in situ SST data from ships, drifters and moorings. The SST data represents daily foundation temperatures free from diurnal warming. More information <a href='https://researchdata.ands.org.au/imos-srs-sst-ramssa-australia/1431818'>here</a>",
                                             "<b> Cold and Warm SSTs</b>: Cold (warm) SSTs are cooler (hotter) than the 10<sup>th</sup> (90<sup>th</sup>) percentiles of the monthly SST climatology.",
                                             "<b>SST Climatology</b>: SSTAARS (SST Atlas of Australian Regional Seas) climatology created by fitting four annual sinusoids (and a trend) to 25 years of daily, night-only AVHRR SST, L3S-1d, provided by BOM.",
                                             "<b>Ocean colour</b>: Chlorophyll-a concentration infered from sunlight reflected from within the ocean as measured by the MODIS sensor. The Chl-a inference is done using the OC3 algorithm, described 
                                             <a href='http://oceancolor.gsfc.nasa.gov/cms/atbd/chlor_a'>here</a>. Data obtained from <a href='https://researchdata.ands.org.au/imos-srs-modis-oc3-model/961150'>here</a>.",
                                             "<b>Ocean current velocities</b>: Represented by white animated bezier curves. Data obtained from the Coffs Harbour and Newcastle IMOS Australian Coastal Ocean Radar (ACORN) Facilities. Note that the curves represent 
                                             ocean velocities (instead of actual location of the current). This is because the curves are scaled by an arbitrary scaling constant for easier visualisation. As such certain high velocity currents may go over land. Furthermore the direction of 
                                             the entire bezier curve (start to end) represents the direction of the current at the location at the start of the bezier curve.
                                             Note also that due to its realtime nature the data ocean current data is not quality controlled. 
                                             The curves represent the change in velocities between around 0900h, 3pm and 9pm UTC time for each day. Data can be downloaded <a href='http://thredds.aodn.org.au/thredds/catalog/IMOS/ACORN/gridded_1h-avg-current-map_non-QC/catalog.html'>here</a>."
                                       )))
                                  #      "Colour of the labels indicate whether temperatures today are anomalous. A green label indicates the current temperatures are within the 10<sup>th</sup>",
                                  # "and 90<sup>th</sup> percentiles of the climatology, a red label indicates the current temperatures are greater than the 90<sup>th</sup> percentile, and",
                                  # "a blue label indicates the current temperatures are less than the 10<sup>th</sup> percentile."))
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
                           # tags$head(HTML('<script async defer src="https://maps.googleapis.com/maps/api/js?key=AIzaSyBERuc7VFPIDMhCoUTMcE-z59CvoIe5wlU&sensor=true&callback=initMap"></script>
                           #                <script type="text/javascript" src="imos_map.js"></script>')),
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
                                     leafletOutput("stationMap_About", height = 600))
                           # HTML('<div id="map_canvas" style="margin-left:30px;width:90%; height:600px;">'))
                         )
                           
                           # htmlOutput("test")
                         )
                         
               )
             )
)


server <- function(input, output, session){
  
  observeEvent(input$prevDate, {
    current <- input$date
    if (current > min(df$date)) {
      updateDateInput(session, "date", value = current - 1)
    }
  })
  
  observeEvent(input$nextDate, {
    current <- input$date
    if (current < max(df$date)) {
      updateDateInput(session, "date", value = current + 1)
    }
  })
  
  output$clim_plot <- renderPlotly({
    pressure = input$Pressure
    mean = input$Mean == "Mean"
    smooth = input$Smooth
    p <- plot_temp_ts(mean = mean, depth = pressure, smooth = smooth)
    year = input$Year
    Temp_Avg <- yearly_data[[paste(year)]]
    # colnames(Temp_Avg) <- rep("", ncol(Temp_Avg))
    smoothP90 <- rollmean(x = Temp_clim_P90[paste(pressure),], k = as.numeric(smooth), fill = NA)
    smoothP10 <- rollmean(x = Temp_clim_P10[paste(pressure),], k = as.numeric(smooth), fill = NA)
    hotPts <- which(Temp_Avg[paste(pressure),] > smoothP90)
    coldPts <- which(Temp_Avg[paste(pressure),] < smoothP10)
    avgPts <- which(Temp_Avg[paste(pressure),] <= smoothP90 & Temp_Avg[paste(pressure),] >= smoothP10)
    if (any(avgPts)) {
      p <- p %>% add_trace(x = ~as.integer(names(avgPts)), y = ~Temp_Avg[paste(pressure),names(avgPts)], type = "scatter", mode = "markers",
                    name = paste(year, "yearly data"),
                    marker = list(size = 12,
                                  color = "rgba(0,0,0,0.4)",
                                  line = list(color = "rgba(43,140,190,1)",
                                              width = 2)))
    }
    if (any(hotPts)) {
     p <- p %>% add_trace(x = ~as.integer(names(hotPts)), y = ~Temp_Avg[paste(pressure),names(hotPts)], type = "scatter", mode = "markers",
                name = paste(year, "yearly data"),
                marker = list(size = 12,
                              color = "rgba(255,0,0,1)",
                              line = list(color = "rgba(43,140,190,1)",
                                          width = 2)))
    }
    if (any(coldPts)) {
      p <- p %>% add_trace(x = ~as.integer(names(coldPts)), y = ~Temp_Avg[paste(pressure),names(coldPts)], type = "scatter", mode = "markers",
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
    plot_ly(x=years,y=get(paste0("num.coldwaves.RL", runLen))[[which(pressures == pressure)]], name = "Marine coldwave", type = 'bar') %>%
      add_trace(y=get(paste0("num.heatwaves.RL", runLen))[[which(pressures == pressure)]], name = "Marine heatwave") %>%
      layout(xaxis = list(title = "Years", range=c(2009,2019)),
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
      layout(xaxis = list(title = "Years", range = c(2009, 2019)),
             yaxis = list(title = ylab),
             paper_bgcolor = 'rgba(236,239,244,0)',
             plot_bgcolor = 'rgba(236,239,244,0)')
  })
  
  output$Caption_tab1 <- renderText("TOP: Temperature Climatology: mean or median of all temperature observations for a day of the year over all available years (black solid line). The pink shaded region represents the region enclosed by the 90th percentile and the 10th percentile of the termperature observations. Solid circles plotted over the climatology represent the daily averages for a specified year. Black, red and blue fills represent temperatures within the 90th and 10th percentiles, temperatures higher than the 90th percentile, and temperatures lower than the 10th percentiles respectively.
                                    BOTTOM: The total number of observations for each day of the year. These include observations from multiple data sources and over multiple years.")
  
  output$Caption_tab2 <- renderText("TOP: Number of marine temperature anomalies each year.
                                    BOTTOM: The maximum possible number of heat/coldwaves of specified length that can be detected based on the number of missing values in the data. As an example, a value of 80 on the log scale represents 10^80 possibilities for a heat/coldwave.")
  
  output$homeframe <- renderUI({
    tags$iframe(src="figures/home_leaflet_map.html", width="100%", height="500", marginwidth="0", marginheight="0", frameborder="0", vspace="0", hspace="0", seamless="seamless")
  })
  
  output$stationMap_Home <- renderLeaflet({
    # colnames(stationLocs) <- c("site_code", "avg_lat", "avg_lon")
    # stationLocs <- stationLocs %>% slice(c(3,6,8,9))
    # 
    # # generate random temperatures based on climatology distributions
    # doy  = yday(Sys.Date())
    # sd = max((Temp_clim_P90[1,doy] - Temp_clim_mean[1,doy])/1.28, (Temp_clim_mean[1,doy] - Temp_clim_P10[1,doy])/1.28, na.rm = T)
    # rTemps <- rnorm(4, mean = Temp_clim_mean[1,doy], sd = sd)
    # 
    # # determine background-color based on climatology and rTemps
    # rBG <- ifelse(rTemps < Temp_clim_P10[1, doy], "rgba(0,0,255,0.5)",
    #               ifelse(rTemps <= Temp_clim_P90[1, doy], "rgba(0,255,0,0.5)", "rgba(255,0,0,0.5)"))
    n <- which(df$date == input$date)
    date <- df$date[n]
    sst <- sst_month[[n]]
    sst_10 <- sst_10_month[[n]]
    sst_90 <- sst_90_month[[n]]
    oc <- oc_month[[n]]
    
    # determine colourmapping for sst raster image
    pal <- colorNumeric(
      palette = "magma",
      domain = values(sst),
      na.color = "#00000000")

    m <- leaflet() %>% addTiles() %>% setView(lng = 153.5, lat = -32.5, zoom = 7)

    m <- m %>% addRasterImage(x = sst, colors = pal, group = "SST",opacity = 0.8) %>%
      addLegend(pal = pal, values = values(sst), opacity = 0.7, #labFormat = labelFormat(transform = function(x) {sort(x, decreasing = T)}),
                title = "Surface temp", group = c("SST"), position = "topleft") %>% #, labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
      addRasterImage(x = sst_10, colors = pal, group = "Cold SSTs", opacity = 0.8) %>%
      addRasterImage(x = sst_90, colors = pal, group = "Warm SSTs", opacity = 0.8) #%>%
      # addLabelOnlyMarkers(lng = 151.4, lat = -27.9, label = HTML(paste("Date:<br>",date)),
      #                     labelOptions = labelOptions(noHide = T, direction = "bottom", textsize = "15px")) #%>%
      # addMarkers(data = stationLocs %>% filter(site_code == "CH100"), lat = ~avg_lat, lng = ~avg_lon,
      #                                       label = HTML(paste(sep = "<br/>", stationLocs %>% dplyr::filter(site_code == "CH100") %>% dplyr::select(site_code), paste(round(rTemps[1],1), "degrees"))),
      #                                       labelOptions = labelOptions(noHide = T, direction = "bottom", textsize = "15px",
      #                                                                   style = list("background-color" = rBG[1])),
      #                                       group = "Moorings") %>%
      # addMarkers(data = stationLocs %>% dplyr::filter(site_code == "SYD100"), lat = ~avg_lat, lng = ~avg_lon,
      #            label = HTML(paste(sep = "<br/>", stationLocs %>% filter(site_code == "SYD100") %>% dplyr::select(site_code), paste(round(rTemps[2],1), "degrees"))),
      #            labelOptions = labelOptions(noHide = T, direction = "right", textsize = "15px",
      #                                        style = list("background-color" = rBG[2])),
      #            group = "Moorings") %>%
      # addMarkers(data = stationLocs %>% dplyr::filter(site_code == "PH100"), lat = ~avg_lat, lng = ~avg_lon,
    #            label = HTML(paste(sep = "<br/>",
    #                               a(paste(stationLocs %>% dplyr::filter(site_code == "PH100") %>% dplyr::select(site_code)), onclick = "openTab('PH100_Clim')", href="#"),
    #                               paste(round(rTemps[3],1), "degrees"))),
    #            labelOptions = labelOptions(noHide = T, direction = "bottom", textsize = "15px",
    #                                        style = list("background-color" = rBG[3],
    #                                                     "pointer-events" = "auto")),
    #            group = "Moorings") %>%
    # addMarkers(data = stationLocs %>% dplyr::filter(site_code == "BMP120"), lat = ~avg_lat, lng = ~avg_lon,
    #            label = HTML(paste(sep = "<br/>", stationLocs %>% dplyr::filter(site_code == "BMP120") %>% dplyr::select(site_code), paste(round(rTemps[4],1), "degrees"))),
    #            labelOptions = labelOptions(noHide = T, direction = "bottom", textsize = "15px",
    #                                        style = list("background-color" = rBG[4])),
    #            group = "Moorings") %>%
    
    if (!is.na(df$OC_filename[n])) {
      if (any(!is.na(values(oc)))) {
        # determine colourmapping for oc raster image
        palOC <- colorNumeric(
          palette = "viridis",
          domain = log(values(oc)),
          na.color = "transparent")
        
        m <- m %>% addRasterImage(x = log(oc), colors = palOC, group = "Ocean Colour", opacity = 0.8) %>%
          addLegend(pal = palOC, values = rev(log(values(oc))), labFormat = labelFormat(transform = exp), opacity = 0.7,
                    title = "Ocean colour \n(Chl-a)", group = "Ocean Colour", position = "topleft",)
      }
    }
    
    m <- m %>%     
      # Layers control
      addLayersControl(
        baseGroups = c("SST", "Cold SSTs", "Warm SSTs", "Ocean Colour"),
        # overlayGroups = c("SST", "Ocean Colour"),
        options = layersControlOptions(collapsed = FALSE, autoZIndex = T),
        position = "topleft"
      )# %>% addFlows(uv_cart_df$lon0, uv_cart_df$lat0, uv_cart_df$lon1, uv_cart_df$lat1, maxThickness = 0.5)
    
    # load javascript plugin 
    curveplugin <- htmlDependency("leaflet.curve", "0.5.2",
                                  src = file.path(normalizePath("www")),
                                  script = "leaflet.curve.js")
    
    # A function that takes a plugin htmlDependency object and adds
    # it to the map. This ensures that however or whenever the map
    # gets rendered, the plugin will be loaded into the browser.
    registerPlugin <- function(map, plugin) {
      map$dependencies <- c(map$dependencies, list(plugin))
      map
    }
    
    uv_cart_df <- UVCart_month[[n]]
    m %>% # Register ESRI plugin on this map instance
      registerPlugin(curveplugin) %>%
      # Add your custom JS logic here. The `this` keyword
      # refers to the Leaflet (JS) map object.
      onRender(paste("function(el, x) {",
                     paste0("L.curve(['M', [", uv_cart_df$lat0, ",", uv_cart_df$lon0, 
                            "], 'C', [", uv_cart_df$lat1, ",", uv_cart_df$lon1, "], [", 
                            uv_cart_df$lat2, ",", uv_cart_df$lon2[], "], [",
                            uv_cart_df$lat3, ",", uv_cart_df$lon3[], "]], ",
                            "{weight: 0.5, color: 'white', animate: {duration: 1500, iterations: Infinity}}).addTo(this);", sep = " ", collapse = "\n"),
                     "}",sep = "\n"))
  })
  
  # observeEvent(input$HFRadar_checkbox, {
  #   
  #   m <- leafletProxy("stationMap_Home")
  #   
  #   if(input$HFRadar_checkbox) {
  #     m %>% addFlows(uv_cart_df$lon0, uv_cart_df$lat0, uv_cart_df$lon1, uv_cart_df$lat1, maxThickness = 0.5)
  #   } else {
  #     m %>% clearFlows()
  #   }
  # })
  
  output$stationMap_About <- renderLeaflet({
    leaflet() %>% addTiles() %>%
      addMarkers(lat = -33.841, lng = 151.264, 
                 popup = HTML(paste0('<h1 id="firstHeading" class="firstHeading">SHMO</h1>',
                                     '<div id="bodyContent">',
                                     '<p>The <b>S</b>ydney <b>H</b>arbour <b>M</b>arine <b>O</b>bservatory Realtime Buoy <b>SHMO</b> is located 0.4nm Southwest of the Sow and Pigs Reef between the Western and Eastern Channel.</p>',
                                     '<p><a href="http://www.oceanography.unsw.edu.au/realtime.html">',
                                     'Link to realtime data</a></p>'))
      ) %>%
      addMarkers(lat = -30.275, lng = 153.300, 
                 popup = HTML(paste0('<h1 id="firstHeading" class="firstHeading">CH070</h1>',
                                     '<p>The Coffs Harbour mooring <b>CH070</b> is located in 70 m water depth</p>',
                                     '<p><a href="./nsw-imos/CH070_latest.html">',
                                     'Plot of 12 Month Data</a></p>'))
      ) %>%
      addMarkers(lat = -30.268, lng = 153.397, 
                 popup = HTML(paste0('<h1 id="firstHeading" class="firstHeading">CH100</h1>',
                                     '<div id="bodyContent">',
                                     '<p>The Coffs Harbour mooring <b>CH100</b> is located in 100 m water depth</p>',
                                     '<p><a href="./nsw-imos/CH100_latest.html">',
                                     'Plot of 12 Month Data</a></p>'))
      ) %>%
      addMarkers(lat = -33.898, lng = 151.315, 
                 popup = HTML(paste0('<h1 id="firstHeading" class="firstHeading">ORS065</h1>',
                                     '<p>The Sydney Water Ocean Reference Station mooring <b>ORS065</b> is located in 65 m water depth</p>',
                                     '<p><a href="./nsw-imos/ORS065_latest.html">',
                                     'Plot of 12 Month Data</a></p>',
                                     '<p><a href="./nsw-imos/ORS065_Realtime.html">',
                                     'Plot of Realtime Data</a></p>'))
      ) %>%
      addMarkers(lat = -33.943, lng = 151.382, 
                 popup = HTML(paste0('<h1 id="firstHeading" class="firstHeading">SYD100</h1>',
                                     '<div id="bodyContent">',
                                     '<p>The Sydney mooring <b>SYD100</b> is located in 100 m water depth</p>',
                                     '<p><a href="./nsw-imos/SYD100_latest.html">',
                                     'Plot of 12 Month Data</a></p>'))
      ) %>%
      addMarkers(lat = -33.994, lng = 151.495, 
                 popup = HTML(paste0('<h1 id="firstHeading" class="firstHeading">SYD140</h1>',
                                     '<p>The Sydney mooring <b>SYD140</b> is located in 140 m water depth</p>',
                                     '<p><a href="./nsw-imos/SYD140_latest.html">',
                                     'Plot of 12 Month Data</a></p>'))
      ) %>%
      addMarkers(lat = -34.120, lng = 151.224, 
                 popup = HTML(paste0('<h1 id="firstHeading" class="firstHeading">PH100</h1>',
                                     '<p>The Port Hacking NRS mooring <b>PH100</b> is located in 100 m water depth</p>',
                                     '<p><a href="./nsw-imos/PH100_latest.html">',
                                     'Plot of 12 Month Data</a></p>'))
      ) %>%
      addMarkers(lat = -36.19, lng = 150.19, 
                 popup = HTML(paste0('<h1 id="firstHeading" class="firstHeading">BMP070</h1>',
                                     '<p>The Batemans Marine Park mooring <b>BMP070</b> is located in 70 m water depth</p>',
                                     '<p><a href="./nsw-imos/BMP070_latest.html">',
                                     'Plot of 12 Month Data</a></p>'))
      ) %>%
      addMarkers(lat = -36.192, lng = 150.233, 
                 popup = HTML(paste0('<h1 id="firstHeading" class="firstHeading">BMP090</h1>',
                                     '<p>The Batemans Marine Park mooring <b>BMP090</b> was located in 90 m water depth</p>',
                                     '<p><a href="./nsw-imos/BMP090_latest.html">',
                                     'Plot of 12 Month Data</a></p>'))
      ) %>%
      addMarkers(lat = -36.213, lng = 150.309, 
                 popup = HTML(paste0('<h1 id="firstHeading" class="firstHeading">BMP120</h1>',
                                     '<p>The Batemans Marine Park mooring <b>BMP120</b> is located in 120 m water depth</p>',
                                     '<p><a href="./nsw-imos/BMP120_latest.html">',
                                     'Plot of 12 Month Data</a></p>'))
      )
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
