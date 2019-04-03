# Exploratory script for climatologies

library(ncdf4)
library(zoo)
library(lubridate)
library(readODS)
library(tidyverse)

# change local to True when developing locally
local = system("uname -n", intern = T) == "matht250"#T

if (local) {
  basePath <- "~/ownCloud/Working_Directory/Postdoc-SchoolOfMathsStats/Scripts/Simple_shiny_Climatology_dashboard_app/data/"
              #"~/sci-maths-ocean/shared/PEOPLE/Steefan/climatology/data/"
} else {
  basePath <- "./data/"
}

nc <- nc_open(filename = paste0(basePath,"IMOS_ANMN-NSW_TZ_S19535120000Z_PH100NRSPHB_FV02_CLIMATOLOGY_TEMP_E20187120000Z_C20190214123834Z.nc"))
Temp_clim_mean <- ncvar_get(nc, varid = "TEMP_AVE")
Temp_clim_med <- ncvar_get(nc, varid = "TEMP_MED")
Temp_clim_std <- ncvar_get(nc, varid = "TEMP_STD")
Temp_clim_P90 <- ncvar_get(nc, varid = "TEMP_PER90")
Temp_clim_P10 <- ncvar_get(nc, varid = "TEMP_PER10")
Temp_clim_nobs <- ncvar_get(nc, varid = "NOBS")
Temp_clim_nyrs <- ncvar_get(nc, varid = "NYRS")
pressures <- ncvar_get(nc, varid = "PRES")
time <- ncvar_get(nc, varid = "TIME")
nc_close(nc)

dates <- ymd("19500101") + time
yearday <- yday(dates)

dimnames(Temp_clim_mean) <- list(paste(pressures), paste(yearday))
dimnames(Temp_clim_med) <- list(paste(pressures), paste(yearday))
dimnames(Temp_clim_std) <- list(paste(pressures), paste(yearday))
dimnames(Temp_clim_P90) <- list(paste(pressures), paste(yearday))
dimnames(Temp_clim_P10) <- list(paste(pressures), paste(yearday))
dimnames(Temp_clim_nobs) <- list(paste(pressures), paste(yearday))
dimnames(Temp_clim_nyrs) <- list(paste(pressures), paste(yearday))

########################
# load yearly data
get_fnames <- function(years) {
  # function to fetch file names with a particular year given a list of years
  fnames <- c()
  for (y in years) {
    fname <- list.files(path = basePath, 
                        pattern = glob2rx(paste0("*S",y,"*FV02_AVERAGE_TEMP*E",y,"*")))
    fnames <- append(fnames, fname)
  }
  return(fnames)
}

read_yearly_data <- function(fname) {
  # function to read yearly data give a vector of nc file names
  if(fname == "") stop("No file name provided")
  nc <- nc_open(filename = paste0(basePath,
                                  fname))
  Temp_Avg <- ncvar_get(nc, varid = "TEMP_AVE")
  nc_close(nc)
  dimnames(Temp_Avg) <- list(paste(pressures), paste(yearday))
  return(Temp_Avg)
}

is.anomalous <- function(temp_ts, Temp_clim_P90_ts, Temp_clim_P10_ts) {
  # function that checks yearly_data to see if temperatures are outside the 90th and 10th pc
  temp_ts > Temp_clim_P90_ts | temp_ts < Temp_clim_P10_ts
}

is.hot.anomaly <- function(temp_ts, Temp_clim_P90_ts) {
  # function that checks yearly_data to see if temperatures are outside the 90th and 10th pc
  temp_ts > Temp_clim_P90_ts
}

is.cold.anomaly <- function(temp_ts, Temp_clim_P10_ts) {
  # function that checks yearly_data to see if temperatures are outside the 90th and 10th pc
  temp_ts < Temp_clim_P10_ts
}

num.True.runLen <- function(boolTS, runLen = 2) {
  length(which(rle(boolTS)$lengths[which(rle(boolTS)$values == 1)] >= runLen))
}

num.complete.runs <- function(TS, runLen = 2) {
  boolTS <- !is.na(TS)
  num.True.runLen(boolTS = boolTS, runLen = runLen)
}

possibleRuns <- function(n, runLen = 2) {
  if(n < runLen){
    return(0)
  } else if (n == runLen) {
    return(1)
  } else {
    return(2*possibleRuns(n-1, runLen = runLen)+1)
  }
}

totalPossibleRuns <- function(TS, runLen = 2) {
  boolTS <- !is.na(TS)
  runs <- rle(boolTS)$length[which(rle(boolTS)$values == T)]
  sum(unlist(sapply(runs, FUN = possibleRuns, runLen = runLen)))
}


create.num.True.runLen.TS <- function(yearly_data, hotAnomaly = T, depth = pressures[1], runLen = 2) {
  sapply(lapply(yearly_data, FUN = function(x) {
    if (hotAnomaly) {
    is.hot.anomaly(temp_ts = x[paste(depth),], 
                 Temp_clim_P90_ts = Temp_clim_P90[paste(depth), ])
    } else {
      is.cold.anomaly(temp_ts = x[paste(depth),], 
                   Temp_clim_P10_ts = Temp_clim_P10[paste(depth), ])
    }
  }), FUN = num.True.runLen, runLen = runLen)
}

create.num.complete.runs.TS <- function(yearly_data, depth = pressures[1], runLen = 2) {
  #sapply(l
         sapply(yearly_data, FUN = function(x, RL) {
    totalPossibleRuns(x[paste(depth),], runLen = RL)#, 
                 # Temp_clim_P90_ts = Temp_clim_P90[paste(depth), ], 
                 # Temp_clim_P10_ts = Temp_clim_P10[paste(depth), ])
  }, RL = runLen)#, FUN = num.complete.runs, runLen = runLen)
}

years <- seq(1954,2018)
fnames <- get_fnames(years)
yearly_data <- lapply(fnames, FUN = read_yearly_data)
years <- as.numeric(substring(fnames, regexpr("*TZ_S", fnames) + 4, regexpr("*TZ_S", fnames) + 7))
names(yearly_data) <- years

# bool <- is.anomalous(yearly_data$`2015`[,1], 
#                      Temp_Clim_P90_ts = Temp_Clim_P90_grid[1,], 
#                      Temp_Clim_P10_ts = Temp_Clim_P10_grid[1,])

# Heatwaves
# runLen = 2
num.heatwaves.RL2 <- lapply(pressures, FUN = create.num.True.runLen.TS, yearly_data = yearly_data, runLen = 2, hotAnomaly = T)
num.complete.runs.TS.RL2 <- lapply(pressures, FUN = create.num.complete.runs.TS, yearly_data = yearly_data, runLen = 2)
# runLen = 3
num.heatwaves.RL3 <- lapply(pressures, FUN = create.num.True.runLen.TS, yearly_data = yearly_data, hotAnomaly = T, runLen = 3)
num.complete.runs.TS.RL3 <- lapply(pressures, FUN = create.num.complete.runs.TS, yearly_data = yearly_data, runLen = 3)
# runLen = 4
num.heatwaves.RL4 <- lapply(pressures, FUN = create.num.True.runLen.TS, yearly_data = yearly_data, hotAnomaly = T, runLen = 4)
num.complete.runs.TS.RL4 <- lapply(pressures, FUN = create.num.complete.runs.TS, yearly_data = yearly_data, runLen = 4)
# runLen = 5
num.heatwaves.RL5 <- lapply(pressures, FUN = create.num.True.runLen.TS, yearly_data = yearly_data, hotAnomaly = T, runLen = 5)
num.complete.runs.TS.RL5 <- lapply(pressures, FUN = create.num.complete.runs.TS, yearly_data = yearly_data, runLen = 5)

# Coldwaves
# runLen = 2
num.coldwaves.RL2 <- lapply(pressures, FUN = create.num.True.runLen.TS, yearly_data = yearly_data, hotAnomaly = F, runLen = 2)
# runLen = 3
num.coldwaves.RL3 <- lapply(pressures, FUN = create.num.True.runLen.TS, yearly_data = yearly_data, hotAnomaly = F, runLen = 3)
# runLen = 4
num.coldwaves.RL4 <- lapply(pressures, FUN = create.num.True.runLen.TS, yearly_data = yearly_data, hotAnomaly = F, runLen = 4)
# runLen = 5
num.coldwaves.RL5 <- lapply(pressures, FUN = create.num.True.runLen.TS, yearly_data = yearly_data, hotAnomaly = F, runLen = 5)
########################
# Create a data frame of all yearly data

# year = 1955
# yearly_data_frame <- function(year) {
#   fname <- list.files(path = basePath, 
#                       pattern = glob2rx(paste0("*S",year,"*FV02_AVERAGE_TEMP*E",year,"*")))
#   if (length(fname) == 0) stop(paste("No data available for", year))
#   nc <- nc_open(filename = paste0(basePath,
#                                   fname))
#   Temp_Avg <- ncvar_get(nc, varid = "TEMP_AVE")
#   date <- ncvar_get(nc, varid = "TIME") + ymd("19500101")
#   pressure <- ncvar_get(nc, varid = "PRES")
#   nc_close(nc)
#   yearly_data <- bind_cols(crossing(pressure, date), data_frame(temperature = c(Temp_Avg)))
# }
# 

# yearly_data <- data_frame()
# years <- 1954:2018; years <- years[which(years != 1963)]
# yearly_data <- bind_rows(lapply(years, FUN = yearly_data_frame))
# 
# library(plotly)

# yearly_data %>% filter(pressure == 30) %>% 
#   plot_ly(x = ~date, 
#           y = ~temperature)#,
#           mode = "lines")#,
#           connectgaps = TRUE)

########################

# Read in NINO3.4 index
# NIN3.4 <- read.table(url("https://www.esrl.noaa.gov/psd/gcos_wgsp/Timeseries/Data/nino34.long.anom.data"),
#                    skip = 85, nrows = 65, sep = "", na.strings = "-99.99")


# Plot climatology
plot_temp_ts <- function(mean = T, depth = pressures[1], smooth = 1) {
  plot(0, xlim = c(1,365), ylim = range(Temp_clim_mean, na.rm = T),# y = Temp_Clim_Mean_grid[1,],
       xlab = "Day of the year", ylab = paste(ifelse(mean, "Mean", "Median"), "temperature"),
       main = paste("Pressure =", depth, "dbar"))
  nonNA_days <- which(!is.na(Temp_clim_P90[paste(depth),]) & !is.na(Temp_clim_P10[paste(depth),]))
  yday_noNA <- yday(dates)[nonNA_days]
  Temp_clim_P90_noNA <- Temp_clim_P90[paste(depth), nonNA_days]
  Temp_clim_P10_noNA <- Temp_clim_P10[paste(depth), nonNA_days]
  polygon(x = c(yday_noNA,rev(yday_noNA)),
          y = c(Temp_clim_P90_noNA,rev(Temp_clim_P10_noNA)),
          col = "#fa9fb555", border = NA)
  if (mean) {
    smoothTS <- rollmean(x = Temp_clim_mean[paste(depth),], k = as.numeric(smooth), fill = NA)
    lines(smoothTS)
  } else {
      smoothTS <- rollmean(x = Temp_clim_med[paste(depth),], k = as.numeric(smooth), fill = NA)
      lines(smoothTS)
  }
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
                         fluidRow(column(12, align = "center", leafletOutput("stationMap_Home", height = 600)))
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
                              textOutput(outputId = "Caption_tab1")
                            ),
                            mainPanel(plotOutput(outputId = "clim_plot"),
                                      plotOutput(outputId = "numObs_for_clim"))
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
                              textOutput(outputId = "Caption_tab2")
                            ),
                            mainPanel(plotOutput(outputId = "num_MCW"),
                                      plotOutput(outputId = "num_complete_runs"))
                          )
                 ),
                 tabItem("About",
                         leafletOutput("stationMap_About", height = 800))
               )
             )
)


server <- function(input, output){
  
  output$clim_plot <- renderPlot({
    pressure = input$Pressure
    mean = input$Mean == "Mean"
    smooth = input$Smooth
    plot_temp_ts(mean = mean, depth = pressure, smooth = smooth)
    year = input$Year
    fname <- list.files(path = basePath, 
                        pattern = glob2rx(paste0("*S",year,"*FV02_AVERAGE_TEMP*E",year,"*")))
    # nc <- nc_open(filename = paste0(basePath,
    #                                 fname))
    # Temp_Avg <- ncvar_get(nc, varid = "TEMP_AVERAGE")
    # nc_close(nc)
    Temp_Avg <- read_yearly_data(fname)
    points(x = yearday, y = Temp_Avg[paste(pressure),], pch = 19)
    hotPts <- which(Temp_Avg[paste(pressure),] > Temp_clim_P90[paste(pressure),])
    coldPts <- which(Temp_Avg[paste(pressure),] < Temp_clim_P10[paste(pressure),])
    points(x = hotPts, y = Temp_Avg[paste(pressure), hotPts], pch = 19, col = "red")
    points(x = coldPts, y = Temp_Avg[paste(pressure), coldPts], pch = 19, col = "blue")
  })
  
  output$numObs_for_clim <- renderPlot({
    pressure = input$Pressure
    par(lwd = 0.5)
    barplot(Temp_clim_nobs[paste(pressure),],
         xlab = "Day of the year", ylab = "Number of observations used to create climatology")
  })
  
  output$num_MCW <- renderPlot({
    pressure = input$Pressure2
    runLen = input$MHWLen
    data = rbind(get(paste0("num.coldwaves.RL", runLen))[[which(pressures == pressure)]],
                 get(paste0("num.heatwaves.RL", runLen))[[which(pressures == pressure)]])
    par(mar=c(5.1,5.1,4.1,2.1))
    barplot(data, col = c("blue", "red"), beside = T,
            xlab = "Years", ylab = "Number of heat/coldwave events")
    legend('topleft',fill=c("blue", "red"),legend=c('Coldwaves','Heatwaves'))
  })
  
  output$num_complete_runs <- renderPlot({
    pressure = input$Pressure2
    runLen = input$MHWLen
    par(mar=c(5.1,5.1,4.1,2.1))
    totalPossibleRuns_TS = get(paste0("num.complete.runs.TS.RL", runLen))[[which(pressures == pressure)]]
    ylab = "Maximum possible number of heat/coldwaves\n based on data availability"
    if (max(totalPossibleRuns_TS) > 200) {
      totalPossibleRuns_TS = ifelse(totalPossibleRuns_TS == 0, 0, log(totalPossibleRuns_TS))
      ylab = paste(ylab, "(log scale)")
    }
    barplot(totalPossibleRuns_TS,
            xlab = "Years", ylab = ylab)
  })
  
  output$Caption_tab1 <- renderText("TOP: Temperature Climatology: mean or median of all temperature observations for a day of the year over all available years (black solid line). The pink shaded region represents the region enclosed by the 90th percentile and the 10th percentile of the termperature observations. Solid circles plotted over the climatology represent the daily averages for a specified year. Black, red and blue fills represent temperatures within the 90th and 10th percentiles, temperatures higher than the 90th percentile, and temperatures lower than the 10th percentiles respectively.
                                    BOTTOM: The total number of observations for each day of the year. These include observations from multiple data sources and over multiple years.")
  
  output$Caption_tab2 <- renderText("TOP: Number of marine temperature anomalies each year.
                                    BOTTOM: The maximum possible number of heat/coldwaves of specified length that can be detected based on the number of missing values in the data. As an example, a value of 80 on the log scale represents 10^80 possibilities for a heat/coldwave.")
  
  output$stationMap_Home <- renderLeaflet({
    stationLocs <- read_ods(paste0(basePath, "NSW-IMOS_assets_2018-10-30.ods"))
    colnames(stationLocs) <- c("site_code", "avg_lat", "avg_lon")
    stationLocs <- stationLocs %>% slice(c(3,6,8,9))
    
    # generate random temperatures based on climatology distributions
    doy  = yday(Sys.Date())
    sd = max((Temp_clim_P90[1,doy] - Temp_clim_mean[1,doy])/1.28, (Temp_clim_mean[1,doy] - Temp_clim_P10[1,doy])/1.28, na.rm = T)
    rTemps <- rnorm(4, mean = Temp_clim_mean[1,doy], sd = sd)
    
    # determine background-color based on climatology and rTemps
    rBG <- ifelse(rTemps < Temp_clim_P10[1, doy], "rgba(0,0,255,0.5)", 
                  ifelse(rTemps <= Temp_clim_P90[1, doy], "rgba(0,255,0,0.5)", "rgba(255,0,0,0.5)"))
    
    leaflet() %>% addTiles() %>% addMarkers(data = stationLocs %>% filter(site_code == "CH100"), lat = ~avg_lat, lng = ~avg_lon, 
                                            label = HTML(paste(sep = "<br/>", stationLocs %>% filter(site_code == "CH100") %>% select(site_code), paste(round(rTemps[1],1), "degrees"))),
                                            labelOptions = labelOptions(noHide = T, direction = "bottom",
                                                                        style = list("background-color" = rBG[1]))) %>%
      addMarkers(data = stationLocs %>% filter(site_code == "SYD100"), lat = ~avg_lat, lng = ~avg_lon, 
                 label = HTML(paste(sep = "<br/>", stationLocs %>% filter(site_code == "SYD100") %>% select(site_code), paste(round(rTemps[2],1), "degrees"))),
                 labelOptions = labelOptions(noHide = T, direction = "right",
                                             style = list("background-color" = rBG[2]))) %>%
      addMarkers(data = stationLocs %>% filter(site_code == "PH100"), lat = ~avg_lat, lng = ~avg_lon, 
                 label = HTML(paste(sep = "<br/>", 
                                    a(paste(stationLocs %>% filter(site_code == "PH100") %>% select(site_code)), onclick = "openTab('PH100_Clim')", href="#"),
                                    paste(round(rTemps[3],1), "degrees"))),
                 labelOptions = labelOptions(noHide = T, direction = "bottom",
                                             style = list("background-color" = rBG[3],
                                                          "pointer-events" = "auto"))) %>%
      addMarkers(data = stationLocs %>% filter(site_code == "BMP120"), lat = ~avg_lat, lng = ~avg_lon, 
                 label = HTML(paste(sep = "<br/>", stationLocs %>% filter(site_code == "BMP120") %>% select(site_code), paste(round(rTemps[4],1), "degrees"))),
                 labelOptions = labelOptions(noHide = T, direction = "bottom",
                                             style = list("background-color" = rBG[4])))
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
