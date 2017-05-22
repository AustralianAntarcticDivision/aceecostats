## admin install with

#file.copy("inst/workflow/graphics/shiny-habitat-assessment.R", "/srv/shiny-server/habitat-assess/basic/app.R")

## preparation
library(aceecostats)
library(raster)
library(feather)
library(dplyr)
library(ggplot2)
## local path to required cache files
datapath <- "/mnt/acebulk"



 library(ggplot2)
 library(tidyr)
 
 ##db file
 library(dplyr)
 db <- src_sqlite("/mnt/acebulk/habitat_assessment_output.sqlite3")
 
 icegrid <- raadtools::readice() * 0
 sstgrid <- raster(extent(-180, 180, -80, -30), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0", res = 0.25)
 epoch <- ISOdatetime(1970, 1, 1, 0, 0, 0, tz = "GMT")
 #ice_density_tab <- tbl(db, "ice_days_density_tab") %>% collect(n = Inf) %>% mutate(date = date + epoch)  %>% 
#   filter(!Zone == "Mid-Latitude") %>% filter(days > 0, days < 365)
# 
# # ice_sparkline_tab <- tbl(db, "ice_days_sparkline_tab") %>% collect(n = Inf) %>% 
# #   mutate(date = date + epoch) 
# 
 #ice_sparkline_tab_nozone <- tbl(db, "ice_days_sparkline_tab_nozone") %>% collect(n = Inf) %>% 
#   mutate(date = date + epoch)
# 
# 
# 
# spark_data <- ice_sparkline_tab_nozone %>% filter(date > as.POSIXct("1981-02-15"))
 #density_data <-  ice_density_tab %>% filter(days > 0, days < 365)
 
 polymap <- sp::spTransform(aes_zone, projection(icegrid))
 polysstmap <- aes_zone_ll
# gspark <-  ggplot(spark_data, aes(x = date, y = days)) + geom_line() + facet_wrap(~SectorName)
# gdens <- ggplot(density_data, aes(x = days, weights = area,  group = decade, colour = decade)) + 
 #  geom_density() + facet_wrap(~SectorName)  
# 
# # p <- options(warn = -1)  
# # print(gspark + ggtitle("Combined zones"))
# # print(gdens + ggtitle("Combined zones"))
# # par(p)

 zone_names <- c("High-Latitude", "Mid-Latitude", "Continent")
 region_names <- (tbl(db, "ice_days_sparkline_tab_nozone") %>% select(SectorName) %>% distinct() %>% collect())$SectorName
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Habitat assessment"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("region", "Region",  region_names), 
      selectInput("zone", "Zone \n(Continent ice-only, Mid-lat sst-only)", zone_names), 
      selectInput("season", "Season (ignored for ice)", c("Summer","Winter"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
       tabPanel("Sea ice days", plotOutput("ice_sparkPlot"), plotOutput("ice_mapPlot")), 
       tabPanel("SST", plotOutput("sst_sparkPlot"), plotOutput("sstmin_mapPlot"), plotOutput("sstmax_mapPlot"))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$ice_sparkPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- tbl(db, "ice_days_sparkline_tab_nozone") %>% 
      
      filter(SectorName == input$region) %>% collect(n = Inf) %>% 
      mutate(date = date + epoch) %>% 
      filter(date > as.POSIXct("1981-02-15")) 
    #bins <- seq(min(x), max(x), length.out = input$bins + 1)
    gspark <-  ggplot(x, aes(x = date, y = days)) + geom_line() #+ facet_wrap(~Season)
    gspark
  })
  output$ice_mapPlot <- renderPlot({
    pmap <- subset(polymap, SectorName == input$region & Zone == input$zone)
    dens <- tbl(db, "ice_days_density_tab")   %>% 
      filter(!Zone == "Mid-Latitude") %>% 
      filter(days > 0, days < 365) %>% 
      filter(SectorName == input$region, Zone == input$zone) %>% 
      collect(n = Inf) %>% mutate(date = date + epoch)
    if (nrow(dens) < 1) return(ggplot() + ggtitle("no data"))
    dens[c("x", "y")] <- as.data.frame(raster::xyFromCell(icegrid, dens$cell_))
    gmap <- ggplot(dens, aes(x, y, fill = days)) + geom_raster() + facet_wrap(~decade) + coord_equal() + 
      geom_path(data = fortify(pmap), aes(x = long, y = lat, group = group, fill = NULL))
    gmap
  })
  
  output$sst_sparkPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- tbl(db, "sst_sparkline_tab") %>% 
      filter(SectorName == input$region, Zone == input$zone) %>% collect(n = Inf) %>% 
      mutate(date = season_year * 24 * 3600 + epoch, Season = aes_season(date)) %>% 
      gather(measure, sst, -SectorName, -Zone, -season_year, -date, -Season) %>% 
      mutate(measure = gsub("mean", "", measure))
    if (nrow(x) < 1) return(ggplot() + ggtitle("no data"))
    #bins <- seq(min(x), max(x), length.out = input$bins + 1)
    gspark <-  ggplot(x, aes(x = date, y = sst, group = measure, colour = measure)) + geom_line() + facet_wrap(~Season)
    gspark
  })
  
  output$sstmin_mapPlot <- renderPlot({
    
    sstmap <- fortify(subset(polysstmap, SectorName == input$region & Zone == input$zone))
    dens <- tbl(db, "sst_density_tab")   %>% 
     # filter(!Zone == "Mid-Latitude") %>% 
    #  filter(days > 0, days < 365) %>% 
      filter(SectorName == input$region, Zone == input$zone, season == input$season) %>% 
      collect(n = Inf) 
    if (nrow(dens) < 1) return(ggplot() + ggtitle("no data"))
    dens[c("x", "y")] <- as.data.frame(raster::xyFromCell(sstgrid, dens$cell_))
    gmap <- ggplot(dens, aes(x, y, fill = min)) + geom_raster() + facet_wrap(~decade) + coord_equal() + 
      scale_fill_gradientn(colours = palr::sstPal(100)) + 
      geom_path(data = sstmap, aes(x = long, y = lat, group = group, fill = NULL))
    gmap
  })
  
  output$sstmax_mapPlot <- renderPlot({
    
    sstmap <- fortify(subset(polysstmap, SectorName == input$region & Zone == input$zone))
    dens <- tbl(db, "sst_density_tab")   %>% 
      # filter(!Zone == "Mid-Latitude") %>% 
      #  filter(days > 0, days < 365) %>% 
      filter(SectorName == input$region, Zone == input$zone, season == input$season) %>% 
      collect(n = Inf) 
    if (nrow(dens) < 1) return(ggplot() + ggtitle("no data"))
    dens[c("x", "y")] <- as.data.frame(raster::xyFromCell(sstgrid, dens$cell_))
    gmap <- ggplot(dens, aes(x, y, fill = max)) + geom_raster() + facet_wrap(~decade) + coord_equal() + 
      scale_fill_gradientn(colours = palr::sstPal(100)) + 
      geom_path(data = sstmap, aes(x = long, y = lat, group = group, fill = NULL))
    gmap
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

