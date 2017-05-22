


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
 epoch <- ISOdatetime(1970, 1, 1, 0, 0, 0, tz = "GMT")
 ice_density_tab <- tbl(db, "ice_days_density_tab") %>% collect(n = Inf) %>% mutate(date = date + epoch)  %>% 
   filter(!Zone == "Mid-Latitude") 
# 
# # ice_sparkline_tab <- tbl(db, "ice_days_sparkline_tab") %>% collect(n = Inf) %>% 
# #   mutate(date = date + epoch) 
# 
 ice_sparkline_tab_nozone <- tbl(db, "ice_days_sparkline_tab_nozone") %>% collect(n = Inf) %>% 
   mutate(date = date + epoch)
# 
# 
# 
 spark_data <- ice_sparkline_tab_nozone %>% filter(date > as.POSIXct("1981-02-15"))
 density_data <-  ice_density_tab %>% filter(days > 0, days < 365)
 
 polymap <- sp::spTransform(aes_zone, projection(icegrid))
# gspark <-  ggplot(spark_data, aes(x = date, y = days)) + geom_line() + facet_wrap(~SectorName)
# gdens <- ggplot(density_data, aes(x = days, weights = area,  group = decade, colour = decade)) + 
 #  geom_density() + facet_wrap(~SectorName)  
# 
# # p <- options(warn = -1)  
# # print(gspark + ggtitle("Combined zones"))
# # print(gdens + ggtitle("Combined zones"))
# # par(p)

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Sea Ice days"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("region", "Region", unique(spark_data$SectorName)), 
      selectInput("zone", "Zone", unique(density_data$Zone))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"), 
      plotOutput("mapPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- spark_data %>% filter(SectorName == input$region)
    #bins <- seq(min(x), max(x), length.out = input$bins + 1)
    gspark <-  ggplot(x, aes(x = date, y = days)) + geom_line() #+ facet_wrap(~Season)
    gspark
  })
  output$mapPlot <- renderPlot({
    pmap <- subset(polymap, SectorName == input$region & Zone == input$zone)
    dens <- density_data %>% filter(SectorName == input$region, Zone == input$zone)
    if (nrow(dens) < 1) return(ggplot() + ggtitle("no data"))
    dens[c("x", "y")] <- as.data.frame(raster::xyFromCell(icegrid, dens$cell_))
    gmap <- ggplot(dens, aes(x, y, fill = days)) + geom_raster() + facet_wrap(~decade) + coord_equal() + 
      geom_path(data = fortify(pmap), aes(x = long, y = lat, group = group, fill = NULL))
    gmap
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

