## admin install with
# if (FALSE) {
# appfile <- "inst/workflow/graphics/shiny-chl-assessment.R"
# srvfile <- "/srv/shiny-server/habitat-assess/variables/chl/app.R"
# file.copy(appfile,  srvfile, overwrite = TRUE)
# 
# }

## test mode
input <- list(region = "Atlantic", zone = "Mid-Latitude", season = "Spring")
input$coord1 <- TRUE
## preparation
library(aceecostats)
library(raster)
library(dplyr)
library(ggplot2)

library(tidyr)
library(DT)
dp <- "/home/acebulk/data"
db <- dplyr::src_sqlite(file.path(dp, "habitat_assessment.sqlite3"))

jet.colors <-
  colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                     "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
reproj_tibble <- function(x, ingrid, outgrid) {
  dcells <- distinct(x, cell_)
  outcell <- cellnumbers(outgrid, spTransform(xyFromCell(ingrid, dcells$cell_, spatial=TRUE), projection(outgrid)))
}
lat.labs<- function(the.proj="polar"){
  if(the.proj=="latlon"){
    ext <- extent(aes_zone_ll)
    text("Polar", x=ext@xmin, y=ext@ymin, xpd=NA, pos=2, cex=0.6)
    text("High latitude", x=ext@xmin, y=ext@ymin*0.8, xpd=NA, pos=2, cex=0.6)
    text("Mid latitude", x=ext@xmin, y=ext@ymin*0.6, xpd=NA, pos=2, cex=0.6)
  }
  if(the.proj=="polar"){
    text(c("Polar", "High latitude", "Mid latitude"), x=c(113064.6,-1017581.1,-3642294), y=c(-1518296,-2285519,-3012363), cex=0.5, col=rgb(0,0,0,0.7))
  }
}
icegrid <- raster(extent(-3950000, 3950000, -3950000, 4350000), 
crs = "+proj=stere +lat_0=-90 +lat_ts=-70 +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs", 
res = 25000)

sstgrid <- raster(extent(-180, 180, -80, -30), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0", res = 0.25)
epoch <- ISOdatetime(1970, 1, 1, 0, 0, 0, tz = "GMT")

polymap <- sp::spTransform(aes_zone, projection(icegrid))
polysstmap <- aes_zone_ll


zone_names <- c("High-Latitude", "Mid-Latitude", "Continent")
region_names <- (tbl(db, "ice_days_sparkline_tab_nozone") %>% dplyr::select(SectorName) %>% distinct() %>% collect())$SectorName
library(shiny)
#rm(input)
# Define UI for application that conquers the universe
ui <- function(request) {
  fluidPage(
    # Application title
    titlePanel("Southern Ocean Ecosystems Habitat Assessment", "Southern Ocean Ecosystems Habitat Assessment"),
    bookmarkButton(),
    # Sidebar with widgey didgets
    sidebarLayout(
      sidebarPanel(
        selectInput("region", "Region",  region_names), 
        selectInput("zone", "Zone ", zone_names), 
        numericInput("min_chl", "Density Min chl (mg/m3)", value = 0, min = 0, max = 1000),
        numericInput("max_chl", "Density Max chl (mg/m3)", value = 30, min = 0, max = 1000),
        textInput("intervals", "CHL mg/m3 Interval Breaks", value = "0.02, 0.27, 0.34, 0.5, 30"),
        
        selectInput("season", "Season", c("Summer", "Winter")), 
        #checkboxInput("interactive", "interactive? (be patient)", FALSE), 
        # selectInput("toggle", "Flip map Polar<->Plate Carrée"),
       # selectInput("palette", "Palette for map", choices = c("chl", "sst", "blues", "ice", "viridis", "inferno", "festival", "baser")), 
      #  selectInput("n_colours", "Number of colours for map", choices = c(3, 7, 11, 13, 25, 50, 100), selected = 11), 
        checkboxInput("coord1", "1:1 aspect ratio? (\"no\" means \"better map layout\")", FALSE), 
      
      checkboxInput("dateline", "merge Pacific at dateline?", TRUE)
      ),
      # Show plots, in tabs
      mainPanel(
        tabsetPanel(
          tabPanel("Chlorophyll-a", plotOutput("chl_sparkPlot"), plotOutput("chl_density"), plotOutput("chl_mapPlot")), 
      #    tabPanel("Sea ice days", plotOutput("ice_sparkPlot"), plotOutput("ice_density"), plotOutput("ice_mapPlot"), DT::dataTableOutput("icerat")), 
        # tabPanel("SST", plotOutput("sst_sparkPlot"), plotOutput("sst_density"), plotOutput("sstmin_mapPlot"), plotOutput("sstmax_mapPlot")), 
          tabPanel("Index map", plotOutput("polar_Map"), plotOutput("ll_Map")),
          tabPanel("Help", htmlOutput("helptext"))
        )
      )
    )
  )
}

# Define server logic required to attain wonderment
server <- function(input, output) {

  get_intervals <- reactive({
    ish <- input$intervals
     as.numeric(unlist(strsplit(ish, ",")))
  })
  ## SPARKY
  output$chl_sparkPlot <- renderPlot({
    x    <- tbl(db, "chl_sparkline_tab") %>% 
      filter(SectorName == input$region, Zone == input$zone, season == input$season) %>% collect(n = Inf) %>% 
      mutate(date = season_year + epoch) %>% gather(measure, chla, -date, -SectorName, -Zone, -season, -season_year)
    
    
    gspark <-  ggplot(x, aes(x = date, y = chla, group = measure, colour = measure)) + geom_line() #+ facet_wrap(~Season)
    #if (input$interactive)    ggplotly(gspark) else gspark
    gspark
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
    #if (input$interactive)    ggplotly(gspark) else gspark
    gspark
  })
  
  output$chl_density <- renderPlot({
    dens <- get_chl_density()
    #dens <- dens %>% gather(measure, chla,  -decade, -bin_num,   -season,  -SectorName, -Zone )
    if (nrow(dens) < 1) return(ggplot() + ggtitle("no data"))
    breaks <- get_intervals()
    ggplot(dens, aes(chla_johnson, group = decade,  colour = decade, weight = area)) + geom_density()  + 
      facet_wrap(~Zone)  + #geom_vline(xintercept = breaks) + 
      xlim(0, 5)
    
  })
  # MAPS
  output$chl_mapPlot <- renderPlot({
    colour_pal <- palr::chlPal(palette = TRUE)
    scl <- function(x) {rng <- range(x, na.rm = T); (x - rng[1])/diff(rng)}
    sstmap <- get_sst_map()
    breaks <- get_intervals()
    dens <- get_chl_density() %>% filter(Zone == input$zone)
    #dens <- dens %>% group_by(decade, bin_num, SectorName, Zone) %>% summarize(mean = mean(chla_johnson))
    if (nrow(dens) < 1) return(ggplot() + ggtitle("no data"))
    dens[c("x", "y")] <- as.data.frame(roc::bin2lonlat(dens$bin_num, 4320))
    
    #dens1 <- sample_n(dens, nrow(dens)/1e3)
    if (input$region == "WestPacific"   & input$dateline) {
      dens <- mutate(dens, x = ifelse(x < 0, x + 360, x))
    }
    gmap <- ggplot(dens, aes(x, y, colour = chla_johnson)) + 
      geom_point(pch = ".") + facet_wrap(~decade) + 
      #scale_fill_gradientn(colours = colour_pal) + 
      scale_colour_gradientn(values = scl(head(colour_pal$breaks, -1)), colours = colour_pal$cols) + 
      geom_path(data = sstmap, aes(x = long, y = lat, group = group, colour = NULL))
    if (input$coord1) gmap <- gmap + coord_equal()
    gmap
  })
  
  ## INDEX MAPS
  output$polar_Map <- renderPlot({
    labs <- data.frame(x= c(112406,4488211,-1734264,-4785284), y=c(4271428,-224812,-3958297,-104377), labels=c("Atlantic","Indian", "West Pacific", "East Pacific"))
    labs <- SpatialPointsDataFrame(labs[,1:2],labs, proj4string = CRS(proj4string(aes_zone)))
    plot(aes_zone, col = aes_zone$colour, border="grey")
    text(labs$x, labs$y, labs$labels, cex=0.6)
    # latitude zone labels
    lat.labs()
  })
  output$ll_Map <- renderPlot({
    labs <- data.frame(x= c(112406,4488211,-1734264,-4785284), y=c(4271428,-224812,-3958297,-104377), labels=c("Atlantic","Indian", "West Pacific", "East Pacific"))
    labs <- SpatialPointsDataFrame(labs[,1:2],labs, proj4string = CRS(proj4string(aes_zone)))
    plot(aes_zone_ll, col = aes_zone_ll$colour, border="grey")
    ll_labs <- spTransform(labs, proj4string(aes_zone_ll))
    text(ll_labs$x, ll_labs$y, labels=labs$labels, cex=0.6)
    lat.labs("latlon")
  })
  
  
  
  get_chl_density <- reactive({
   tbl(db, "chl_johnson_tab")  %>%   
      filter(season == input$season, 
           #  Zone == input$zone, 
             SectorName == input$region) %>% 
      mutate(area = (4600 * 4600)/1e6) %>% 
      collect(n = Inf)
   
    
    })
    
  
  get_sst_map <- reactive({
    tab <- fortify(subset(polysstmap, SectorName == input$region & Zone == input$zone))

    if (input$region == "WestPacific"  & input$dateline) {
      tab <- tab %>% mutate(long = ifelse(long < 0, long + 360, long))
    }
    tab
      })
  
  ## UTILS
  cpal <- reactive({
    switch(input$palette, 
           blues = scales::seq_gradient_pal( "#132B43", "#56B1F7", "Lab")(seq(0, 1, length.out = as.integer(as.integer(input$n_colours)))), 
           sst = palr::sstPal(as.integer(input$n_colours)), 
           ice = palr::icePal(as.integer(input$n_colours)),
           viridis = viridis::viridis(as.integer(input$n_colours)), 
           inferno = viridis::inferno(as.integer(input$n_colours)), 
           festival = jet.colors(as.integer(input$n_colours)), 
           baser = rep(palette(), length = as.integer(input$n_colours)))
  })
  
  
  output$helptext <- renderUI({
    lapply(c("Australian Antarctic Division and the Antarctic Climate and Ecosystems Cooperative Research Centre, Hobart", 
             "Explore assessment variables on each tab", "", 
             "Bookmark a particular view with the \"Bookmark\" button, share a link to what you see",
             "", "",
             "Use the controls to specify combinations of region, zone, season and options for the maps",
             "Region: assessment region, one of four radial southern ocean sectors", 
             "Zone: mid-latitude, high-latitude, continent (the continental shelf around Antarctica)", 
             "Season: Summer or Winter (southern hemisphere), not relevant for sea ice days which is calculated within annual window from 15 February",
             "Palette: only controls the fill gradient for the map",
             "Number of colours for map: coarse control over the colour interpolation",
             "1:1 aspect ratio: toggle this for the map, defaults to equal coordinate spacing in x-y, setting to off can improve the layout",
             "", "", "Region on the map is plotted along with polygon boundary,", 
             "sst lumps high-latitude and continental shelf together, so the region boundaries aren't always exclusive", 
             "ice distinguishes continent and high-latitude, but ignores mid-latitudes", "Source code for this app is here:", 
             "", "",
             "https://github.com/AustralianAntarcticDivision/aceecostats/blob/master/inst/workflow/graphics/shiny-habitat-assessment.R", 
             "Please file feedback in the repo's issues tab: https://github.com/AustralianAntarcticDivision/aceecostats/issues", 
             "TODO: ", 
             "- chlorophyll", 
             "- make absolute fill scale for min/max SST",
             "- flip between map projections", 
             "- plotly", 
             "- ..."), tags$p)
  })
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")

