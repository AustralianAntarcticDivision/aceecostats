# ## admin install with
#  if (FALSE) {
#  appfile <- "inst/workflow/graphics/seaice-live/app.R"
#  srvfile <- "/srv/shiny-server/seaice-live/app.R"
# file.copy(appfile,  srvfile, overwrite = TRUE)
# 
# }


library(shiny)
library(raadtools)


library(viridis)
library(palr)
## housekeeping
fs <- icefiles()
maxdate <- as.Date(max(fs$date))
mindate <- as.Date(min(fs$date))
firstdate <- maxdate - 10 * 365.25 

# Define UI for application that draws a histogram
ui <- function(request) {
  fluidPage(
  # Application title
  headerPanel("NSIDC daily sea ice concentration"),
  bookmarkButton(),
  # date input
  sidebarPanel(
    dateRangeInput("dateRange",
                   "Dates to query:",
                   start = firstdate,
                   end = maxdate,
                   min = mindate,
                   max = maxdate,
                   separator = "and"), 
    shiny::selectInput("palette", label = "palette", choices = c("viridis", "inferno", "nsidc", "grey"), selected = "viridis"), 
    shiny::checkboxInput("reversepal", "Reverse palette?")
    
  ),
  
  
  # Show the plot
  mainPanel(
    plotOutput("rasterPlot")
  ))
}

# Define server logic required to draw a histogram
server <- function(input, output) {
  ## plot two dates, chosen by the user
  output$rasterPlot <- renderPlot({
    dates <- input$dateRange
    x <- readice(dates)
    names(x) <- format(getZ(x), "NSIDC_%d_%b_%Y")
    pal <- switch(input$palette, 
                  viridis = viridis::viridis(99), 
                  inferno = viridis::inferno(99), 
                  nsidc = palr::icePal(99), 
                  grey = grey(seq(0, 1, length = 100)[-1]))
    if (input$reversepal) pal <- rev(pal)
    plot(x, col = pal, zlim = c(0, 100), axes = FALSE)

  })
  
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")

