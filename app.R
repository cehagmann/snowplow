# 
# SYRACUSE SNOWPLOW TRACKING - CARL HAGMANN - MARCH 1. 2018
# 
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(foreach)
library(doParallel)
doParallel::registerDoParallel(4)
library(plyr); library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(chron)
library(hexbin)
library(purrr)
library(ggmap)
library(gganimate)
library(DT)

# Load data
sp2 <- readRDS("sp2.rds")
sp2$truck_name=as.factor(sp2$truck_name)
sp2$diff_days <- as.factor(floor_date(sp2$dates_times,"day"))
sp2$hour <- as.integer(period_to_seconds(hms(sp2$onlytime))/3600)

# Which dates to select from (the ones with data)
d = factor(unique(sp2$diff_days)[c(1:7,9:13)])
map_dates = sp2[sp2$diff_days %in% d,c(2,4:9,18,20,21)]
empty_activity_idx=which(map_dates$activity_type == "    " | map_dates$activity_type == "" )
map_dates$activity_type[empty_activity_idx] <- ""
load("syr_mapz13.rda") 

ui <- fluidPage(
  # Some custom CSS for a smaller font for preformatted text
  tags$head(
    tags$style(HTML("
                    pre, table.table {
                    font-size: smaller;
                    }
                    "))
  ),
  
  # Application title
  titlePanel("Syracuse Plowing"),
  
  wellPanel(
    selectInput('truck_name', 'Truck Number', unique(sp2$truck_name), multiple = T, selected = 265),
    checkboxInput("alltrucks", "Select all trucks", value = TRUE),
    selectInput('diff_days', 'Date', unique(sp2$diff_days), multiple = T, selected = unique(sp2$diff_days)[[4]]),
    sliderInput('time24', 'Time (hours since midnight):', min=0, max=24,value=9:15, step = 1)#,
  ),
  helpText("Click and move your cursor in the map on the left to zoom in on the map on the right. Click a point in the right-side map to show information about nearby points"),
  # Show a plot of the map with selected truck route and day
  fluidRow(
    column(width = 5,
           plotOutput("map", height = 500,
                      click = "map_click",
                      hover = hoverOpts(id="map_hover", delayType = "throttle"),
                      brush = brushOpts(
                        id = "map_brush",
                        resetOnNew = TRUE
                      )
           )
           
    ),
    
    column(width = 5,
           plotOutput("map2", height = 500,
                      click = "map2_click",
                      brush = brushOpts(
                        id = "map2_brush",
                        resetOnNew = TRUE
                      ))
    )
  ),
  
  fluidRow(
    column(width = 12,
           h4("Observations near click"),
           dataTableOutput("click_info"),
           downloadButton(outputId = "mydownload", label = "Download Table")
           
    )
  )
)




# Define server logic 
server <- function(input, output, session) {
  ranges2 <- reactiveValues(x = NULL, y = NULL)
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    dat = map_dates[map_dates$diff_days %in% input$diff_days & map_dates$truck_name %in% input$truck_name,]
    mintime=input$time24[1]
    maxtime=input$time24[2]
    dat[dat$hour>mintime & dat$hour<maxtime,]
  })
  
  output$mydownload <- downloadHandler(
    filename = "snowplow_extract.csv",
    content = function(file) {
      write.csv(selectedData(), file)})
  
  output$map <- renderPlot({
    
    ggmap(syr13) +
      geom_point(data=selectedData(), aes(x=longitude, y=latitude, color = diff_days, shape = activity_type)) +
      scale_shape_manual(values = 1:26)
  }) 
  
  output$map_w_overlay <- renderPlot({
    
    ggmap(syr13) +
      geom_point(data=selectedData(), aes(x=longitude, y=latitude, color = diff_days, shape = activity_type)) +
      scale_shape_manual(values = 1:26) +
      coord_cartesian() + annotation_raster(w,xmin = -76.202, xmax = -76.085, ymin = 42.985, ymax=43.105)
    
  })
  # Then plot truck paths in zoomed plot
  output$map2 <- renderPlot({
    ggmap(syr13) +
      geom_point(data=selectedData(), aes(x=longitude, y=latitude, shape = activity_type)) +
      scale_shape_manual(values = 1:26) +
      geom_path(data=selectedData(), aes(x=longitude, y=latitude, color = hour, group = interaction(diff_days, truck_name))) +
      scale_color_gradient(low="red", high="green") +
      coord_cartesian(xlim = ranges2$x, ylim = ranges2$y) #+
  })
  
  observe({
    brush <- input$map_brush
    if (!is.null(brush)) {
      ranges2$x <- c(brush$xmin, brush$xmax)
      ranges2$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges2$x <- NULL
      ranges2$y <- NULL
    }
    
    #if (input$alltrucks) tn <- unique(sp2$truck_name)
    updateSelectInput(session, "truck_name", choices = unique(sp2$truck_name), selected = if (input$alltrucks) unique(sp2$truck_name))
  })
  
  output$click_info <- renderDataTable({
    # nearPoints(selectedData()[, c(8,9,2,4,5,6,7,18,20)], input$map2_click, xvar="longitude", yvar = "latitude", addDist = FALSE,maxpoints = 10, threshold = 10)
    nearPoints(selectedData(), input$map2_click, xvar="longitude", yvar = "latitude", addDist = FALSE,maxpoints = 100, threshold = 20)
  })
  output$brush_info <- renderPrint({
    brushedPoints(map_dates[, 8:9], input$map_brush)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

