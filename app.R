# Creates an example Shiny website highlighting supported features of the Bigelow Shiny theme object

suppressPackageStartupMessages({
  library(shiny)
  library(ggplot2)
  library(dplyr)
  library(leaflet)
})

# Import the bigelow theming materials
source("bigelow_theme.R")

s <- storms |>
  mutate(date = as.POSIXct(sprintf("%s-%s-%s %s:00", year, month, day, hour))) 

ui <- fluidPage(
  includeCSS("www/additionalStyles.css"),
  theme = bigelow_theme(),
  # Header
  bigelow_header(h2("Bigelow Shiny Theme"),
                 h6(class = "custom-theming", "Tutorial")),
  # Main content``
  bigelow_main_body(
    p("The Bigelow Shiny theme is a package that provides pre-built theming for shiny components as well as a handful of custom functions for structing your Shiny application. This demo app highlights key functionality and supported features."),
    p("To add Bigelow Theming to your Shiny application, add ", code("theme = bigelowShiny::bigelow_theme()"), "as the first argument to your ui object."),
    selectInput("storm_choice",
                "Choose a storm to plot",
                choices=sort(unique(s$name)),
                selected="Hugo"),
    checkboxInput("use_custom_icon", "Use custom icons?", value = FALSE),
    # navset_bar - Full width navigation bar
    navset_bar(
      title = "Navigation Bar",
      nav_panel("Nav 1", 
                fluidRow(height = "90vh",
                         column(width = 6,
                                bigelow_card(footerContent = "HELLO", headerContent = NULL, 
                                             plotOutput("wind_plot"))),
                         column(width = 6,
                                bigelow_card(headerContent = "Track of Storm",
                                             leafletOutput("storm_track")))
                )),
      nav_panel("Nav 2",
                navlistPanel(
                  "Header",
                  tabPanel("First", dateInput("dateSelect", "Choose a date (nonfunctional)")),
                  tabPanel("Second", "Different content"),
                  tabPanel("Third", "More Content")
                )),
      nav_spacer(),
      nav_menu("Dropdown",
               nav_panel("Sub 1", p("Dropdown item 1")),
               nav_panel("Sub 2", p("Dropdown item 2")),
               nav_item(tags$a("External Link", href = "#", target = "_blank"))
      )
    )
  ),
  # Footer with bigelow logo
  bigelow_footer("Shiny Theming Tutorial")
)

server <- function(input, output, session) {
  
  stormdata <- reactive({s |>
    filter(name == input$storm_choice) |>
    arrange(date)})
  
  output$wind_plot <- renderPlot({
    ggplot(stormdata(), aes(x=date)) + 
      geom_line(aes(y=wind)) 
  })
  
  output$storm_track <- renderLeaflet({
    data <- stormdata()
    
    addVariableMarkers <- function(leafletobj) {
      if (input$use_custom_icon) {
        addRecordMarkers(leafletobj, ~long, ~lat)
      } else {
        addCircleMarkers(leafletobj, ~long, ~lat, radius = 5, color = "red", fillOpacity = 0.8)
      }
    }
    
    leaflet(data = data) |>
      addTiles() |>
      addVariableMarkers()
  })
  
}

shinyApp(ui, server)
