library(shiny)
library(bslib)

source("bigelow_theme.R")

ui <- fluidPage(
  theme = bigelow_theme(),
  bigelow_header("Bigelow UI Inputs & Nav Showcase", 
                 p("Right Click + 'Inspect' to see details")),
  bigelow_main_body(
  fluidRow(
    selectInput("storm_choice",
                "Choose a storm to plot",
                choices=c("Hugo", "Charlie"),
                selected="Hugo"),
    sliderInput("sliderthing", "help", 5, 10, 7),
    radioButtons("buttonsradio", "choices", choices = c("one", "two")),
    checkboxInput(inputId = "show_data",
                  label = "Show data table",
                  value = TRUE),
    dateInput("dateinput", "date"),
    submitButton(text = "jdsfdk"),
    textInput("hello my name is", label = "jdsk"),
    fileInput("file here", "jdsfkk")
  ),
  
  # Standard navset_tab
  card(
    card_header("navset_tab - Standard tabs"),
    navset_tab(
      nav_panel("Tab 1", p("Standard tab panel content 1")),
      nav_panel("Tab 2", p("Standard tab panel content 2")),
      nav_panel("Tab 3", p("Standard tab panel content 3"))
    )
  ),
  
  br(),
  
  # navset_pill
  card(
    card_header("navset_pill - Pill-style navigation"),
    navset_pill(
      nav_panel("Pill 1", p("Pill navigation content 1")),
      nav_panel("Pill 2", p("Pill navigation content 2")),
      nav_panel("Pill 3", p("Pill navigation content 3"))
    )
  ),
  
  br(),
  
  # navset_underline
  card(
    card_header("navset_underline - Underlined tabs"),
    navset_underline(
      nav_panel("Under 1", p("Underlined tab content 1")),
      nav_panel("Under 2", p("Underlined tab content 2")),
      nav_panel("Under 3", p("Underlined tab content 3"))
    )
  ),
  
  br(),
  
  # navset_card_tab
  card(
    card_header("navset_card_tab - Card-style tabs"),
    navset_card_tab(
      nav_panel("Card 1", p("Card tab content 1")),
      nav_panel("Card 2", p("Card tab content 2")),
      nav_panel("Card 3", p("Card tab content 3"))
    )
  ),
  
  br(),
  
  # navset_card_pill
  card(
    card_header("navset_card_pill - Card with pills"),
    navset_card_pill(
      nav_panel("Card Pill 1", p("Card pill content 1")),
      nav_panel("Card Pill 2", p("Card pill content 2")),
      nav_panel("Card Pill 3", p("Card pill content 3"))
    )
  ),
  
  br(),
  
  # navset_card_underline
  card(
    card_header("navset_card_underline - Card with underlined tabs"),
    navset_card_underline(
      nav_panel("Card Under 1", p("Card underlined content 1")),
      nav_panel("Card Under 2", p("Card underlined content 2")),
      nav_panel("Card Under 3", p("Card underlined content 3"))
    )
  ),
  
  br(),
  
  # navset_bar - Full width navigation bar
  navset_bar(
    title = "Full Width Navigation Bar",
    nav_panel("Nav 1", p("Full width nav bar content 1")),
    nav_panel("Nav 2", p("Full width nav bar content 2")),
    nav_spacer(),
    nav_menu("Dropdown",
             nav_panel("Sub 1", p("Dropdown item 1")),
             nav_panel("Sub 2", p("Dropdown item 2")),
             nav_item(tags$a("External Link", href = "#", target = "_blank"))
    )
  ),
  
  br(),
  
  # Partial width navset_bar
  div(style = "width: 60%; margin: 0 auto;",
      card(
        card_header("navset_bar - Partial width (60%)"),
        navset_bar(
          title = "Partial Width Bar",
          nav_panel("Partial 1", p("Partial width bar content 1")),
          nav_panel("Partial 2", p("Partial width bar content 2")),
          nav_spacer(),
          nav_panel("Right", p("Right-aligned content"))
        )
      )
  ),
  
  br(),
  
  # navset_hidden for programmatic navigation
  card(
    card_header("navset_hidden - Programmatically controlled"),
    actionButton("switch_tab", "Switch Hidden Tab"),
    br(), br(),
    navset_hidden(
      id = "hidden_tabs",
      nav_panel("hidden1", p("Hidden tab content 1 - controlled by button")),
      nav_panel("hidden2", p("Hidden tab content 2 - controlled by button"))
    )
  ),
  
  br(),
  
  # Accordion-style navigation
  card(
    card_header("accordion - Collapsible panels"),
    accordion(
      accordion_panel("Section 1", p("Accordion content 1 - collapsible section")),
      accordion_panel("Section 2", p("Accordion content 2 - collapsible section")),
      accordion_panel("Section 3", p("Accordion content 3 - collapsible section"))
    )
  ),
  
  br(),
  
  # Complex example with nav_menu and nav_item
  card(
    card_header("Complex navigation with menus and items"),
    navset_pill(
      nav_panel("Home", p("Home page content")),
      nav_panel("About", p("About page content")),
      nav_menu("Tools",
               nav_panel("Tool 1", p("First tool content")),
               nav_panel("Tool 2", p("Second tool content")),
               "----",
               nav_item(tags$a("Documentation", href = "#"))
      ),
      nav_spacer(),
      nav_item(actionButton("btn", "Action Button", class = "btn-primary"))
    )
  ),
  
  br(),
  
  # Sidebar-style navigation examples
  h2("Sidebar-Style Navigation"),

  navlistPanel(
    "Header",
    tabPanel("First", "hello"),
    tabPanel("Second", "hllo hter"),
    tabPanel("Third", "tjdks")
  )
  ),
  bigelow_footer("Updated October 2025.")
)

server <- function(input, output, session) {
  # Handle hidden tab switching
  observeEvent(input$switch_tab, {
    current <- if (is.null(input$hidden_tabs) || input$hidden_tabs == "hidden1") {
      "hidden2"
    } else {
      "hidden1"
    }
    nav_select("hidden_tabs", current)
  })
}

shinyApp(ui = ui, server = server)
