# Lesson 2.0 - Layouts
# Objectives
# 1. Learn how to define the layout of your app in the user interface

suppressPackageStartupMessages({
  library(shiny)
})

ui2 <- fluidPage(
  theme = bigelow_theme(),
  bigelow_header("Hello shiny!"),
  bigelow_main_body(
    sidebarLayout(
      sidebarPanel("sidebar"),
      mainPanel("main panel")
    )
  )
)

# column widths can be between 1-12
ui2 <- fluidPage(
  theme = bigelow_theme(),
  bigelow_header("Fluid Row Columns Layout!"),
  bigelow_main_body(
    fluidRow(
      column(width = 3, "top column 1"),
      column(width = 6, "top column 2", offset=2)
    ),
    fluidRow(
      column(width=12, "bottom column")
    )
  ), 
  bigelow_footer("2025")
)

ui2 <- fluidPage(
  theme = bigelow_theme(),
  bigelow_header("Navbar Page"),
  tabsetPanel(type = "pills",
    tabPanel("Layout 1",
             titlePanel("Sidebar"),
             sidebarLayout(
               sidebarPanel("sidebar"),
               mainPanel("main panel")
             )),
    tabPanel("Layout 2",
             titlePanel("Fluid Rows with Columns"),
             fluidRow(
               column(width = 3, "top column 1"),
               column(width = 6, "top column 2", offset=2)
             ),
             fluidRow(
               column(width=12, "bottom column")
             ))
  ),
  bigelow_footer("Youpity Doo")
)

ui2 <- fluidPage(
  theme = bigelow_theme(), 
  bigelow_navset_bar(
    title = "Title Item",
    nav_spacer(),
    nav_panel(title = "One", p("Content 1")),
    nav_panel(title = "Two", p("Content 2")),
    nav_menu("coelenterates",
             nav_item(p("Hello"), nav_item(p("Hello there"))))
  ), 
  bigelow_footer("2025")
)

server2 <- function(input,output) {}

shinyApp(ui = ui2, server = server2)

# Exercises:
# 1. Add a column to the fluidRow layout