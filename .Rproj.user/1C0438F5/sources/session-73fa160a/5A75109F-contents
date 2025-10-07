library(bslib)

#' Creates and returns a Bigelow-style theme object for R Shiny.
bigelow_theme <- function() {
  # Custom CSS as text. Specifying color variables & basic theming elements, classes
  bigelow_css <- "
    :root {
      --bg: white;
      --bg-accent: #EAF2F6;
      --fg: #444;
      --fg-light: #CCC;
      --primary: #02A5DD;
      --secondary: #1E4E7B;
      --tertiary: #81AB1F;
      --danger: #D83838;
      --danger-accent: #901515;
    }
    
    body {
      font-size: 14px;
      line-height: 1.6;
    }
    
    body > .container-fluid {
      padding: 0;
    }
    
    h1, h2, h3, h4, h5, h6 {
      font-weight: bold;
      color: var(--primary);
    }
    
    .flex-justify {
      display: flex;
      justify-content: space-between;
      align-contents: center;
    }
    
    .header {
      border-bottom: solid 2px var(--fg-light); 
      padding: 1em; 
    }
    .header * {
      margin: 0
    }
    .main-body{
      padding: 1em;
      max-width: 1200px;
      margin: auto;
    }
    .footer {
      background-color: var(--bg-accent); 
      color: var(--secondary);
      padding: 3em; 
    }
    .card {height: 100%;}
    .card-header, .card-footer {
      font-weight: bold; 
      background-color: var(--secondary);
      color: var(--bg);
      height: 2.5em;
      font-size: 1.2em;
      display: flex;
      align-items: center;
    }
    .card-body {
      height: 100%;
      padding: 0;
    }
    
    .form-group label {
      font-weight: bold;
      color: var(--tertiary);
      font-size: 1.2em;
    }
  "
  
  theme_obj <- bslib::bs_theme(
    version = 5,
    # Controls the default grayscale palette
    bg = "white", fg = "#444",
    # Controls the accent (e.g., hyperlink, button, etc) colors
    primary = "#02A5DD", secondary = "#1E4E7B",
    success = "#81AB1F", info = "#02A5DD",
    danger = "#D83838",
    # Local = TRUE stores the font to local cache, but just in case we specify a fallback font
    base_font = font_collection(font_google("Open Sans", wght = "300..800", local = TRUE), "Roboto", "sans-serif"), 
    code_font = c("Courier", "monospace"),
    heading_font = "Open Sans",
    "input-border-color" = "#CCCCCC",
    "card-border-radius" = "0",
    "card-inner-border-radius" = "0"
  )
  
  theme_obj |>
    bs_add_rules(bigelow_css)
}

#' Creates and returns a Bigelow-style header for an R Shiny application.
#' @param left_hand, div, material to have on left hand side of footer
#' @param right_hand, div, material to have on right hand side of footer or NULL
#' @return div, footer element with bigelow logo.
bigelow_header <- function(left_hand, right_hand) {
  if (is.null(right_hand)) {
    div(class = "header", left_hand)
  } else {
    div(class = c("header", "flex-justify"), left_hand, right_hand)
  }
}

#' Creates and returns a Bigelow-style main body for an R Shiny application.
#' Basically just gives the main content a slight padding & restricts max width.
#' @param ..., content
#' @return div, content with wrapped styling
bigelow_main_body <- function(...) {
  div(class = "main-body", ...)
}

#' Creates and returns a Bigelow-style footer for an R Shiny application.
#' Required to have bigelow_logo.svg in the www/images folder
#' @param left_hand, div, material to have on left hand side of footer
#' @return div, footer element with bigelow logo.
bigelow_footer <- function(left_hand) {
  div(class = c("footer", "flex-justify"),
      left_hand,
      img(src='images/bigelow_logo.svg', alt = "Bigelow Laboratory Logo"))
}

#' Creates and returns a Bigelow-style plot card with optional header, main content, 
#' and optional footer
#' @param headerContent content, text to put in header
#' @param footerContent content, text to put in footer
#' @param ... content to put in main body, typically a plot
bigelow_card <- function(headerContent = NULL, footerContent = NULL, ...) {
  header_div <- if (is.null(headerContent)) {NULL} else {div(class = "card-header", headerContent)}
  footer_div <- if (is.null(footerContent)) {NULL} else {div(class = "card-footer", footerContent)}
  body <- div(class = "card-body", ...)
  
  arg_list <- Filter(Negate(is.null), list(class = "card", header_div, body, footer_div))
  
  do.call(div, arg_list)
}
