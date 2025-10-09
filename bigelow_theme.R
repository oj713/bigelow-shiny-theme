library(bslib)

#' Creates and returns a Bigelow-style theme object for R Shiny.
bigelow_theme <- function() {
  
  # Basic theme object built using bslib. 
  theme_obj <- bslib::bs_theme(
    version = 5, # Bootstrap refernece version
    # Controls the default grayscale palette
    bg = "white", fg = "#444",
    # Controls the accent (e.g., hyperlink, button, etc) colors
    primary = "#02A5DD", secondary = "#1E4E7B",
    success = "#81AB1F", info = "#02A5DD",
    danger = "#D83838",
    # Local = TRUE stores the font to local cache, but just in case we specify a fallback font
    base_font = font_collection(font_google("Open Sans", wght = "300..800", local = TRUE), "Roboto", "sans-serif"), 
    code_font = c("Courier", "monospace"),
    heading_font = font_collection(font_google("Open Sans", wght = "300..800"), "Roboto", "sans-serif"), 
    # Additional bootstrap variables we're overriding
    "input-border-color" = "#CCCCCC",
    "border-radius" = "0",
    # Card defaults
    "card-border-radius" = "0",
    "card-inner-border-radius" = "0",
    # Navbar defaults
    "border-color" = "#CCC",
    "nav-tabs-link-active-color" = "#1E4E7B"
  )
  
  # Additional CSS specifies custom rules for classes, additional variables, and styles
  cssPath <- "/mnt/ecocast/projects/students/ojohnson/bigelow-shiny-theme/bigelowStyles.css"
  bigelow_css <- readChar(cssPath, nchars = file.info(cssPath)$size)
  
  theme_obj |>
    bs_add_rules(bigelow_css)
}

#' Creates and returns a Bigelow-style header for an R Shiny application.
#' @param left_hand, div, material to have on left hand side of footer.
#'  If this is pure text it is parsed to a header element
#' @param right_hand, div, material to have on right hand side of footer or NULL
#' @return div, footer element with bigelow logo.
bigelow_header <- function(left_hand, right_hand = NULL) {
  if (class(left_hand) == "character") {left_hand <- h2(left_hand)}
  if (is.null(right_hand)) {
    div(class = "bigelow-header", left_hand)
  } else {
    div(class = c("bigelow-header", "flex-justify"), left_hand, right_hand)
  }
}

#' Creates and returns a Bigelow-style main body for an R Shiny application.
#' Basically just gives the main content a slight padding & restricts max width.
#' @param ..., content
#' @return div, content with wrapped styling
bigelow_main_body <- function(...) {
  div(class = "bigelow-main-body", ...)
}

#' Creates and returns a Bigelow-style footer for an R Shiny application.
#' Required to have bigelow_logo.svg in the www/images folder
#' @param left_hand, div, material to have on left hand side of footer
#' @return div, footer element with bigelow logo.
bigelow_footer <- function(left_hand) {
  div(class = c("bigelow-footer", "flex-justify"),
      left_hand,
      img(src='images/bigelow_logo.svg', alt = "Bigelow Laboratory Logo"))
}

#' Creates and returns a Bigelow-style plot card with optional header, main content, 
#' and optional footer
#' @param headerContent content, text to put in header
#' @param footerContent content, text to put in footer
#' @param ... content to put in main body, typically a plot
bigelow_card <- function(..., headerContent = NULL, footerContent = NULL) {
  header_div <- if (is.null(headerContent)) {NULL} else {div(class = "card-header", headerContent)}
  footer_div <- if (is.null(footerContent)) {NULL} else {div(class = "card-footer", footerContent)}
  body <- div(class = "card-body", ...)
  
  arg_list <- Filter(Negate(is.null), list(class = c("card", "bigelow-card"), header_div, body, footer_div))
  
  do.call(div, arg_list)
}




