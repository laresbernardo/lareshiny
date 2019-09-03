####################################################################
#' Custom Header: Customize for Shiny's Header
#' 
#' This function personalizes your Shiny dashboard's header with logo, 
#' links, favicon, font, and texts.
#' 
#' @param title Character. Your Shinys title (displayed in Navigators' tab)
#' @param site Character. URL for your site (opens when logo is clicked)
#' @param favicon Character. Image for your favicon. Save file in www directory
#' @param font Character. Font for whole Shiny app. Use Google Fonts names
#' @param logosrc,loadingsrc Character. Logo image and loading image
#' @param height,width Integer. Logo image dimentions
#' @param text Character. Text displayed in top right corner
#' @param type Integer. 1 for complete dashboardHeader results, 
#' 2 for title results
#' @export
custom_header <- function(title = "MyLareShiny", 
                          site = "https://github.com/laresbernardo/lareshiny", 
                          favicon = NULL, 
                          font = "Montserrat", 
                          logosrc = NULL, 
                          loadingsrc = NULL, 
                          height = NULL, 
                          width = NULL,
                          text = Sys.Date(),
                          type = 1) {
  
  aux <- tagList(
    tags$head(
      tags$title(title),
      tags$head(
        # SHINY'S TITLE
        tags$title(title),
        # FAVICON
        tags$link(rel = "shortcut icon", href = favicon),
        # hr() for separators
        tags$style(HTML("hr {border-top: 1px solid #000000}")),
        # FONT
        tags$style(HTML("@import url('https://fonts.googleapis.com/css?",
                        "family = ", font, "');*{font-family: ", font, ";}")),
        # UPPER RIGHT CORNER
        tags$style(HTML('.myClass { font-size: 16px; line-height: 50px; text-align: right;',
                        'padding: 0 15px; overflow: hidden; color: white;}'))),
      
      # LOADING IMAGE
      tags$script(
        "setInterval(function() {
        if ($('html').attr('class')=='shiny-busy') {
        $('div.busy').show();
        $('div.notbusy').hide();
        } else {
        $('div.busy').hide();
        $('div.notbusy').show();
        }},100)")),
    
    # LOGO AND LOADING IMAGES
    # Loading image created with http://www.ajaxload.info/ 
    tags$a(href = site,
           div(class = "busy", img(src = loadingsrc, height = height, width = width)),
           div(class = 'notbusy', img(src = logosrc, height = "40px", width = width))),
    
    # UPPER RIGHT CORNER TEXT
    tags$script(HTML('$(document).ready(function() {',
                     '$("header").find("nav").append(\'<div id="pageHeader" class="myClass">', 
                     as.character(text), '</div>\');})')))
  
  if (type == 1)
    return(dashboardHeader(title = aux))
  if (type == 2)
    return(aux)
}