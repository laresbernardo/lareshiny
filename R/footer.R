####################################################################
#' Brand Customize for Shiny's Header
#' 
#' This function personalizes your Shiny dashboard's header with logo, 
#' links, favicon, font, and texts.
#' 
#' @param company,site Character. Your company's name and URL
#' @param helpmail Character. Developers mail
#' @param type Integer. Which footer type to use? Needs further developments
#' @export
custom_footer <- function(company = "MyCoolCompany", 
                          site = "https://github.com/laresbernardo/lareshiny", 
                          helpmail = "myemail@mydomain.com", 
                          type = 2) {
  if (type == 1)
    tags$footer(
      HTML(paste("ShinyDashboard ©", lubridate::year(Sys.Date()),
            a(company, href = site, target = "_blank"), "|", 
            a("Contact", href = paste0("mailto:", helpmail)))),
      align = "center")
  if (type == 2)
    tags$footer(
      HTML(paste("ShinyDashboard ©", lubridate::year(Sys.Date()),
            a(company, href = site, target = "_blank"), "|", 
            a("Contacto", href = paste0("mailto:", helpmail)))),
      align = "center")
}
