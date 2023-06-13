####################################################################
#' Custom Footer: Customize for Shiny's Footer
#' 
#' This function personalizes your Shiny dashboard's footer with your
#' company's name, link, help mail.
#' 
#' @param company,text,site Character. Your company's name and URL and
#' the project's name to show in footer.
#' @param helpmail Character. Developers mail
#' @param type Integer. Which footer type to use? Needs further developments.
#' @returns A tags$footer formatted object
#' @export
custom_footer <- function(company = "MyCoolCompany", 
                          text = "ShinyDashboard",
                          site = "https://github.com/laresbernardo/lareshiny", 
                          helpmail = "myemail@mydomain.com", 
                          type = 2) {
  if (type == 1)
    tags$footer(
      HTML(paste(text, "&#169;", lubridate::year(Sys.Date()),
                 a(company, href = site, target = "_blank"), "|", 
                 a("Contact", href = paste0("mailto:", helpmail)))),
      align = "center")
  if (type == 2)
    tags$footer(
      HTML(paste(text, "&#169;", lubridate::year(Sys.Date()),
                 a(company, href = site, target = "_blank"), "|", 
                 a("Contacto", href = paste0("mailto:", helpmail)))),
      align = "center")
}
