####################################################################
#' Custom Footer: Customizes for Shiny's Footer
#'
#' This function personalizes your Shiny dashboard's footer with your
#' company's name, link, help mail.
#'
#' @param company,text,site Character. Your company's name and URL and
#' the project's name to show in footer.
#' @param helpmail Character. Developers mail
#' @param lang Character. Language for the text.
#' @returns A tags$footer formatted object
#' @export
custom_footer <- function(company = "MyCoolCompany",
                          text = "ShinyDashboard",
                          site = "https://github.com/laresbernardo/lareshiny",
                          helpmail = "myemail@mydomain.com",
                          lang = "en") {
  contact <- "@"
  if ("en" %in% lang) contact <- "Contact"
  if ("es" %in% lang) contact <- "Contacto"
  tags$footer(
    HTML(paste(
      text, "&#169;", format(Sys.Date(), format = "%Y"),
      a(company, href = site, target = "_blank"), "|",
      a(contact, href = paste0("mailto:", helpmail))
    )),
    align = "center", class = "footer"
  )
}
