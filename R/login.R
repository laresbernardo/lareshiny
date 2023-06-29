####################################################################
#' Login Module for Shiny
#'
#' Login Module for Shiny with local User and Password. This must be
#' added in the server section (no need to add UI)
#'
#' @param input,session Shiny's input and session objects.
#' @param users,pwds Character Vector. User and password combinations accepted
#' @param logo Character. Select image for logo display. Host local
#' file in your www directory preferably
#' @param logo_height Character. Height for rendering the logo.
#' @param lang Character. Language. Currently accepted: es, en
#' @param style List. Possible values for styling the module such as
#' \code{botton_txt_colour} and \code{botton_bgd_colour}.
#' @param change_text Named list. Change the default texts used.
#' @param logged Boolean. You might want to set to TRUE when developing
#' or testing your app so this module doesn't show up every time.
#' Check the personal parameter as well
#' @param personal Character vector. If you wish to auto-login certain
#' user(s), set the values from \code{Sys.info()[["nodename"]]}.
#' @returns A reactiveValues object
#' @export
# library(shiny)
# if (interactive()) {
#   ui <- fluidPage("Hello world!")
#   server <- function(input, output, session) {
#     login <- module_login(input, session, personal = "")
#     observe({
#       if (login$authenticated)
#         message("We are in!")
#     })
#   }
#   shinyApp(ui, server)
# }
module_login <- function(input, session,
                         users = c("123","321"),
                         pwds = c("123","321"),
                         logo = NA,
                         logo_height = "100px",
                         lang = "es",
                         style = list(
                           "botton_txt_colour" = "#FFFFFF",
                           "botton_bgd_colour" = "#EBB600"),
                         change_text = list(),
                         logged = FALSE,
                         personal = "MacBookBLB.local") {
  
  values <- reactiveValues(authenticated = logged)
  nodename <- Sys.info()[["nodename"]]
  
  # Personal auto-login
  if (Sys.info()[["nodename"]] %in% personal){
    logged <- TRUE
    values$user <- personal[1]
  }
  
  # Run Login Module
  if (!logged) {
    
    # Languages
    dic <- data.frame(rbind(
      c("title","AUTENTICACIÓN","es"),
      c("title","AUTHENTICATION","en"),
      c("user","Usuario","es"),
      c("user","User","en"),
      c("pass","Contraseña","es"),
      c("pass","Password","en"),
      c("enter","Entrar","es"),
      c("enter","Enter","en"),
      c("wrong","Usuario/contraseña incorrecta","es"),
      c("wrong","Incorrect user/password","en"),
      c("wrong_text","Por favor, intenta de nuevo y asegurate que tus credenciales sean los correctos.","es"),
      c("wrong_text","Please, try again with correct user or password.","en")))
    colnames(dic) <- c("term","text","lang")
    dic <- dic[dic$lang == lang, ]
    
    # Rename values with custom texts
    if (length(change_text) > 0) {
      lares::check_opts(names(change_text), dic$term)
      for (term in names(change_text)) {
        dic$text[dic$term == term] <- change_text[[term]]
      }
    }
    
    # Login Modal
    dataModal <- function(failed = FALSE) {
      button_style <- paste("color:", style$botton_txt_colour,
                            "; background-color:", style$botton_bgd_colour,
                            "; border-radius: 6px;")
      modalDialog(if (!is.na(logo)) img(src = logo, height = logo_height, align = "center"),
                  title = dic$text[dic$term == "title"],
                  textInput("username", paste0(dic$text[dic$term == "user"],":"), width = "100%"),
                  passwordInput("password", paste0(dic$text[dic$term == "pass"],":"), width = "100%"),
                  footer = tagList(actionButton("ok", HTML(paste0(
                    '<span>',dic$text[dic$term == "enter"],
                    '</span> <span class="fa fa-chevron-right"></span>')),
                    style = button_style)), size = "m")
    }
    
    # Show Modal as Pop-up
    showModal(dataModal())
    loginvalidator <- observeEvent(input$ok,{
      if (length(users) > 1) {
        pwds <- pwds[users == input$username]
        users <- input$username
      }
      users <- which(users == input$username)
      pwds <- which(pwds == input$password)
      if (length(users) > 0 & length(pwds) > 0) {
        if (users == pwds) {
          values$authenticated <- TRUE
          loginvalidator$suspend()
          removeModal()
        }
      } else {
        # Wrong user/pass
        sendSweetAlert(
          session = session,
          title = dic$text[dic$term == "wrong"],
          type = "error",
          text = dic$text[dic$term == "wrong_text"])
      }
      values$user <- input$username
    })
  } else {
    values$authenticated <- TRUE
  }
  values$timestamp <- Sys.time()
  values$nodename <- nodename
  return(values)
}
