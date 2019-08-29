####################################################################
#' Export Results from h2o_automl()
#' 
#' Friendly app that helps you export lares' h2o_automl() results
#' 
#' @param model List. Result from h2o_automl()
#' @export
#' @examples
#' if (interactive()) {
#'   model_exporter(model)
#' }
model_exporter <- function(model) {
  
  try_require("lares")
  
  ui <- miniPage(
    gadgetTitleBar(model$model_name, left = NULL),
    miniContentPanel(
      strong(p("Performance summary:")),
      tableOutput("metrics"),
      tableOutput("metrics_other"),
      checkboxGroupInput("checkGroup",
                         label = strong("Select files to export:"),
                         choices = list("Summary in txt file" = 1L,
                                        "CSV file with datasets and scores" = 5L,
                                        "RDS object" = 2L,
                                        "MOJO files (cross-platform friendly)" = 4L,
                                        "Binary model file" = 3L,
                                        "Plots as PNG" = 6L),
                         selected = c(1, 2, 3, 4, 5)),
      hr(),
      strong(p("Current directory:")),
      p(textOutput("directory")), 
      hr(),
      actionButton("run", icon = icon("save"), label = "Generate files")
    )
  )
  
  server <- function(input, output, session) {
    
    output$metrics <- renderTable(model$metrics$metrics) 
    
    output$directory <- renderPrint(getwd())
    
    observeEvent(input$run, {
      withProgress(message = "Generating files...", value = 1, {
        export_results(model, 
                       txt = 1L %in% input$checkGroup,
                       rds = 2L %in% input$checkGroup,
                       binary = 3L %in% input$checkGroup,
                       mojo = 4L %in% input$checkGroup,
                       csv = 5L %in% input$checkGroup,
                       plots = 6L %in% input$checkGroup)  
      })
      sendSweetAlert(session, title = "Done!", type = "success",
                     text = paste("Succesfully exported results for", 
                                  model$model_name, "into", getwd()))
    })
    
    observeEvent(input$done, {
      stopApp(message("Done exporting. Bye!"))
    })
  }
  runGadget(ui, server)
}
