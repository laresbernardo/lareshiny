####################################################################
#' Automatically wrap ggplot2 texts depending on rendered size
#' 
#' This function automatically adapts the length of a title or subtitle
#' text so that it wraps long texts in separate lines. This is useful
#' to avoid trimming long texts when the rendered shiny size is shorter.
#' 
#' @param session Shiny's session object.
#' @param text Character. Text for the title or subtitle or any text
#' to be wrapped.
#' @param plot_name Character. The name of the output plot.
#' @param font_size,n Numeric. Values to help calculate a proxy of length.
#' @returns Character. String with wrapped values.
#' @export
autoline_shiny <- function(session, text, plot_name, font_size = 14, n = 1.6) {
  x <- session$clientData[[sprintf("output_%s_width", plot_name)]]
  max_title_len <- n * x / font_size
  words <- strsplit(text, " ")[[1]]
  lines <- character()
  curr_line <- ""
  for (word in words) {
    curr_len <- nchar(curr_line)
    word_len <- nchar(word)
    if (curr_len + word_len + 1 <= max_title_len) {
      curr_line <- paste(curr_line, word, sep = " ")
    } else {
      lines <- c(lines, curr_line)
      curr_line <- word
    }
  }
  lines <- c(lines, curr_line)
  text <- paste(lines, collapse = "\n")
  return(text)
}
