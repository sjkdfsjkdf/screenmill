# ---- Editable data frame ----
# library(shiny)
# library(miniUI)
# library(rhandsontable)
# library(dplyr)


table_gadget <- function() {

  # ---- Input DF ----
  df <- data_frame(a = 1:10, b = LETTERS[1:10])

  # ---- Shiny Gadget UI ----
  ui <-
    miniPage(
      gadgetTitleBar("Edit a data.frame"),
      miniContentPanel(
        rHandsontableOutput("hot")
      )
    )

  # ---- Shiny Gadget Server ----
  server <- function(input, output, session) {
    output$hot <- renderRHandsontable({
      if (is.null(input$hot)) DF <- df else DF <- hot_to_r(input$hot)
      DF %>%
        rhandsontable %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE)
    })
    observeEvent(input$done, stopApp(hot_to_r(input$hot)))
  }

  # ---- Run ----
  runGadget(ui, server, viewer = dialogViewer("Edit", width = 1000, height = 800))
}

table_gadget()
