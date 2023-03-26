sidebar_server <- function(input, output, session, data) {
  # Actualiza las opciones de la lista desplegable de columnas
  observe({
    req(data())
    updateSelectInput(session, "select_col",
                      choices = names(data()), selected = NULL)
  })

  # Actualiza las opciones de la lista desplegable de valores
  observe({
    req(data(), input$select_col)
    updateSelectInput(session, "select_val",
                      choices = sort(unique(data()[[input$select_col]])),
                      selected = NULL)
  })
}
