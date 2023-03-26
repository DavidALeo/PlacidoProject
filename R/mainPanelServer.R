library(DT)

main_panel_server <- function(input, output, session, data) {
  # Muestra la tabla filtrada
  output$table <- renderDT({
    req(data(), input$select_col, input$select_val)
    data() %>% filter(!!sym(input$select_col) == input$select_val)
  })

  # Genera un gráfico en función de las columnas seleccionadas
  output$grafico <- renderPlot({
    req(data(), input$select_col_x, input$select_col_y)
    ggplot(data = data(), aes(x = !!sym(input$select_col_x), y = !!sym(input$select_col_y))) +
      geom_point()
  })

  # Actualiza el selector de columna para el eje X
  observe({
    req(data())
    updateSelectInput(session, "select_col_x",
                      choices = names(data()), selected = NULL)
  })

  # Actualiza el selector de columna para el eje Y
  observe({
    req(data(), input$select_col)
    updateSelectInput(session, "select_col_y",
                      choices = names(data()),
                      selected = NULL)
  })
}
