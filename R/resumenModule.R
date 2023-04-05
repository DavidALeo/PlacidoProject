resumenModuleUI <- function(id, label = "CSV file") {
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(
      card_body_fill(
        h2("Resumen"),
        "Datos necesarios para continuar:",
        numericInput(
          inputId = ns("BatchSize"),
          label = "Tamaño del lote",
          value = 0
        ),
        numericInput(
          inputId =  ns("LabeledQuantity"),
          label = "Cantidad nominal",
          value = 0
        )
      )
    ),
    mainPanel(
      h2("Resumen"),
      htmlOutput(ns("textualExplanation")),
    )
  )
}

resumenModuleServer <- function(id, sizes_df) {
  cat("Cargando resumenModuleServer\n")
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      sampleSizes <- reactive({
        sizes_row <- sizes_df %>%
          filter(input$BatchSize >= batch_size_min, input$BatchSize <= batch_size_max) %>%
          slice(1)

        # Extrae los valores correspondientes
        list(nonCon1=sizes_row$sample1_size,
             nonCon2=sizes_row$sample2_size)})

      observe({
        if (length(sampleSizes()$nonCon1) == 0) {
          output$textualExplanation <- renderText({
            paste("El tamaño del lote es demasiado pequeño.")
          })
        } else {
          output$textualExplanation <- renderText({
            paste(
              "Para un tamaño de lote de ", tags$span(class = "badge bg-secondary", input$BatchSize), "unidades, corresponde un tamaño de muestra inicial de ",
              tags$span(class = "badge bg-secondary", sampleSizes()$nonCon1)
            )
          })
        }
      })

      output$salida1 <- renderText({
        paste("El tamaño del lote es:", input$BatchSize)
      })
      output$salida2 <- renderText({
        paste("La cantidad nominal es:", input$LabeledQuantity)
      })

      return(list(BatchSize = reactive(input$BatchSize),
                  LabeledQuantity= reactive(input$LabeledQuantity),
                  sampleSizes = sampleSizes
            ))
    }
  )
}
