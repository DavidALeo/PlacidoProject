#' overview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import dplyr
mod_overview_ui <- function(id) {
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

#' overview Server Functions
#'
#' @noRd
mod_overview_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    sample_sizes <- reactive({
      sizes_row <- sizes_df %>%
        filter(input$BatchSize >= .data$batch_size_min, input$BatchSize <= .data$batch_size_max) %>%
        slice(1)

      # Extrae los valores correspondientes
      if (length(sizes_row) == 0) {
        list(
          nonCon1 = 0,
          nonCon2 = 0
        )
      } else {
        list(
          nonCon1 = sizes_row$sample1_size,
          nonCon2 = sizes_row$sample2_size
        )
      }
    })

    observe({
      if ((length(sample_sizes()$nonCon1) == 0) || sample_sizes()$nonCon1 == 0) {
        output$textualExplanation <- renderText({
          paste("El tamaño del lote es demasiado pequeño.")
        })
      } else {
        output$textualExplanation <- renderText({
          paste(
            "Para un tamaño de lote de ", tags$span(class = "badge bg-secondary", input$BatchSize), "unidades, corresponde un tamaño de muestra inicial de ",
            tags$span(class = "badge bg-secondary", sample_sizes()$nonCon1)
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

    return(list(
      BatchSize = reactive(input$BatchSize),
      LabeledQuantity = reactive(input$LabeledQuantity),
      sample_sizes = sample_sizes
    ))
  })
}

## To be copied in the UI
# mod_overview_ui("overview_1")

## To be copied in the server
# mod_overview_server("overview_1")
