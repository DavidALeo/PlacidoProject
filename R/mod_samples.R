#' samples UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_samples_ui <- function(id){
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      h2("Resumen"),
      "Datos necesarios para continuar:",
      numericInput(
        inputId = ns("BatchSize"),
        label = "Tamaño del lote",
        value = 100
      ),
      numericInput(
        inputId =  ns("LabeledQuantity"),
        label = "Cantidad nominal",
        value = 45
      ),
      fileInput(ns("first_sample"), "Selecciona un fichero"),
      selectInput(ns("first_noncon_column"), "Selecciona columna de valores", c()),
      hidden(tags$div(
        id = ns("second_sample_div"),
        fileInput(ns("second_sample"), "Selecciona un fichero para la segunda muestra"),
        selectInput(ns("second_noncon_column"), "Selecciona columna de valores para la segunda muestra", c())
      ))
    ),
    mainPanel(
      h2("Resumen"),
      verbatimTextOutput(ns("print"))
    )
  )
}

#' samples Server Functions
#'
#' @noRd
mod_samples_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observe({
      # Actualizar el valor de BatchSize cuando cambie en otro módulo
      if (data$batch_size != input$BatchSize){
        updateNumericInput(
        session,
        inputId = "BatchSize",
        value = data$batch_size
      )}
    }) %>% bindEvent(data$batch_size)

    observe({
      # Actualizar el valor de LabeledQuantity cuando cambie en otro módulo
      if (data$labeled_quantity != input$LabeledQuantity){
        updateNumericInput(
          session,
          inputId = "LabeledQuantity",
          value = data$labeled_quantity
        )
      }
    }) %>% bindEvent(data$labeled_quantity)

    observe({
      data$labeled_quantity <- input$LabeledQuantity
    }) %>% bindEvent(input$LabeledQuantity)

    observe({
      data$batch_size <- input$BatchSize
    }) %>% bindEvent(input$BatchSize)

    output$print <- renderPrint({
      data$labeled_quantity
    })
  })
}

## To be copied in the UI
# mod_samples_ui("samples_1")

## To be copied in the server
# mod_samples_server("samples_1")
