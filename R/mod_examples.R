#' examples UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_examples_ui <- function(id){
  ns <- NS(id)
  tagList(

    h2("Cargar Datos de Ejemplo"),
    p("Aquí puedes cargar tres conjuntos de datos de ejemplo, cada uno con diferentes resultados."),

    h3("Conjunto de datos 1 (Rechazado)"),
    p("El análisis de este conjunto de datos contiene demasiadas no conformidades y se rechazará."),
    actionButton(ns("load_fail"), "Cargar Conjunto de Datos 1"),

    h3("Conjunto de datos 2 (Requiere Segundo Análisis)"),
    p("Este conjunto de datos requerirá un análisis adicional."),
    actionButton(ns("load_second_analysis"), "Cargar Conjunto de Datos 2"),

    h3("Conjunto de datos 3 (Aceptado)"),
    p("El análisis de este conjunto de datos será suficiente para aceptar el lote."),
    actionButton(ns("load_accepted"), "Cargar Conjunto de Datos 3")
  )
}

#' examples Server Functions
#'
#' @noRd
mod_examples_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observe({
      data$batch_data[[data$current_batch]]$first_sample <- sample_dataset
      gargoyle::trigger("first_sample")

      data$batch_data[[data$current_batch]]$first_sample_column <- "Volumen"
      gargoyle::trigger("first_sample_column")

      data$batch_data[[data$current_batch]]$labeled_quantity <- 30
      data$batch_data[[data$current_batch]]$batch_size <- 100

      gargoyle::trigger("labeled_quantity")
      gargoyle::trigger("batch_size")
      perform_analysis()
    }) %>% bindEvent(input$load_accepted)

    observe({
      data$batch_data[[data$current_batch]]$first_sample <- sample_dataset
      gargoyle::trigger("first_sample")

      data$batch_data[[data$current_batch]]$first_sample_column <- "Volumen"
      gargoyle::trigger("first_sample_column")

      data$batch_data[[data$current_batch]]$labeled_quantity <- 33
      data$batch_data[[data$current_batch]]$batch_size <- 100
      gargoyle::trigger("labeled_quantity")
      gargoyle::trigger("batch_size")

      perform_analysis()
      data$batch_data[[data$current_batch]]$second_sample_required <- TRUE
      gargoyle::trigger("second_sample_required")

      data$batch_data[[data$current_batch]]$second_sample <- sample_dataset
      gargoyle::trigger("second_sample")

      data$batch_data[[data$current_batch]]$second_sample_column <- "Volumen"
      gargoyle::trigger("second_sample_column")
      perform_analysis()
    }) %>% bindEvent(input$load_second_analysis)

    observe({
      data$batch_data[[data$current_batch]]$first_sample <- sample_dataset
      gargoyle::trigger("first_sample")

      data$batch_data[[data$current_batch]]$first_sample_column <- "Volumen"
      gargoyle::trigger("first_sample_column")

      data$batch_data[[data$current_batch]]$labeled_quantity <- 40
      data$batch_data[[data$current_batch]]$batch_size <- 100

      gargoyle::trigger("labeled_quantity")
      gargoyle::trigger("batch_size")
      perform_analysis()
    }) %>% bindEvent(input$load_fail)

    observe({
      data$batch_data[[data$current_batch]]$first_sample <- sample_dataset
      gargoyle::trigger("first_sample")

      data$batch_data[[data$current_batch]]$first_sample_column <- "Volumen"
      gargoyle::trigger("first_sample_column")

      data$batch_data[[data$current_batch]]$labeled_quantity <- 30
      data$batch_data[[data$current_batch]]$batch_size <- 100

      gargoyle::trigger("labeled_quantity")
      gargoyle::trigger("batch_size")
      perform_analysis()
    }) %>% bindEvent(input$load_accepted)

    perform_analysis <- function (){
      req_data <- data$batch_data[[data$current_batch]]

      data$batch_data[[data$current_batch]]$first_noncon_analysis <- first_noncon_analysis(
        sample_dataset,
        req_data$first_sample_column,
        req_data$labeled_quantity,
        req_data$batch_size)

      if (data$batch_data[[data$current_batch]]$first_noncon_analysis$decision == "Second analysis" & data$batch_data[[data$current_batch]]$second_sample_column != "") {

        data$batch_data[[data$current_batch]]$second_noncon_analysis <- second_noncon_analysis(
          req_data$first_sample,
          req_data$first_sample_column,
          req_data$second_sample,
          req_data$second_sample_column,
          req_data$labeled_quantity,
          req_data$batch_size
        )
      }

      data$batch_data[[data$current_batch]]$mean_analysis <- mean_analysis(
        sample_dataset,
        req_data$first_sample_column,
        req_data$batch_size,
        req_data$labeled_quantity
      )
      data$batch_data[[data$current_batch]]$decision <- data$batch_data[[data$current_batch]]$mean_analysis$decision

      if ((data$batch_data[[data$current_batch]]$first_noncon_analysis$decision == "Accept" | data$batch_data[[data$current_batch]]$second_noncon_analysis$decision == "Accept")
          & data$batch_data[[data$current_batch]]$mean_analysis$decision == "Accept") {
        data$batch_data[[data$current_batch]]$decision <- "Accept"
      } else{
        data$batch_data[[data$current_batch]]$decision <- "Reject"
      }

      gargoyle::trigger("analysis_completed")
    }
  })
}

## To be copied in the UI
# mod_examples_ui("examples")

## To be copied in the server
# mod_examples_server("examples")
