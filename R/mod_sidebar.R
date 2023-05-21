#' sidebar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyjs hidden show hide
#' @importFrom gargoyle trigger watch
mod_sidebar_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Resumen"),
    "Datos necesarios para continuar:",
    hidden(selectInput(
      inputId = ns("analysis_select"),
      label = "Selecciona un análisis",
      choices = c() # inicialmente vacío
    )),
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
    selectInput(ns("first_sample_column"), "Selecciona columna de valores", c()),
    hidden(tags$div(
      id = ns("second_sample_div"),
      fileInput(ns("second_sample"), "Selecciona un fichero para la segunda muestra"),
      selectInput(ns("second_sample_column"), "Selecciona columna de valores para la segunda muestra", c())
    )),
    actionButton(ns("do"), "Analizar")
  )
}

#' sidebar Server Functions
#'
#' @noRd
mod_sidebar_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Show/Hide second analysis parts
    observe({
      if (data$batch_data[[data$current_batch]]$second_sample_required) {
        show("second_sample_div")
      } else {
        hide("second_sample_div")
      }
    }) %>% bindEvent(gargoyle::watch("second_sample_required"))

    # Update BatchSize input when changes in other modules
    observe({
      if (data$batch_data[[data$current_batch]]$batch_size != input$BatchSize) {
        updateNumericInput(
          session,
          inputId = "BatchSize",
          value = data$batch_data[[data$current_batch]]$batch_size
        )
      }
    }) %>% bindEvent(gargoyle::watch("batch_size"))

    # Update LabeledQuantity input when changes in other modules
    observe({
      if (data$batch_data[[data$current_batch]]$labeled_quantity != input$LabeledQuantity) {
        updateNumericInput(
          session,
          inputId = "LabeledQuantity",
          value = data$batch_data[[data$current_batch]]$labeled_quantity
        )
      }
    }) %>% bindEvent(gargoyle::watch("labeled_quantity"))

    # LabeledQuantity input
    observe({
      req(input$LabeledQuantity)
      if (data$batch_data[[data$current_batch]]$labeled_quantity != input$LabeledQuantity) {
        data$batch_data[[data$current_batch]]$labeled_quantity <- input$LabeledQuantity
        gargoyle::trigger("labeled_quantity")
      }
    }) %>% bindEvent(input$LabeledQuantity)

    # BatchSize input
    observe({
      if (data$batch_data[[data$current_batch]]$batch_size != input$BatchSize) {
        data$batch_data[[data$current_batch]]$batch_size <- input$BatchSize
        gargoyle::trigger("batch_size")
      }
    }) %>% bindEvent(input$BatchSize)

    # first_sample input
    observe({
      req(input$first_sample)
      data$batch_data[[data$current_batch]]$first_sample <- read.csv(file = input$first_sample$datapath)
      updateSelectInput(session, "first_sample_column", "Selecciona columna de valores",
        choices = data$batch_data[[data$current_batch]]$first_sample %>%
          select_if(is.numeric) %>%
          names()
      )
      gargoyle::trigger("first_sample")
    }) %>% bindEvent(input$first_sample)

    # Update first_sample_column input when changes in other modules
    observe({
      if (data$batch_data[[data$current_batch]]$first_sample_column != input$first_sample_column) {
        updateSelectInput(session, "first_sample_column", "Selecciona columna de valores",
          choices = data$batch_data[[data$current_batch]]$first_sample %>%
            select_if(is.numeric) %>%
            names(), selected = data$batch_data[[data$current_batch]]$first_sample_column
        )
      }
    }) %>% bindEvent(gargoyle::watch("first_sample_column"))

    # first_sample_column input
    observe({
      if (data$batch_data[[data$current_batch]]$first_sample_column != input$first_sample_column) {
        data$batch_data[[data$current_batch]]$first_sample_column <- input$first_sample_column
        gargoyle::trigger("first_sample_column")
      }
    }) %>% bindEvent(input$first_sample_column)

    # second_sample input
    observe({
      req(input$second_sample)
      data$batch_data[[data$current_batch]]$second_sample <- read.csv(file = input$second_sample$datapath)
      updateSelectInput(session, "second_sample_column", "Selecciona columna de valores",
        choices = data$batch_data[[data$current_batch]]$second_sample %>%
          select_if(is.numeric) %>%
          names()
      )
      gargoyle::trigger("second_sample")
    }) %>% bindEvent(input$second_sample)

    # Update second_sample_column input when changes in other modules
    observe({
      if (data$batch_data[[data$current_batch]]$second_sample_column != input$second_sample_column) {
        updateSelectInput(session, "second_sample_column", "Selecciona columna de valores",
          choices = data$batch_data[[data$current_batch]]$second_sample %>%
            select_if(is.numeric) %>%
            names(), selected = data$batch_data[[data$current_batch]]$second_sample_column
        )
      }
    }) %>% bindEvent(gargoyle::watch("second_sample_column"))

    # second_sample_column input
    observe({
      if (data$batch_data[[data$current_batch]]$second_sample_column != input$second_sample_column) {
        data$batch_data[[data$current_batch]]$second_sample_column <- input$second_sample_column
        gargoyle::trigger("second_sample_column")
      }
    }) %>% bindEvent(input$second_sample_column)

    # Perform the analysis
    observe({
      req_data <- data$batch_data[[data$current_batch]]
      req(
        req_data$first_sample,
        req_data$first_sample_column,
        req_data$labeled_quantity,
        req_data$batch_size
      )
      data$batch_data[[data$current_batch]]$decision <- "N/A"
      data$batch_data[[data$current_batch]]$first_noncon_analysis$decision <- "N/A"
      data$batch_data[[data$current_batch]]$second_noncon_analysis$decision <- "N/A"
      data$batch_data[[data$current_batch]]$mean_analysis$decision <- "N/A"
      tryCatch(expr = {
        data$batch_data[[data$current_batch]]$first_noncon_analysis <- first_noncon_analysis(
          req_data$first_sample,
          req_data$first_sample_column,
          req_data$labeled_quantity,
          req_data$batch_size
        )
      }, error = function(e) {
        data$batch_data[[data$current_batch]]$first_noncon_analysis$decision <- "ERROR"
        data$batch_data[[data$current_batch]]$first_noncon_analysis$msg <- e$message
        data$batch_data[[data$current_batch]]$first_noncon_analysis$df <- NULL
        data$batch_data[[data$current_batch]]$first_noncon_analysis$plot <- NULL
      })

      first_analysis_decision <- data$batch_data[[data$current_batch]]$first_noncon_analysis$decision
      if (first_analysis_decision == "Second analysis") {
        data$batch_data[[data$current_batch]]$second_sample_required <- TRUE
        gargoyle::trigger("second_sample_required")
      } else {
        data$batch_data[[data$current_batch]]$second_sample_required <- FALSE
        hide("second_sample_div")
        gargoyle::trigger("second_sample_required")
      }

      if (data$batch_data[[data$current_batch]]$first_noncon_analysis$decision == "Second analysis" & data$batch_data[[data$current_batch]]$second_sample_column != "") {
        tryCatch(expr = {
          data$batch_data[[data$current_batch]]$second_noncon_analysis <- second_noncon_analysis(
            req_data$first_sample,
            req_data$first_sample_column,
            req_data$second_sample,
            req_data$second_sample_column,
            req_data$labeled_quantity,
            req_data$batch_size
          )
        }, error = function(e) {
          print(e$message)
          data$batch_data[[data$current_batch]]$second_noncon_analysis$decision <- "ERROR"
          data$batch_data[[data$current_batch]]$second_noncon_analysis$msg<- e$message
          data$batch_data[[data$current_batch]]$second_noncon_analysis$df <- NULL
          data$batch_data[[data$current_batch]]$second_noncon_analysis$plot <- NULL
        })
      }

      tryCatch(expr = {
        data$batch_data[[data$current_batch]]$mean_analysis$decision <- mean_analysis(
          req_data$first_sample,
          req_data$first_sample_column,
          req_data$batch_size,
          req_data$labeled_quantity
        )
        data$batch_data[[data$current_batch]]$decision <- data$batch_data[[data$current_batch]]$mean_analysis$decision
      }, error = function(e) {
        data$batch_data[[data$current_batch]]$mean_analysis$decision <- "ERROR"
        data$batch_data[[data$current_batch]]$mean_analysis$msg <- e$message
      })

      if ((data$batch_data[[data$current_batch]]$first_noncon_analysis$decision == "Accept" | data$batch_data[[data$current_batch]]$second_noncon_analysis$decision == "Accept")
          & data$batch_data[[data$current_batch]]$mean_analysis$decision == "Accept") {
        data$batch_data[[data$current_batch]]$decision <- "Accept"
      } else{
        data$batch_data[[data$current_batch]]$decision <- "Reject"
      }

      gargoyle::trigger("analysis_completed")
    }) %>% bindEvent(input$do)
  })
}

## To be copied in the UI
# mod_sidebar_ui("sidebar_1")

## To be copied in the server
# mod_sidebar_server("sidebar_1")
