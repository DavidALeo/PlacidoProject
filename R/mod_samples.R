#' samples UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom gargoyle trigger watch
mod_samples_ui <- function(id){
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      mod_sidebar_ui(ns("sidebar_1"))
    ),
    mainPanel(
      h2("Muestras"),
      h3("Primera muestra"),
      DTOutput(ns("first_sample_table")),
      hidden(tags$div(
        id = ns("second_sample_div"),
        h3("Segunda muestra"),
        DTOutput(ns("second_sample_table"))))
    )
  )
}

#' samples Server Functions
#'
#' @noRd
mod_samples_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    mod_sidebar_server("sidebar_1", data)
    observe({
      req(data$batch_data[[data$current_batch]]$first_sample)
      df <- data$batch_data[[data$current_batch]]$first_sample
      selected_column <- data$batch_data[[data$current_batch]]$first_sample_column
      output$first_sample_table <- renderDT({
        DT::datatable(df,
                      options = list(
                        pageLength = 5
                      ),
                      rownames = FALSE, selection = 'none',
                      extensions = 'Responsive'
        ) %>% formatStyle(selected_column,
                          color = "#FFFFFF", backgroundColor = "#E49393", fontWeight = "bold"
        )
      },server = FALSE)
    }) %>% bindEvent(gargoyle::watch("first_sample_column"))

    observe({
      req(data$batch_data[[data$current_batch]]$second_sample)
      df <- data$batch_data[[data$current_batch]]$second_sample
      selected_column <- data$batch_data[[data$current_batch]]$second_sample_column
      output$second_sample_table <- renderDT({
        DT::datatable(df,
                      options = list(
                        pageLength = 5
                      ),
                      rownames = FALSE, selection = 'none',
                      extensions = 'Responsive'
        ) %>% formatStyle(selected_column,
                          color = "#FFFFFF", backgroundColor = "#E49393", fontWeight = "bold"
        )
      },server = FALSE)
    }) %>% bindEvent(gargoyle::watch("second_sample_column"))

    # Show/Hide second analysis parts
    observe({
      if (data$batch_data[[data$current_batch]]$second_sample_required) {
        show("second_sample_div")
      } else {
        hide("second_sample_div")
      }
    }) %>% bindEvent(gargoyle::watch("second_sample_required"))

    output$print <- renderPrint({
      data$labeled_quantity
    })
  })
}

## To be copied in the UI
# mod_samples_ui("samples_1")

## To be copied in the server
# mod_samples_server("samples_1")
