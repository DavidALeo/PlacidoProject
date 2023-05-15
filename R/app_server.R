#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @noRd
app_server <- function(input, output, session) {
  mod_overview_server("overview_1")
  data <- reactiveValues(batch_size = 45, labeled_quantity = 100)
  mod_samples_server("samples_1", data = data)
  mod_samples_server("samples_2", data = data)
}
