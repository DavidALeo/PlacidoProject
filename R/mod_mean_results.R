#' mean_results UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_mean_results_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' mean_results Server Functions
#'
#' @noRd 
mod_mean_results_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_mean_results_ui("mean_results_1")
    
## To be copied in the server
# mod_mean_results_server("mean_results_1")
