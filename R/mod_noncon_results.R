#' noncon_results UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_noncon_results_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' noncon_results Server Functions
#'
#' @noRd 
mod_noncon_results_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_noncon_results_ui("noncon_results_1")
    
## To be copied in the server
# mod_noncon_results_server("noncon_results_1")
