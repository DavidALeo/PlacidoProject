#' visual_settings UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_visual_settings_ui <- function(id){
  ns <- NS(id)
  tagList(
    h2("Configuraciones de Visualización"),
    p("Puedes cambiar el tema visual de la aplicación para que se adapte mejor a tus necesidades."),
    actionButton(ns("standard_theme"), "Tema Estándar"),
    actionButton(ns("high_contrast_theme"), "Tema de Alto Contraste")
  )
}

#' visual_settings Server Functions
#'
#' @noRd
mod_visual_settings_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observe({
      session$setCurrentTheme(standard_theme())
    }) %>% bindEvent(input$standard_theme)
    observe({
      session$setCurrentTheme(high_contrast_theme())
    }) %>% bindEvent(input$high_contrast_theme)
  })
}

## To be copied in the UI
# mod_visual_settings_ui("visual_settings")

## To be copied in the server
# mod_visual_settings_server("visual_settings")
