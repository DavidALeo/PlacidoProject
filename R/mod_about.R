#' about UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_about_ui <- function(id){
  ns <- NS(id)
  tagList(
    h2("Acerca de.."),
    p("Este es un proyecto de análisis de datos utilizando el marco de trabajo Shiny de R.
      El objetivo principal de este proyecto es proporcionar una herramienta interactiva
      para explorar, visualizar y analizar datos relacionados con el Real Decreto 1801/2008."),
    p("Para más detalles sobre el código fuente y la implementación, por favor visite nuestro ",
      a(href="https://github.com/DavidALeo/PlacidoProject", "repositorio de GitHub"), "."),
    h3("Autores"),
    p("David Leo"),
    p("Emilio López Cano")
  )
}

#' about Server Functions
#'
#' @noRd
mod_about_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_about_ui("about")

## To be copied in the server
# mod_about_server("about")
