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
    p("Este es un proyecto de an\u00e1lisis de datos utilizando el marco de trabajo Shiny de R.
      El objetivo principal de este proyecto es proporcionar una herramienta interactiva
      para explorar, visualizar y analizar datos relacionados con el ", a(href="https://www.boe.es/buscar/doc.php?id=BOE-A-2008-17629", "Real Decreto 1801/2008"),"."),
    p("Puedes consultar también el ", a(href="www/user_manual.pdf", "manual de usuario"),"."),
    p("Para m\u00e1s detalles sobre el c\u00f3digo fuente y la implementaci\u00f3n, por favor visite nuestro ",
      a(href="https://github.com/DavidALeo/PlacidoProject", "repositorio de GitHub"), "."),
    h3("Autores"),
    p("David Leo"),
    p("Emilio L\u00f3pez Cano")
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
