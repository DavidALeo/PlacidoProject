#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bslib bs_theme font_collection font_google bs_theme_preview
#' @importFrom shinyjs useShinyjs
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    useShinyjs(),
    # Your application UI logic
    fluidPage(
      navbarPage(
        title = "PlacidoApp",
        tabPanel(title = "Resumen",mod_overview_ui("overview_1")),
        tabPanel(title = "Muestras",mod_samples_ui("samples_1")),
        tabPanel(title = "An\u00e1lisis de no-conformidades",mod_noncon_results_ui("noncon_analysis_results_page")),
        tabPanel(title = "An\u00e1lisis de la media",mod_mean_results_ui("mean_analysis_results_page")),
        navbarMenu(
          title = "M\u00e1s",
          tabPanel(title = "Datos de ejemplo", mod_examples_ui("examples")),
          tabPanel(title = "Ajustes visuales", mod_visual_settings_ui("visual_settings")),
          "----",
          tabPanel(title = "M\u00e1s informaci\u00f3n", mod_about_ui("about"))
        ),
        windowTitle = "PlacidoApp",
        theme = base_theme()
      ),

      theme = base_theme()
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "PlacidoApp"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
