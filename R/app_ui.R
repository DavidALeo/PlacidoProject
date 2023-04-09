#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bslib bs_theme font_collection font_google bs_theme_preview
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      navbarPage(
        title = "PlacidoApp",
        tabPanel(title = "Resumen",mod_overview_ui("overview_1")),
        tabPanel(title = "Muestreo"),
        tabPanel(title = "Análisis de no-conformidades"),
        tabPanel(title = "Análisis de la media"),
        tabPanel(title = "Informes"),
        navbarMenu(
          title = "Más",
          tabPanel(title = "Summary"),
          "----",
          "Section header",
          tabPanel(title = "Table")
        ),
        windowTitle = "PlacidoApp"
      ),

      theme = bs_theme(
        version = 5,
        bg = "#D8D8D8",
        fg = "#245953",
        primary = "#408E91",
        secondary = "#E49393",
        base_font = font_collection(
          font_google(family = "Nunito Sans"),
          "-apple-system",
          "BlinkMacSystemFont",
          "Segoe UI",
          font_google(family = "Roboto"),
          "Helvetica Neue",
          "Arial",
          "sans-serif",
          "Apple Color Emoji",
          "Segoe UI Emoji",
          "Segoe UI Symbol"
        )
      )
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
