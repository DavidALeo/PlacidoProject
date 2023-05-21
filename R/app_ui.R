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
        tabPanel(title = "Análisis de no-conformidades",mod_plots_ui("plots")),
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

      theme = custom_theme()
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

#' Modify Shiny Theme
#'
#' This function creates a custom Shiny theme for this app.
#'
#' @return A modified Shiny theme object.
#' @importFrom bslib bs_theme font_collection font_google bs_add_variables bs_add_rules
#' @noRd
custom_theme <- function() {
  theme <- bs_theme(
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

  theme <- bs_add_variables(theme, "container-bg" = "#FFFFFF", "container-border-color" = "#E0E0E0", "control-border-color" = "#CCCCCC")
  theme <- bs_add_variables(theme, "navbar-bg" = "#245953", "navbar-fg" = "#FFFFFF", "sidebar-bg" = "#408E91", "sidebar-fg" = "#FFFFFF")

  theme <- bs_add_rules(theme, ".container { background-color: $container-bg; border-color: $container-border-color; }")
  theme <- bs_add_rules(theme, ".navbar { background-color: $navbar-bg; color: $navbar-fg; position: fixed; top: 0; left: 0; width: 100%; z-index: 100; }")
  theme <- bs_add_rules(theme, ".tab-content {padding-top: 60px;}")
  theme <- bs_add_rules(theme, ".plots { border: 2px solid #408E91; padding: 10px; }")
  return(theme)
}
