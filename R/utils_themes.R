#' Base theme
#'
#' This function creates a custom Shiny theme for this app.
#'
#' @return A modified Shiny theme object.
#' @importFrom bslib bs_theme font_collection font_google bs_add_variables bs_add_rules
#' @noRd
base_theme <- function() {
  theme <- bs_theme(
    version = 5,
    bg = "#D8D8D8",
    fg = "#1E453E",
    primary = "#40B9BD",
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
  theme <- bs_add_variables(theme, "navbar-bg" = "#1E453E", "navbar-fg" = "#FFFFFF", "sidebar-bg" = "#40B9BD", "sidebar-fg" = "#FFFFFF")

  theme <- bs_add_rules(theme, ".container { background-color: $container-bg; border-color: $container-border-color; }")
  theme <- bs_add_rules(theme, ".navbar { background-color: $navbar-bg; color: $navbar-fg; position: fixed; top: 0; left: 0; width: 100%; z-index: 100; }")
  theme <- bs_add_rules(theme, ".tab-content {padding-top: 60px;}")
  theme <- bs_add_rules(theme, ".plots { border: 2px solid #408E91; padding: 10px; }")
  return(theme)
}

#' Standard theme
#'
#' This function creates a custom Shiny theme for this app.
#'
#' @return A modified Shiny theme object.
#' @importFrom bslib bs_theme font_collection font_google bs_add_variables bs_add_rules
#' @noRd
standard_theme <- function() {
  theme <- bs_theme(
    version = 5,
    bg = "#D8D8D8",
    fg = "#1E453E",
    primary = "#40B9BD",
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
  theme <- bs_add_variables(theme, "navbar-bg" = "#1E453E", "navbar-fg" = "#FFFFFF", "sidebar-bg" = "#40B9BD", "sidebar-fg" = "#FFFFFF")

  theme <- bs_add_rules(theme, ".container { background-color: $container-bg; border-color: $container-border-color; }")
  theme <- bs_add_rules(theme, ".navbar { background-color: $navbar-bg; color: $navbar-fg; position: fixed; top: 0; left: 0; width: 100%; z-index: 100; }")
  theme <- bs_add_rules(theme, ".tab-content {padding-top: 60px;}")
  theme <- bs_add_rules(theme, ".plots { border: 2px solid #408E91; padding: 10px; }")
  return(theme)
}

#' Standard theme
#'
#' This function creates a custom Shiny theme for this app.
#'
#' @return A modified Shiny theme object.
#' @importFrom bslib bs_theme font_collection font_google bs_add_variables bs_add_rules
#' @noRd
high_contrast_theme <- function() {
  theme <- bs_theme(
    version = 5,
    bg = "#D8D8D8",
    fg = "#000000",
    primary = "#706C61",
    secondary = "#E1F4F3",
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

  theme <- bs_add_variables(theme, "container-bg" = "#FFFFFF", "container-border-color" = "#000000", "control-border-color" = "#000000")
  theme <- bs_add_variables(theme, "navbar-bg" = "#D8D8D8", "navbar-fg" = "#000000", "sidebar-bg" = "#D8D8D8", "sidebar-fg" = "#000000")

  theme <- bs_add_rules(theme, ".container { background-color: $container-bg; border-color: $container-border-color; }")
  theme <- bs_add_rules(theme, ".navbar { background-color: $navbar-bg; color: $navbar-fg; position: fixed; top: 0; left: 0; width: 100%; z-index: 100; }")
  theme <- bs_add_rules(theme, ".tab-content {padding-top: 60px;}")
  theme <- bs_add_rules(theme, ".plots { border: 2px solid #408E91; padding: 10px; }")
  return(theme)
}
