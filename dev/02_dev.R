# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Amend DESCRIPTION with dependencies read from package code parsing
## install.packages('attachment') # if needed.
attachment::att_amend_desc()

## Add modules ----
## Create a module infrastructure in R/
golem::add_module(name = "name_of_module1", with_test = TRUE) # Name of the module
golem::add_module(name = "name_of_module2", with_test = TRUE) # Name of the module

## Add helper functions ----
## Creates fct_* and utils_*
golem::add_fct("helpers", with_test = TRUE)
golem::add_utils("helpers", with_test = TRUE)

## External resources (Not used in this project)
## Creates .js and .css files at inst/app/www
golem::add_js_file("script")
golem::add_js_handler("handlers")
golem::add_css_file("custom")
golem::add_sass_file("custom")

## Add internal datasets
## If you have data in your package
usethis::use_data_raw(name = "my_dataset", open = FALSE)

## Tests ----
usethis::use_test("app")
record_test("./")

# Documentation

## Vignette ----
usethis::use_vignette("PlacidoApp")
devtools::build_vignettes()

## Code Coverage  (Not used in this project)
## Set the code coverage service ("codecov" or "coveralls")
usethis::use_coverage()

## CI ----
## Use this part of the script if you need to set up a CI
## service for your application
##
## (You'll need GitHub there)
usethis::use_github()

# GitHub Actions
usethis::use_github_action()
# Chose one of the three
# See https://usethis.r-lib.org/reference/use_github_action.html
usethis::use_github_action_check_release()
usethis::use_github_action_check_standard()
usethis::use_github_action_check_full()
# Add action for PR
usethis::use_github_action_pr_commands()

# Fix non ASCII Characters

fix_non_ascii_characters <- function() {
  convertir_a_unicode <- function(texto) {
    # Vector con caracteres a convertir
    special_characters <- c("á", "é", "í", "ó", "ú", "Á", "É", "Í", "Ó", "Ú", "ñ", "Ñ", "ü", "Ü")

    for (i in 1:length(special_characters)) {
      texto <- gsub(special_characters[i], paste0("\\", stringi::stri_escape_unicode(special_characters[i])), texto)
    }

    return(texto)


  }

  files <- list.files(path = "R", pattern = "*.R")
  for (file in files) {

    content <- readLines(paste0("R/", file))

    fixed_content <- convertir_a_unicode(content)

    writeLines(fixed_content, paste0("R/", file))
  }
}
