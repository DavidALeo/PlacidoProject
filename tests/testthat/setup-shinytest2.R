# Load application support files into testing environment

shinytest2::load_app_env(
  app_dir = "../../",
  renv = rlang::caller_env(),
  globalrenv = rlang::caller_env()
)
