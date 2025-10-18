load_reticulate <- function(reticulate_loaded = FALSE) {
  if (!reticulate_loaded) {
    withProgress(message = "Environment", value = 0, {
      if (is_server()) {
        incProgress(1 / 3, detail = paste("Setting server"))
        # venvname <- "pyenv"
        # if (!dir.exists(venvname)) {
        #   # pip install virtualenv
        #   system(paste("virtualenv", venvname))
        #   system(paste0("source ", venvname, "/bin/activate"))
        # }
        # venvdir <- paste0("./", venvname)
        # Sys.setenv("RETICULATE_PYTHON" = venvdir)
      } else {
        incProgress(1 / 3, detail = paste("Setting local"))
        virtualenv_create("r-reticulate")
        use_virtualenv("r-reticulate", required = FALSE)
      }
      # Print loaded configurations
      print(reticulate::py_config())

      Sys.sleep(0.5)
      incProgress(1 / 3, detail = paste("Setting nevergrad"))
      if (!reticulate::py_module_available("nevergrad")) {
        if (is_server()) {
          system("pip install nevergrad")
        } else {
          reticulate::py_install("nevergrad", ignore_installed = TRUE, pip = TRUE)
        }
      }
      ng <- reticulate::import("nevergrad", delay_load = TRUE)
      seed <- 123
      if (is.integer(seed)) {
        np <- reticulate::import("numpy", delay_load = FALSE)
        np$random$seed(seed)
      }
      Sys.sleep(0.5)
      incProgress(1 / 3, detail = paste("Setup done"))
      Sys.sleep(1)
    })
  }
  return(TRUE)
}
