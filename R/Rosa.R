#' Interface for MMM powered by Robyn
#'
#' @name Rosa
#' @docType package
#' @import dplyr
#' @importFrom ellmer chat_openai
#' @import ggplot2
#' @importFrom graphics abline mtext
#' @importFrom gt gt opt_interactive fmt_number fmt_percent data_color cols_label
#' tab_style tab_options cell_fill cell_text cells_body
#' cells_column_labels cells_row_groups tab_footnote
#' @importFrom hover hover_action_button hover_download_button use_hover
#' @importFrom httr add_headers content GET POST
#' @importFrom jsonlite read_json write_json toJSON fromJSON
#' @importFrom lares cache_clear cache_exists cache_read cache_write check_opts
#' cleanText file_name formatNum noPlot num_abbr removenacols glued num_abbr
#' robyn_hypsbuilder robyn_modelselector robyn_performance scale_x_abbr
#' scale_y_abbr scale_y_percent theme_lares tic toc try_require v2t
#' @importFrom markdown markdownToHTML
#' @importFrom officer add_slide block_list fp_text fpar ftext move_slide
#' ph_location_label ph_with read_pptx remove_slide slide_visible
#' @importFrom patchwork free plot_layout wrap_elements wrap_table
#' @importFrom querychat querychat_data_source querychat_init querychat_server querychat_ui
#' @importFrom reticulate py_install use_virtualenv virtualenv_create
#' @importFrom ragnar embed_openai markdown_chunk ragnar_register_tool_retrieve
#' ragnar_store_build_index ragnar_store_create ragnar_store_connect
#' ragnar_store_insert read_as_markdown
#' @import Robyn
#' @import shiny
#' @importFrom shinyjs alert disable enable useShinyjs
#' @importFrom shinyWidgets autonumericInput pickerInput sendSweetAlert switchInput
#' updatePickerInput updateSwitchInput updateAutonumericInput
#' @importFrom stats cor lm reorder setNames
#' @importFrom utils data head read.csv packageVersion tail write.csv
"_PACKAGE"

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste0(
    "Hello ", Sys.info()[["user"]], ", welcome to Rosa v",
    packageVersion("Rosa"),
    "\n>>> To load the interactive app, run: launch()"
  ))
  shiny::addResourcePath("www", system.file("www", package = "Rosa"))
  options(ragnar.markitdown.use_reticulate = FALSE)
}

recommended <- list(
  AzureAuth = "get_azure_token",
  AzureStor = c("storage_endpoint", "storage_container", "storage_upload"),
  AzureTableStor = c("insert_table_entity", "storage_table"),
  callr = "r_bg",
  devtools = c("build", "install_local"),
  remotes = "install_github",
  utils = "shortPathName"
)

if (getRversion() >= "2.15.1") {
  globalVariables(c(as.vector(unlist(recommended)), ".", "demo_data", "demo_json"))
}
