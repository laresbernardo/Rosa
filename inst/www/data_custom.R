#' Demo Dummy Data
#' 
#' Dummy data to play with. Dataset and JSON file.
#'
#' @name dummy_data
#' @docType data
#' @usage data(demo_data)
#' @format An object of class \code{"data.frame"}
#' @examples
#' data(demo_data)
#' demo_data
"demo_data"

# save(demo_data, file = "data/demo_data.RData")

#' @name dummy_data
#' @docType data
#' @usage data(demo_json)
#' @format An object of class \code{"list"}
#' @examples
#' data(demo_json)
#' demo_json
"demo_json"

# demo_json <- Robyn::robyn_read("Robyn_202403121204_init/RosaModel-1_238_10.json")
# save(demo_json, file = "inst/data/demo_json.RData")
