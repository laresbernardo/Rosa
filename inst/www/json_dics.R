library(lares)
library(jsonlite)

replace_strings_in_json_files <- function(file_list, replacement_df, output_dir) {
  # Create the output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  file_list <- file_list[grepl(".json", file_list)]
  for (file in file_list) {
    # Read the JSON file
    json_data <- fromJSON(file)
    # Convert JSON data to a string
    json_str <- toJSON(json_data, pretty = TRUE, auto_unbox = TRUE)
    # Replace strings based on the data.frame
    for (i in 1:nrow(replacement_df)) {
      old_string <- replacement_df$old_string[i]
      new_string <- replacement_df$new_string[i]
      json_str <- gsub(old_string, new_string, json_str)
    }
    # Convert the modified string back to JSON data
    modified_json_data <- fromJSON(json_str)
    # Create the new filename by appending "STD" and placing it in the output directory
    original_filename <- basename(file)
    new_filename <- file.path(output_dir, sub("(.*)\\.json$", "\\1_STD.json", original_filename))
    # Write the modified JSON data to a new file
    write_json(modified_json_data, new_filename, pretty = TRUE)
    lares::statusbar(which(file == file_list), length(file_list), original_filename)
  }
}
