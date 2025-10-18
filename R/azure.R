azure_append_log <- function(
    new_log,
    table_name = Sys.getenv("AZURE_STORAGE_LOGS"),
    storage_account_name = Sys.getenv("AZURE_STORAGE_NAME"),
    storage_account_key = Sys.getenv("AZURE_STORAGE_KEY")) {
  lares::try_require("AzureStor")
  lares::try_require("AzureTableStor")
  # Create a table service client
  endpoint <- storage_endpoint(
    endpoint = sprintf("https://%s.table.core.windows.net", storage_account_name),
    key = storage_account_key
  )
  # Access the table
  table_service <- storage_table(endpoint, name = table_name)
  # Adapt log with partitions
  new_log$PartitionKey <- new_log$RowKey <- as.character(proc.time()["elapsed"])
  # Insert the new log entry into the Azure Table
  invisible(insert_table_entity(table_service, new_log))
}

azure_upload <- function(
    file, new_file = file,
    app_id = Sys.getenv("AZURE_APP_ID"),
    app_secret = Sys.getenv("AZURE_APP_SECRET"),
    tenant = Sys.getenv("AZURE_TENANT"),
    storage_account = Sys.getenv("AZURE_STORAGE_ACCOUNT"),
    container = Sys.getenv("AZURE_CONTAINER"),
    quiet = FALSE) {
  # To skip the dir creation prompt on AzureAuth
  Sys.setenv("R_AZURE_DATA_DIR" = tempdir())
  lares::try_require("AzureAuth")
  lares::try_require("AzureStor")
  # Get token
  token <- get_azure_token(
    resource = "https://storage.azure.com",
    tenant = tenant,
    app = app_id,
    password = app_secret,
    use_cache = FALSE
  )
  # Upload file
  endp <- storage_endpoint(
    endpoint = storage_account,
    token = token
  )
  cont <- storage_container(endp, container)
  new_file <- gsub("//", "/", new_file)
  storage_upload(cont, file, new_file)
  if (!quiet) message("File uploaded: ", new_file)
}
