log_action <- function(action, extra = "", elapsed = 0, upload = TRUE, callr = FALSE, ...) {
  tryCatch(
    {
      custom <- list(...)
      df <- data.frame(
        ts = Sys.time(),
        session_id = session_id,
        user = Sys.info()[["user"]],
        action = action,
        extra = as.character(jsonlite::toJSON(extra)),
        elapsed = elapsed,
        Rosa = as.character(packageVersion("Rosa")),
        Robyn = as.character(packageVersion("Robyn")),
        server = Sys.info()[["nodename"]]
      )
      if (any(names(custom) %in% colnames(df))) {
        for (i in names(custom)) {
          df[[i]] <- custom[[i]]
        }
      }
      if (upload && Sys.getenv("AZURE_STORAGE_KEY") != "") {
        message(sprintf(">>> Logging %s results%s...", action, ifelse(callr, " using callr", "")))
        if (callr) {
          try_require("callr")
          p <- r_bg(func = function(x) {
            azure_append_log(x)
            return(x)
          }, args = list(x = df), supervise = TRUE)
          return(invisible(p))
        } else {
          p <- azure_append_log(df)
        }
      }
    },
    error = function(e) {
      message("Error logging: ", e)
    }
  )
  return(invisible(df))
}
