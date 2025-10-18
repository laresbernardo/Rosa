process_bundle <- function(filename, name, path = tempdir(), project = "Dashboard", save = FALSE) {
  withProgress(message = "Rebuilding:", value = 0, {
    n <- length(filename) + 2 + save
    name <- gsub("\\.json", "", name)

    # Recreate all models
    recreated <- suppressWarnings(
      lapply(filename, function(x) {
        dat <- jsonlite::read_json(x)
        incProgress(1 / n, detail = paste(dat$ExportedModel$select_model, "model"))
        ret <- robyn_recreate(
          x, bind_rows(dat$Extras$raw_data),
          plot_folder = path,
          plot_folder_sub = "Dashboard/",
          cores = 1,
          quiet = TRUE
        )
        return(ret)
      })
    )
    dash_sols <- unlist(lapply(recreated, function(x) x$OutputCollect$allSolutions))
    names(recreated) <- sprintf("%s (%s)", name, dash_sols)

    # Check that all models have the same performance metric
    dep_var_types <- unique(unlist(lapply(recreated, function(x) x$InputCollect$dep_var_type)))
    if (length(dep_var_types) != 1) {
      # e <- "You may only compare models with same dep_var_type (revenue or conversion)"
      # showNotification(e, type = "error", duration = NULL)
      # message(e)
      # out <- NULL
      # return(out)
      dep_var_types <- "Mixed"
    }

    # Recreate all one-pagers
    onepagers <- NULL
    if (FALSE) {
      message(">>> Recreating one-pagers...")
      dir.create(paste0(path, "/Dashboard"), showWarnings = FALSE)
      rc_ops <- suppressWarnings(
        lapply(seq_along(filename), function(x) {
          incProgress(1 / n, detail = paste(dash_sols[x], "plots"))
          dir <- recreated[[x]]$OutputCollect$plot_folder
          file <- fix_double_slash(paste0(dir, "/", dash_sols[x], ".png"))
          new_file <- fix_double_slash(sprintf("%s/%s (%s).png", dir, name[x], dash_sols[x]))
          temp <- file.rename(file, new_file)
          message(">> Recreating One-Pager: ", new_file)
          onepager <- Rosa_onepagers(
            recreated[[x]]$InputCollect, recreated[[x]]$OutputCollect,
            export = TRUE, quiet = TRUE
          )
        })
      )
    }
    onepagers <- fix_double_slash(sprintf(
      "%s/%s (%s).png", recreated[[1]]$OutputCollect$plot_folder, name, dash_sols
    ))
    out <- list(recreated = recreated, solIDs = dash_sols, onepagers = onepagers)

    # Export results as RDS to create a new scenario
    if (save) {
      incProgress(1 / n, detail = paste("Saving", project))
      message(">>> Saving processed data and files in inst as a new saved case")
      filename <- paste0("inst/Dashboards/", gsub(" ", "_", project), ".RDS")
      if (dir.exists(dirname(filename))) {
        saveRDS(out, filename)
      } else {
        warning("Exported into current working directory instead: ", getwd())
        saveRDS(out, basename(filename))
      }
      Sys.sleep(0.5)
    }
  })
  message("Processed files successfully!")
  return(out)
}
