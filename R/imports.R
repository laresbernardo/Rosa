import_json_data <- function(js_file, rv, input, session) {
  df <- NULL
  if ("raw_data" %in% names(js_file)) df <- js_file$raw_data
  if ("raw_data" %in% names(js_file$Extras)) df <- js_file$Extras$raw_data
  if ("dt_input" %in% names(js_file$InputCollect)) df <- js_file$InputCollect$dt_input

  if (length(js_file$InputCollect$calibration_input) > 1) {
    message("Calibration data detected and imported...")
    rv$calibration_input <- js_file$InputCollect$calibration_input
    showNotification("Calibration data imported from JSON", type = "message")
  }
  if (!is.null(df)) {
    message("Data detected and imported...")
    rv <- process_data(df, rv, input, session)
    try(isolate(config_json(js_file, df, session)))
    rv$demo <- FALSE
    # Update refresh steps to refresh with all available data
    max_steps <- df %>%
      mutate(date = df[, js_file$InputCollect$date_var]) %>%
      filter(.data$date > js_file$InputCollect$window_end) %>%
      nrow()
    updateNumericInput(session, "steps", value = max_steps, min = 1, max = max_steps)
    showNotification("Data imported from JSON", type = "message")
  }
  return(rv)
}

config_json <- function(cf, dataset = NULL, session) {
  mylist <- append(cf$ExportedModel, cf$ModelsCollect)
  if ("Extras" %in% names(cf)) mylist <- append(mylist, cf$Extras)
  l_cores <- unlist(mylist$cores)
  updateTextInput(session, "cores", value = l_cores)
  l_ts <- unlist(mylist$ts_validation)
  updateSwitchInput(session, "ts_val", value = l_ts)
  l_in <- isTRUE(unlist(mylist$intercept))
  updateSwitchInput(session, "int_use", value = l_in)
  l_ins <- unlist(mylist$intercept_sign)
  updateSelectInput(session, "int_sign", selected = l_ins)
  l_tr <- unlist(mylist$trials)
  updateNumericInput(session, "trials", value = l_tr)
  l_it <- unlist(mylist$iterations)
  updateNumericInput(session, "iters", value = l_it)
  l_se <- unlist(mylist$seed)
  updateNumericInput(session, "seed", value = l_se)
  l_ap <- unlist(mylist$add_penalty_factor)
  updateSwitchInput(session, "add_pen", value = l_ap)
  l_co <- unlist(cf$InputCollect$prophet_country)
  updateSelectInput(session, "holidays_country", selected = l_co)
  l_ad <- unlist(cf$InputCollect$adstock)
  updateSelectInput(session, "adstock", selected = l_ad)
  l_pv <- unlist(cf$InputCollect$prophet_vars)
  updateCheckboxGroupInput(session, "prophet_vars", selected = l_pv)
  l_ps <- unlist(cf$InputCollect$prophet_signs)
  updateSelectInput(session, "prophet_signs", selected = l_ps)
  l_w <- as.Date(unlist(c(cf$InputCollect$window_start, cf$InputCollect$window_end)), format = "%Y-%m-%d", origin = "1970-01-01")
  updateSliderInput(session, "window", value = l_w, step = 1)
  l_d <- unlist(cf$InputCollect$date_var)
  updateSelectInput(session, "date", selected = l_d)
  l_s <- unlist(cf$InputCollect$dep_var)
  updateSelectInput(session, "sales", selected = l_s)
  l_st <- unlist(cf$InputCollect$dep_var_type)
  updateRadioButtons(session, "dep_var_type", selected = l_st)
  l_dpu <- unlist(cf$Extras$currency)
  updateSelectInput(session, "currency", selected = l_dpu)
  l_ma <- unlist(cf$Extras$market)
  updateSelectInput(session, "market", selected = l_ma)
  lares::quiet(try({
    l_br <- unlist(cf$Extras$brand)
    updateSelectInput(session, "brand", selected = l_br)
    updateTextInput(session, "brand", value = l_br)
  }))
  if (cf$InputCollect$dep_var_type == "conversion") {
    l_ct <- unlist(cf$Extras$conversion_units)
    updateTextInput(session, "conversion_units", value = l_ct)
  }
  l_cv <- unlist(cf$InputCollect$context_vars)
  updateSelectizeInput(session, "context_vars", selected = l_cv)
  l_cs_y <- unlist(cf$InputCollect$context_signs)
  updateSelectizeInput(session, "context_signs", choices = l_cs_y, selected = l_cs_y)
  l_ps <- unlist(cf$InputCollect$paid_media_spends)
  updateSelectizeInput(session, "paid_media_spends", selected = l_ps)
  l_pi <- rep(0, length(unlist(cf$InputCollect$paid_media_spends)))
  updateSelectizeInput(session, "paid_media_vars", selected = l_pi)
  l_o <- unlist(cf$InputCollect$organic_vars)
  updateSelectizeInput(session, "organic_vars", selected = l_o)
  l_o_y <- unlist(cf$InputCollect$organic_signs)
  updateSelectizeInput(session, "organic_signs", choices = l_o_y, selected = l_o_y)
  l_f <- unlist(cf$InputCollect$factor_vars)
  updateSelectizeInput(session, "factor_vars", selected = l_f)
  l_t <- unlist(cf$InputCollect$hyperparameters$train_size)
  updateSliderInput(session, "train_size", value = l_t)
  if (!is.null(dataset)) {
    ch <- c(l_ps, l_o, l_cv)
    if (!all(ch %in% colnames(dataset))) {
      stop(sprintf("Missing variables: %s", v2t(ch[!ch %in% colnames(dataset)])))
    }
  }
}

process_data <- function(df, rv, input, session, query_chat = FALSE) {
  tryCatch(
    {
      ds <- tibble(df)
      check_nas(df, session)
      colnames(ds) <- gsub("\\.|\\ ", "_", colnames(ds))
      colnames(df) <- gsub("\\.|\\ ", "_", colnames(df))
      formats <- sapply(ds, class)
      dates <- c()
      nums <- c()
      contx <- c()
      contx_nn <- c()
      orgx <- c()
      for (dc in names(ds)) {
        if (sapply(ds[dc], function(x) !all(is.na(as.Date(as.character(x), format = "%Y-%m-%d"))))) {
          dates <- append(dates, dc)
        } else {
          if (is.numeric(ds[[dc]])) {
            if (!all(ds[[dc]] == 0)) {
              nums <- append(nums, dc)
              contx <- append(contx, dc)
            }
          } else {
            if (!all(ds[[dc]] == "na")) {
              contx <- append(contx, dc)
              contx_nn <- append(contx_nn, dc)
            }
          }
        }
      }
      rv$contx <- contx <- contx[contx != input$sales]
      # nums_s <- nums_i <- rv$contx
      nums_s <- contx[unlist(lapply(list(contx), function(x) endsWith(tolower(x), "_s")))]
      nums_i <- contx[unlist(lapply(list(contx), function(y) endsWith(tolower(y), "_i")))]
      rv$nums_ns <- nums_ns <- nums[nums != input$sales]
      # If user didn't use the _S nomenclature for spends, enable all numeric
      if (length(nums_s) == 0) nums_s <- nums_ns
      if (length(nums_i) == 0) nums_i <- nums_ns
      orgx <- nums_ns[unlist(lapply(list(nums_ns), function(x) !endsWith(tolower(x), "_s")))]
      orgx <- orgx[unlist(lapply(list(orgx), function(y) !endsWith(tolower(y), "_i")))]
      # UPDATE SelectInputs
      updateSelectInput(session, "date", choices = dates, selected = dates[1])
      updateSelectInput(session, "sales", choices = nums, selected = nums[1])
      rv$adv_cou <- 0
      # UPDATE AND CLEAN SELECTIZE for CONTEXT, PAID_MEDIA, ORGANIC & FACTOR
      updateSelectizeInput(session, "paid_media_spends", choices = nums_s, selected = NULL)
      updateSelectizeInput(session, "paid_media_vars", choices = nums_ns, selected = NULL)
      updateSelectizeInput(session, "organic_vars", choices = orgx, selected = NULL)
      updateSelectizeInput(session, "context_vars", choices = contx, selected = NULL)

      # Update Min y Max dates for Window
      dates_vector <- unlist(sapply(ds[dates[1]], function(x) {
        format(as.Date(x, format = "%Y-%m-%d", origin = "1970-01-01"), "%Y-%m-%d")
      }))
      updateSliderInput(session, "window",
        min = as.Date(min(dates_vector, na.rm = TRUE), "%Y-%m-%d"),
        max = as.Date(next_date(dates_vector) - 1, "%Y-%m-%d"),
        value = c(
          as.Date(dates_vector[round(length(dates_vector) * 0.07, 0)], "%Y-%m-%d"),
          as.Date(next_date(dates_vector) - 1, "%Y-%m-%d")
        ),
        step = 1
      )

      # Budget Allocator values
      start_date <- as.Date(min(dates_vector, na.rm = TRUE), "%Y-%m-%d", origin = "1970-01-01")
      end_date <- as.Date(max(dates_vector, na.rm = TRUE), "%Y-%m-%d", origin = "1970-01-01")
      rv$datesdisabled <- as.Date(start_date:end_date, origin = "1970-01-01")[
        !as.Date(start_date:end_date, origin = "1970-01-01") %in% as.Date(dates_vector, origin = "1970-01-01")
      ]
      if (length(rv$datesdisabled) == 0) rv$datesdisabled <- NULL
      # Default range to cover a year (based on intervalType)
      dayInterval <- as.integer(difftime(dates_vector[2], dates_vector[1], units = "days"))
      intervalType <- dplyr::case_match(dayInterval, 1 ~ "day", 7 ~ "week", dayInterval ~ "month")
      minus_year <- as.Date(
        dplyr::case_match(
          intervalType,
          "month" ~ end_date - months(11),
          intervalType ~ end_date - 365
        ),
        origin = "1970-01-01"
      )
      updateDateRangeInput(session, "date_range",
        start = minus_year, end = end_date,
        min = start_date, max = end_date
      )
      sum_spend <- 0
      for (ch in nums_s) sum_spend <- sum_spend + sum(ds[[ch]])
      updateAutonumericInput(session, "budget", value = round(sum_spend, 0))

      if (!query_chat) {
        rv$dataset <- ds
        rv$dataframe <- df
      }

      # INTERVAL_TYPE needed
      rv$intervalType <- paste0(intervalType, "ly", collapse = "")
      rv$intervalType <- gsub("dayly", "daily", rv$intervalType)

      # SETUP HYPERPARAMETERS
      rv <- setup_hyps_levers(input, rv, NULL)
      rv$demo <- TRUE # To treat as demo and don't overwrite hyperparameters
    },
    error = function(e) {
      showNotification(paste("Error loading the dataset:", e), "", type = "error")
      return(NULL)
    }
  )
  return(invisible(rv))
}
