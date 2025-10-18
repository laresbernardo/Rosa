robyn_fun <- function(
    exec_type, create_files, robyn_path, DS_datapath, int_use, int_sign, ts_val, trials, iters, date, sales, dep_var_type, conversion_units,
    currency, country, prophet_vars, prophet_signs, seed, month1, month2, adstock, hypers, context_vars, context_signs, paid_media_spends, paid_media_vars,
    organic_vars, organic_signs, factor_vars, JS_datapath, JS_model, steps, add_pen, calibration_input, cores, user, ...) {
  lares::tic("robyn_fun")
  x <- NULL
  if (exec_type == 1) {
    x <- mmm_wrapper(
      create_files, robyn_path, DS_datapath, int_use, int_sign, ts_val, trials, iters, date, sales, dep_var_type, conversion_units,
      currency, country, prophet_vars, prophet_signs, seed, month1, month2, adstock, hypers, context_vars, context_signs, paid_media_spends,
      paid_media_vars, organic_vars, organic_signs, factor_vars, add_pen, calibration_input, cores, user, ...
    )
  }
  # Refresh models
  if (exec_type == 2) {
    JS_model_aux <- paste0("/RobynModel-", JS_model, ".json")
    JS_datapath_aux <- gsub("\\\\", "/", JS_datapath)
    x <- robyn_ref(create_files, robyn_path, DS_datapath, JS_datapath, int_use, trials, iters, steps, cores, ...)
  }
  # Recreate a model
  if (exec_type == 3) {
    JS_model_aux <- paste0("/RobynModel-", JS_model, ".json")
    JS_datapath_aux <- gsub("\\\\", "/", JS_datapath)
    x <- robyn_sce_rec(create_files, robyn_path, DS_datapath, JS_datapath, user)
  }
  # # Kill any clusters created
  # doParallel::stopImplicitCluster()
  return(x)
}

mmm_wrapper <- function(
    create_files, robyn_directory, dataset, intercept,
    intercept_sign, ts_validation, trials, iterations,
    date, sales, dep_var_type, conversion_units, currency, country,
    prophet_vars, prophet_signs, seed, window_start, window_end, adstock, hypers,
    context_vars, context_signs, paid_media_spends,
    paid_media_vars, organic_vars, organic_signs, factor_vars,
    add_penalty_factor, calibration_input, cores = 20, user,
    return_inputs = FALSE, ...) {
  lares::tic("mmm_wrapper")
  InputCollect <- robyn_inputs(
    dt_input = dataset,
    dt_holidays = Robyn::dt_prophet_holidays,
    date_var = date,
    dep_var = sales,
    dep_var_type = dep_var_type,
    prophet_vars = prophet_vars,
    prophet_signs = unlist(prophet_signs),
    prophet_country = country,
    context_vars = context_vars,
    context_signs = unlist(context_signs),
    paid_media_spends = paid_media_spends,
    paid_media_vars = paid_media_vars,
    organic_vars = organic_vars,
    organic_signs = unlist(organic_signs),
    factor_vars = factor_vars,
    window_start = window_start,
    window_end = window_end,
    adstock = adstock,
    calibration_input = calibration_input,
    ...
  )

  if (return_inputs) {
    InputCollect$hyperparameters <- hypers
    InputCollect$dt_holidays <- NULL
    return(list(InputCollect = InputCollect, Extras = list(
      currency = isolate(currency),
      conversion_units = isolate(conversion_units),
      cores = isolate(cores),
      iterations = isolate(iterations),
      trials = isolate(trials),
      intercept = isolate(intercept),
      intercept_sign = isolate(intercept_sign),
      ts_validation = isolate(ts_validation),
      add_penalty_factor = isolate(add_penalty_factor),
      ...
    )))
  }

  # Add hyperparameters and let Robyn run feature engineering
  InputCollect <- robyn_inputs(InputCollect = InputCollect, hyperparameters = hypers)
  print(InputCollect)

  #### Step 3: Build initial model
  # Run all trials and iterations. Use ?robyn_run to check parameter definition
  OutputModels <- robyn_run(
    InputCollect = InputCollect, # feed in all model specification
    cores = isolate(cores),
    iterations = isolate(iterations), # 2000 recommended for the dummy dataset with no calibration
    trials = isolate(trials), # 5 recommended for the dummy dataset
    intercept = isolate(intercept),
    intercept_sign = isolate(intercept_sign),
    ts_validation = isolate(ts_validation), # 3-way-split time series for NRMSE validation.
    add_penalty_factor = isolate(add_penalty_factor), # Experimental feature. Use with caution.
    seed = isolate(seed),
    quiet = FALSE, ...
  )

  # Calculate Pareto fronts, cluster and export results and plots. See ?robyn_outputs
  if (!is.null(robyn_directory)) robyn_directory <- getwd()
  OutputCollect <- robyn_outputs(
    InputCollect, OutputModels,
    pareto_fronts = "auto", # automatically pick how many pareto-fronts to fill min_candidates (100)
    clusters = TRUE, # Set to TRUE to cluster similar models by ROAS. See ?robyn_clusters
    wss_var = 0.06, # More than default 7% to reduce too many clusters
    plot_pareto = FALSE, # Manually exporting it later: 7X faster
    # csv_out = if (is_server()) NULL else "pareto",
    export = create_files, # this will create files locally
    plot_folder = robyn_directory, # path for plots exports and files creation
    quiet = FALSE
  )
  print(OutputCollect)

  message(">>> Calculating default scores for models selection...")
  if (length(OutputCollect$allSolutions) > 1) {
    modelselect <- robyn_modelselector(
      InputCollect, OutputCollect,
      metrics = c(
        "rsq_train", "performance", "non_zeroes", "incluster_models"
      ),
      wt = c(3, 1, 0.5, 0.1),
      top = 4,
      cache_dir = tempdir(),
      quiet = FALSE
    )
  } else {
    modelselect <- list(
      data = data.frame(solID = OutputCollect$OutputCollect$allSolutions),
      plot = lares::noPlot("Only 1 model available. Skip this step.")
    )
  }

  message(paste("Folder:", OutputCollect$plot_folder))
  json <- robyn_write(
    InputCollect, OutputCollect,
    raw_data = InputCollect$dt_input,
    model_selection = list(
      metrics = modelselect$metrics,
      weights = modelselect$weights,
      baseline_ref = modelselect$baseline_ref
    ),
    export = FALSE, quiet = TRUE
  )

  return(list(
    OutputCollect = OutputCollect,
    InputCollect = InputCollect,
    modelselect = modelselect,
    Folder = OutputCollect$plot_folder,
    json = json
  ))
}

# Budget allocator (.json file)
robyn_sce <- function(InputCollectS, OutputCollectS, select_modelS, create_files,
                      date_range = "all", budget = 100,
                      ch_low = 1, ch_up = 1, ch_c = 3,
                      target = 100, target_upper = Inf) {
  if (length(OutputCollectS$allSolutions) == 1) select_modelS <- NULL
  # Max_Response
  AllocatorCollect1 <- robyn_allocator(
    InputCollect = InputCollectS,
    OutputCollect = OutputCollectS,
    select_model = select_modelS,
    scenario = "max_response",
    date_range = date_range,
    total_budget = budget,
    channel_constr_low = ch_low,
    channel_constr_up = ch_up,
    channel_constr_multiplier = ch_c,
    export = create_files,
    quiet = FALSE
  )
  # Target_Efficiency
  AllocatorCollect2 <- robyn_allocator(
    InputCollect = InputCollectS,
    OutputCollect = OutputCollectS,
    select_model = select_modelS,
    scenario = "target_efficiency",
    date_range = date_range,
    target_value = target,
    channel_constr_low = 0.1,
    channel_constr_up = target_upper,
    export = create_files,
    quiet = FALSE
  )
  return(list(
    AllocatorCollect1 = AllocatorCollect1,
    AllocatorCollect2 = AllocatorCollect2
  ))
}

# REFRESH MODEL with new data
robyn_ref <- function(create_files, robyn_directory, dataset, json_file,
                      intercept, trials, iterations, steps, cores, ...) {
  tryCatch(
    {
      RobynRefresh <- robyn_refresh(
        json_file = json_file,
        dt_input = dataset,
        dt_holidays = Robyn::dt_prophet_holidays,
        refresh_steps = steps,
        refresh_mode = "manual",
        refresh_iters = iterations,
        refresh_trials = trials,
        plot_folder = robyn_directory,
        plot_pareto = FALSE,
        intercept = intercept,
        cores = cores, ...
      )
    },
    error = function(e) {
      message(e)
    }
  )

  InputCollectX <- RobynRefresh$listRefresh1$InputCollect
  OutputCollectX <- RobynRefresh$listRefresh1$OutputCollect
  OutputCollectX$allSolutions <- OutputCollectX$selectID
  OutputCollectX$mediaVecCollect <- RobynRefresh$listRefresh1$ReportCollect$mediaVecReport
  print(OutputCollectX)
  message(paste("Folder:", OutputCollectX$plot_folder))
  return(list(
    InputCollect = InputCollectX,
    OutputCollect = OutputCollectX,
    PrevModel = RobynRefresh$listInit,
    modelselect = list(
      data = data.frame(solID = OutputCollectX$selectID),
      plot = lares::noPlot("Only refresh model available. Skip this step.")
    ),
    Folder = OutputCollectX$plot_folder
  ))
}

robyn_sce_rec <- function(create_files, robyn_directory, dataset, json_file, user) {
  tic("robyn_sce_rec")
  # Recreate the model
  RobynRecreated <- robyn_recreate(
    json_file = json_file,
    dt_input = dataset,
    dt_holidays = Robyn::dt_prophet_holidays,
    plot_folder = robyn_directory
  )
  InputCollectS <- RobynRecreated$InputCollect
  OutputCollectS <- RobynRecreated$OutputCollect
  select_modelS <- RobynRecreated$OutputCollect$selectID
  json <- robyn_write(
    InputCollectS, OutputCollectS,
    export = create_files,
    dir = OutputCollectS$plot_folder, raw_data = InputCollectS$dt_input
  )
  return(list(
    OutputCollect = OutputCollectS, InputCollect = InputCollectS,
    Folder = OutputCollectS$plot_folder, modelselect = list(
      data = data.frame(solID = OutputCollectS$allSolutions),
      plot = lares::noPlot("Only 1 model available. Skip this step.")
    ), Extras = RobynRecreated[["Extras"]], json = json
  ))
}

Rosa_onepagers <- function(
    InputCollect, OutputCollect,
    select_model = NULL, model_name = NULL,
    baseline_level = 0, export = FALSE,
    report_start = InputCollect$window_start,
    report_end = InputCollect$window_end,
    enable_costs = FALSE, ...) {
  OutputCollect$cores <- 1
  reporting_dates <- sprintf("**Reporting dates: %s to %s", report_start, report_end)
  pg <- robyn_onepagers(
    InputCollect, OutputCollect,
    select_model = select_model,
    quiet = FALSE, export = FALSE,
    baseline_level = baseline_level,
    ...
  )
  OutputCollect$xDecompVecCollect <- OutputCollect$xDecompVecCollect %>%
    filter(.data$ds >= report_start, .data$ds <= report_end)
  tsd <- decomp_plot(
    InputCollect, OutputCollect, select_model,
    baseline_level = baseline_level
  )

  # Include exposure metrics incremental cost
  if (isTRUE(!all(InputCollect$paid_media_vars == InputCollect$paid_media_spends)) &&
    enable_costs == TRUE) {
    spends <- InputCollect$dt_input %>%
      rename_at(InputCollect$date_var, function(x) "ds") %>%
      filter(.data$ds >= report_start, .data$ds <= report_end) %>%
      select(all_of(InputCollect$paid_media_spends)) %>%
      tidyr::gather() %>%
      group_by(.data$key) %>%
      summarise(spend = sum(.data$value), .groups = "drop") %>%
      arrange(desc(.data$spend)) %>%
      rename("variable" = "key")
    impact <- tsd$data %>%
      group_by(.data$variable) %>%
      summarize(return = sum(.data$value), .groups = "drop") %>%
      # mutate(impactP = .data$return / sum(.data$return)) %>%
      filter(.data$variable %in% InputCollect$all_media) %>%
      arrange(desc(abs(.data$return)))
    exposures <- InputCollect$dt_input %>%
      rename_at(InputCollect$date_var, function(x) "ds") %>%
      filter(.data$ds >= report_start, .data$ds <= report_end) %>%
      select(all_of(InputCollect$all_media)) %>%
      tidyr::gather() %>%
      group_by(.data$key) %>%
      summarise(actions = sum(.data$value), .groups = "drop") %>%
      arrange(desc(.data$actions)) %>%
      rename("variable" = "key")
    rpas <- impact %>%
      left_join(spends, "variable") %>%
      left_join(exposures, "variable") %>%
      filter(!is.na(.data$actions)) %>%
      mutate(rpa = .data$return / .data$actions)
    gt_table <- rpas %>%
      mutate(
        "Channel" = .data$variable,
        "Spent" = ifelse(is.na(.data$spend), "", sprintf(
          "%s (%s)",
          formatNum(.data$spend, abbr = TRUE, signif = 3),
          formatNum(100 * .data$spend / sum(.data$spend, na.rm = TRUE), 1, pos = "%")
        )),
        "Returned" = sprintf(
          "%s (%s)",
          formatNum(.data$return, abbr = TRUE, signif = 3),
          formatNum(100 * .data$return / sum(.data$return), 1, pos = "%")
        )
      ) %>%
      rowwise() %>%
      mutate(
        "Performance" = ifelse(is.na(.data$spend), "", ifelse(
          InputCollect$dep_var_type == "revenue",
          formatNum(.data$return / .data$spend, signif = 3),
          formatNum(.data$spend / .data$return, signif = 3)
        )),
        "Actions" = formatNum(.data$actions, abbr = TRUE, signif = 3),
        "RPA" = formatNum(.data$rpa, abbr = TRUE, signif = 3)
      ) %>%
      rename_at("Performance", function(x) ifelse(InputCollect$dep_var_type == "revenue", "ROAS", "CPA")) %>%
      select(which(colnames(.) == "Channel"):ncol(.)) %>%
      gt::gt() %>%
      tab_footnote(
        footnote = gsub("\\*", "", reporting_dates)
      ) %>%
      tab_footnote(
        footnote = paste(
          ifelse(
            InputCollect$dep_var_type == "revenue",
            "ROAS: Return On Ad Spend", "CPA: Cost per Acquisition"
          ),
          "; RPA: Return per Action"
        )
      )
    # tab_options(table.width = "90%")
    pg[[1]][[2]][[1]] <- wrap_table(gt_table, panel = "rows", space = "fixed")
  } else {
    pg[[1]][[2]][[1]] <- GeMMMa_shares_plot(pg[[1]][[2]][[1]])
  }

  # Add timeseries decomposition to one-pager
  onepager <- pg[[1]] +
    free(tsd + theme(legend.key.size = unit(2, "mm"), legend.text = element_text(size = 10))) +
    plot_layout(design = c("\nA\nE\nB\nC\nD\n"), guides = "auto")

  # Add reference performance to CI plot (before changing reporting window data)
  onepager[[2]][[2]]$data <- onepager[[2]][[2]]$data %>%
    left_join(select(onepager[[2]][[1]]$data, "rn", "Performance"), "rn") %>%
    mutate(rn = factor(.data$rn, levels = rev(onepager[[2]][[1]]$data$rn)))
  onepager[[2]][[2]] <- onepager[[2]][[2]] +
    geom_point(
      aes(x = .data$rn, y = .data$Performance),
      color = "red", alpha = 0.9, shape = 8, size = 2
    )

  # If reporting date range is not null, modify date sensitive plots
  if (report_start != InputCollect$window_start || report_end != InputCollect$window_end) {
    temp <- robyn_performance(
      InputCollect, OutputCollect, report_start, report_end,
      select_model,
      carryovers = TRUE, non_promo = TRUE
    )
    od <- onepager[[1]][[1]]$data
    baseline_df <- NULL
    if (baseline_level > 0) {
      baseline_df <- temp[!temp$channel %in% od$rn, ] %>%
        filter(!.data$channel %in% c("PROMOTIONAL TOTAL", "BASELINE", "GRAND TOTAL")) %>%
        summarise(
          rn = paste0("Baseline_L", baseline_level),
          xDecompAgg = sum(.data$response),
          xDecompPerc = sum(.data$contribution)
        )
    }
    # Decomposition plot
    new_data <- temp %>%
      select(rn = .data$channel, xDecompAgg = .data$response, xDecompPerc = .data$contribution) %>%
      filter(.data$rn %in% od$rn) %>%
      bind_rows(baseline_df) %>%
      arrange(.data$xDecompPerc) %>%
      mutate(
        end = 1 - cumsum(.data$xDecompPerc),
        start = lag(.data$end),
        start = ifelse(is.na(.data$start), 1, .data$start),
        id = row_number(),
        rn = as.factor(as.character(.data$rn)),
        sign = as.factor(ifelse(.data$xDecompPerc >= 0, "Positive", "Negative"))
      )
    onepager[[1]][[1]] <- ggplot(new_data, aes(x = .data$id, fill = .data$sign)) +
      geom_rect(aes(
        xmin = .data$id - 0.45, xmax = .data$id + 0.45,
        ymin = .data$end, ymax = .data$start
      ), stat = "identity") +
      scale_y_percent() +
      scale_x_continuous(breaks = seq_along(new_data$rn), labels = new_data$rn) +
      scale_fill_manual(values = c("Positive" = "#59B3D2", "Negative" = "#E5586E")) +
      theme_lares(background = "white", legend = "top") +
      geom_text(mapping = aes(
        label = paste0(
          formatNum(.data$xDecompAgg, abbr = TRUE),
          "\n", round(.data$xDecompPerc * 100, 1), "%"
        ),
        y = rowSums(cbind(.data$end, .data$xDecompPerc / 2))
      ), fontface = "bold", lineheight = .7) +
      coord_flip() +
      labs(
        title = "Response Decomposition Waterfall for Reporting Window**",
        x = NULL, y = NULL, fill = "Sign"
      )

    # Adapt Share of spend, effect, ROAS (using Robyn's outputs plot)
    onepager[[2]][[1]] <- GeMMMa_shares_plot(onepager[[2]][[1]], new_perf = temp)
    onepager[[2]][[1]]$labels$title <- gsub(
      "Modeling Window", "Reporting Window*", onepager[[2]][[1]]$labels$title
    )

    # Adapt carryover percentage plot
    onepager[[3]][[2]]$data <- robyn_immcarr(
      InputCollect, OutputCollect, select_model, report_start, report_end, ...
    ) %>%
      mutate(".group" = as.integer(as.factor(.data$rn)))
    onepager[[3]][[2]]$labels$title <- paste0(onepager[[3]][[2]]$labels$title, "**")

    # Add new caption
    onepager$patches$annotation$caption <- paste(
      reporting_dates, onepager$patches$annotation$caption,
      sep = "\n"
    )

    # Add MAPE error to fitted vs residual plot
    onepager[[4]][[2]]$data <- onepager[[4]][[2]]$data %>%
      filter(.data$ds >= report_start, .data$ds <= report_end)
    onepager[[4]][[2]] <- onepager[[4]][[2]] +
      labs(title = "Fitted vs. Residual for Reporting Window**")
  }

  # Add MAPE error to fitted vs residual plot
  res <- onepager[[4]][[2]]$data
  mape <- mean(abs((res$actual - res$predicted) / res$actual)) * 100
  onepager[[4]][[2]] <- onepager[[4]][[2]] +
    labs(y = sprintf("Residual (MAPE = %s%%)", signif(mape, 3)))

  # When weibull, no need to align
  if (grepl("weibull", InputCollect$adstock)) {
    onepager[[3]][[1]] <- free(onepager[[3]][[1]])
  }

  # Desalign response curves too
  onepager[[4]][[1]] <- free(onepager[[4]][[1]])

  if (export) {
    # To facilitate dashboard use-case with personalized model names and longer OP given tsd plot
    if (is.null(model_name)) model_name <- select_model
    filename <- paste0(OutputCollect$plot_folder, model_name, ".png")
    message("Exporting one-pager: ", filename)
    ggsave(
      filename = filename, plot = onepager,
      limitsize = FALSE, dpi = 300, width = 17, height = 25,
      create.dir = TRUE
    )
  }
  return(onepager)
}

# New Shares and Performance for Spend Channels plot
GeMMMa_shares_plot <- function(p, new_perf = NULL) {
  if (!"Performance" %in% names(p$data)) {
    shares <- p$data %>%
      left_join(select(p$layers[[3]]$data, "rn", "Performance" = "value"), by = "rn") %>%
      tidyr::pivot_wider(names_from = "variable", values_from = "value") %>%
      arrange(desc(.data$`Spend Share`)) %>%
      mutate(rn = factor(.data$rn, levels = rev(.data$rn)))
    metric <- ifelse(p$layers[[3]]$data$variable[1] == "roi_total", " ROAS", " CPA")
    allvals <- p$data$value
  } else {
    shares <- new_perf %>%
      filter(.data$channel %in% p$data$rn) %>%
      rename("rn" = "channel", "Performance" = "performance") %>%
      mutate(
        `Spend Share` = round(.data$spend / sum(.data$spend), 2),
        `Effect Share` = round(.data$response / sum(.data$response), 2)
      ) %>%
      arrange(desc(.data$`Spend Share`)) %>%
      mutate(rn = factor(.data$rn, levels = rev(.data$rn)))
    metric <- ifelse(grepl("ROAS", p$labels$title), " ROAS", " CPA")
    allvals <- c(shares$`Spend Share`, shares$`Effect Share`)
  }
  # Some global settings
  colours <- c("Spend Share" = "orange", "Effect Share" = "blue", "Perf" = "#03396C")
  names(colours)[3] <- metric
  ySecScale <- 1.1 * max(shares$Performance) / max(allvals)
  # New Barbell Chart
  shares %>%
    ggplot(aes(y = .data$rn)) +
    # Add performance below everything
    geom_point(
      aes(y = .data$rn, x = .data$Performance / ySecScale, group = 1, color = metric),
      inherit.aes = FALSE, size = 3.5
    ) +
    geom_text(
      aes(
        y = .data$rn, x = .data$Performance / ySecScale, group = 1,
        label = formatNum(.data$Performance, signif = 3)
      ),
      inherit.aes = FALSE, color = "#03396C", vjust = -1, size = 3.5, fontface = "bold"
    ) +
    # Add lines
    geom_segment(
      aes(x = .data$`Spend Share`, xend = .data$`Effect Share`, y = .data$rn, yend = .data$rn),
      color = "grey", size = 1
    ) +
    # Add points
    geom_point(aes(x = .data$`Spend Share`, color = "Spend Share"), size = 4) +
    geom_point(aes(x = .data$`Effect Share`, color = "Effect Share"), size = 4) +
    # Add labels for "Spend Share" values
    geom_text(
      aes(
        x = .data$`Spend Share`,
        label = formatNum(100 * .data$`Spend Share`, 1, pos = "%"),
        hjust = ifelse(.data$`Spend Share` < .data$`Effect Share`, 0.75, 0.2),
        color = "Spend Share"
      ),
      vjust = 1.9,
      size = 3, fontface = "bold"
    ) +
    # Add labels for "Effect Share" values
    geom_text(
      aes(
        x = .data$`Effect Share`,
        label = formatNum(100 * .data$`Effect Share`, 1, pos = "%"),
        hjust = ifelse(.data$`Effect Share` > .data$`Spend Share`, 0.2, 0.75),
        vjust = 1.9,
        color = "Effect Share"
      ),
      size = 3, fontface = "bold"
    ) +
    # Customizing the theme and legend
    scale_color_manual(values = colours, breaks = names(colours)) +
    labs(
      title = paste0("Share of Spend, Effect &", metric, " in Modeling Window*"),
      x = "Share Value", y = NULL, color = NULL
    ) +
    lares::theme_lares(legend = "top", grid = "Y") +
    lares::scale_x_percent(limits = c(min(allvals) - 0.1, max(allvals) * 1.1))
}

robyn_responses <- function(
    InputCollect, OutputCollect,
    select_model = NULL, channel,
    start_date = NULL, end_date = NULL,
    sim_total = NULL, marginal_unit = 1,
    plot = TRUE) {
  stopifnot(length(channel) == 1)
  check_opts(channel, InputCollect$all_media)

  # Set default start and end dates
  if (is.null(start_date)) start_date <- min(InputCollect$dt_mod$ds)
  if (is.null(end_date)) end_date <- max(InputCollect$dt_mod$ds)

  # Set default model if only one available
  if (is.null(select_model)) select_model <- OutputCollect$allSolutions
  stopifnot(length(select_model) == 1)
  check_opts(select_model, OutputCollect$allSolutions)

  # Filter data for the specified window
  window <- InputCollect$dt_mod %>%
    mutate(rowid = seq(nrow(.))) %>%
    filter(.data$ds >= as.Date(start_date), .data$ds <= as.Date(end_date)) %>%
    pull(.data$rowid)
  stopifnot(length(window) >= 1)

  # Prepare model coefficients and parameters
  temp <- robyn_write(InputCollect, OutputCollect, select_model = select_model, export = FALSE)
  hyps <- temp$ExportedModel$hyper_values
  coeff <- temp$ExportedModel$summary$coef[temp$ExportedModel$summary$variable == channel]
  alpha <- hyps[[paste0(channel, "_alphas")]]
  gamma <- hyps[[paste0(channel, "_gammas")]]
  theta <- hyps[[paste0(channel, "_thetas")]]
  shape <- hyps[[paste0(channel, "_shapes")]]
  scale <- hyps[[paste0(channel, "_scales")]]
  adstock <- InputCollect$adstock

  # Apply adstock transformation to the historical input data
  input_hist <- InputCollect$dt_mod[[channel]]
  ads <- Robyn::transform_adstock(input_hist, adstock, theta, shape, scale)
  # Apply saturation transformation to the adstocked data
  sat <- Robyn::saturation_hill(ads$x_decayed, alpha, gamma, x_marginal = NULL)

  # Helper function to calculate response
  calculate_response <- function(input_metric, sat) {
    coeff * ((1 + sat$inflexion^alpha / input_metric^alpha)^-1)
  }

  # Helper function to create data points
  create_point <- function(metric, response, note = NA) {
    data.frame(metric = metric, response = response, note = note)
  }

  # Calculate response for mean spend/impressions
  input_total <- ads$x_decayed
  input_immediate <- if (adstock == "weibull_pdf") ads$x_imme else ads$x
  input_carryover <- input_total - input_immediate
  mean_input_immediate <- mean(input_immediate[window])
  mean_input_carryover <- mean(input_carryover[window])
  mean_input_total <- mean_input_immediate + mean_input_carryover
  mean_response_total <- calculate_response(mean_input_total, sat)
  point_mean <- create_point(mean_input_total, mean_response_total, "mean")
  # Calculate response for mean carryover
  mean_response_carryover <- calculate_response(mean_input_carryover, sat)
  point_cvr <- create_point(mean_input_carryover, mean_response_carryover, "mean_carryover")
  # Calculate response in inflexion point
  point_inf <- create_point(sat$inflexion, calculate_response(sat$inflexion, sat), "inflexion")

  # Calculate marginal response
  mean_metric_marg <- mean(input_total[window]) + marginal_unit
  mean_response_marg <- calculate_response(mean_metric_marg, sat)
  point_mgl <- create_point(mean_metric_marg, mean_response_marg, "mean_marginal")

  # Performance metrics calculation
  calculate_performance <- function(input_metric, input_response, marginal_metric, marginal_response, dep_var_type) {
    if (dep_var_type == "revenue") {
      perf_metric <- "ROAS"
      perf <- input_response / input_metric
      perf_marg <- (marginal_response - input_response) / (marginal_metric - input_metric)
    } else {
      perf_metric <- "CPA"
      perf <- input_metric / input_response
      perf_marg <- (marginal_metric - input_metric) / (marginal_response - input_response)
    }
    list(perf_metric = perf_metric, perf = perf, perf_marg = perf_marg)
  }

  # Mean point performance calculation
  mean_performance <- calculate_performance(
    input_metric = mean_input_total,
    input_response = point_mean$response,
    marginal_metric = mean_metric_marg,
    marginal_response = mean_response_marg,
    dep_var_type = InputCollect$dep_var_type
  )

  # Simulated point calculations if sim_total is provided
  point_sim <- point_sim_mgl <- sim_performance <- NULL
  if (!is.null(sim_total) && sim_total >= 0) {
    sim_raw <- sim_total / length(window)
    sim_input <- sim_raw + mean(input_carryover[window])
    # ads_sim <- Robyn::transform_adstock(sim_input, adstock, theta, shape, scale)
    # sim_total_x <- ads_sim$x_decayed
    # sim_immediate <- if (adstock == "weibull_pdf") ads_sim$x_imme else ads_sim$x
    # sim_carryover <- sim_total_x - sim_immediate
    # sim_input_immediate <- mean(sim_immediate[window])
    # sim_input_carryover <- mean(sim_carryover[window])
    # sim_input_total <- sim_input_immediate + sim_input_carryover
    sim_response_total <- calculate_response(sim_input, sat)
    point_sim <- create_point(sim_input, sim_response_total, "simulated")

    sim_metric_marg <- sim_input + marginal_unit
    sim_response_marg <- calculate_response(sim_metric_marg, sat)
    point_sim_mgl <- create_point(sim_metric_marg, sim_response_marg, "simulated_marginal")

    sim_performance <- calculate_performance(
      input_metric = sim_input,
      input_response = point_sim$response,
      marginal_metric = sim_metric_marg,
      marginal_response = sim_response_marg,
      dep_var_type = InputCollect$dep_var_type
    )
  }

  # Create saturation curve data
  dt_line <- create_point(metric = ads$x_decayed, response = sat$x_saturated * coeff) %>%
    bind_rows(point_inf, point_mean, point_cvr, point_mgl, point_sim, point_sim_mgl) %>%
    arrange(.data$metric)

  # Plot generation
  p <- NULL
  if (plot) {
    p <- dt_line %>%
      mutate(alpha = ifelse(row_number() %in% window, 1, 0.1)) %>%
      ggplot(aes(x = .data$metric, y = .data$response)) +
      geom_line(color = "black", alpha = 0.5) +
      scale_x_abbr() +
      scale_y_abbr() +
      theme_lares(legend = "top") +
      labs(
        colour = NULL,
        title = paste(
          "Saturation curve for",
          ifelse(channel %in% InputCollect$paid_media_spends, "paid", "organic"),
          "channel:", channel
        ),
        x = sprintf("Input per %s (%s)", InputCollect$intervalType, channel),
        y = sprintf("Response per %s (%s)", InputCollect$intervalType, InputCollect$dep_var),
        caption = sprintf(
          "Considered from %s to %s [%s periods]",
          InputCollect$dt_mod$ds[min(window)],
          InputCollect$dt_mod$ds[max(window)],
          length(window)
        )
      )
    # Add inflexion point
    p <- p + geom_point(
      data = point_inf,
      aes(x = .data$metric, y = .data$response, colour = "Inflexion Point"), size = 2.5, shape = 4
    )
    # Add mean point and carryover area
    lab_mean <- sprintf(
      "Mean: %s @ %s (raw* %s)",
      num_abbr(point_mean$response),
      num_abbr(point_mean$metric),
      num_abbr(mean(input_hist[window]))
    )
    p <- p +
      geom_point(
        data = point_mean,
        aes(x = .data$metric, y = .data$response, colour = lab_mean), size = 2.5
      ) +
      geom_point(x = mean(input_hist[window]), y = 0, size = 2.5, shape = 8) +
      geom_area(
        data = filter(dt_line, .data$metric <= mean_input_carryover),
        aes(x = .data$metric, y = .data$response),
        stat = "align", position = "stack", linewidth = 0.1,
        fill = "grey50", alpha = 0.4, show.legend = FALSE
      )
    p$labels$caption <- sprintf(
      "%s\nMean %s carryover in period: %s",
      p$labels$caption, adstock, num_abbr(mean_input_carryover)
    )
    p$labels$caption <- sprintf(
      "%s\nMean %s: %s | Mean m%s: %s",
      p$labels$caption,
      mean_performance$perf_metric,
      num_abbr(mean_performance$perf),
      mean_performance$perf_metric,
      num_abbr(mean_performance$perf_marg)
    )
    # Add simulation performance to caption if applicable
    if (!is.null(sim_performance) && sim_total >= 0) {
      lab_sim <- sprintf(
        "Sim: %s @ %s (raw %s)",
        num_abbr(point_sim$response),
        num_abbr(point_sim$metric),
        num_abbr(sim_raw)
      )
      p <- p + geom_point(data = point_sim, aes(
        x = .data$metric, y = .data$response, colour = lab_sim
      ), size = 3.2)
      p$labels$caption <- sprintf(
        "%s\nSim %s: %s | Sim m%s: %s",
        p$labels$caption,
        sim_performance$perf_metric,
        num_abbr(sim_performance$perf),
        sim_performance$perf_metric,
        num_abbr(sim_performance$perf_marg)
      )
    }
  }

  return(list(
    data = dt_line, plot = p,
    select_model = select_model,
    channel = channel,
    start_date = start_date,
    end_date = end_date,
    sim_total = sim_total,
    marginal_unit = marginal_unit,
    mean_performance = mean_performance,
    sim_performance = sim_performance
  ))
}
