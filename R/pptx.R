####################################################################
#' PowerPoint Presentation for Selected Model
#'
#' @param InputCollect,OutputCollect List. Robyn's output objects.
#' @param Extras List. Additional information required.
#' Import JSON elements from \code{robyn_write()} for quick build-up.
#' @param selected_model Character. In case there are more than one.
#' @param template,master Character. PPTX template file.
#' @param filename Character. Name of the output file.
#' @param ai_insights Boolean. Add AI Generated Insights?
#' @param report_start,report_end Character date. Reporting start and end dates.
#' @param ... Additional parameters to pass to internal functions
#' @export
mmm_pptx <- function(InputCollect,
                     OutputCollect,
                     Extras = NULL,
                     selected_model = NULL,
                     template, master = "Rosa",
                     filename = "RosaReport.pptx",
                     ai_insights = FALSE,
                     report_start = NULL,
                     report_end = NULL,
                     ...) {
  tic("mmm_pptx")
  tempjson <- robyn_write(
    InputCollect, OutputCollect,
    selected_model,
    export = FALSE, quiet = TRUE,
    raw_data = Extras$raw_data,
    currency = Extras$currency,
    conversion_units = Extras$conversion_units,
    brand = Extras$brand,
    market = Extras$market,
    final_model = Extras$final_model,
    notes = Extras$add_notes,
    baseline_level = Extras$baseline_level,
    model_selection = Extras$model_selection
  )

  # AI Insights
  if (ai_insights) {
    json_ai <- tempjson
    json_ai$raw_data <- "Not included"
    reply <- try(openai_prompt(
      user_prompt = get_ai_prompt("agent_pptx", json = json_ai),
      system_prompt = get_ai_prompt("system", agent_selmod = FALSE),
      ...
    ))
    insights_ai <- paste(trimws(reply$choices[[1]]$message$content), "\n")
  }

  # Auxiliary
  fpt <- fp_text(shading.color = "yellow", font.family = "Arial", font.size = 12, bold = TRUE)
  df <- Extras$raw_data
  brand_market <- sprintf("%s (%s)", Extras$brand, Extras$market)
  kpi_name <- ifelse(InputCollect$dep_var_type == "revenue", "revenue", "conversions")
  kpi_metric <- ifelse(InputCollect$dep_var_type == "revenue", "ROAS", "CPA")
  window_start <- as.Date(InputCollect$window_start)
  window_end <- as.Date(InputCollect$window_end)
  modeling_window <- paste(
    format(window_start, "%B %d, %Y"),
    format(window_end, "%B %d, %Y"),
    sep = " to "
  )
  if (is.null(report_start)) report_start <- InputCollect$window_start
  if (is.null(report_end)) report_end <- InputCollect$window_end
  reporting_window <- paste(
    format(report_start, "%B %d, %Y"),
    format(report_end, "%B %d, %Y"),
    sep = " to "
  )
  tempdf <- df %>%
    select(
      InputCollect$date_var,
      InputCollect$dep_var,
      InputCollect$paid_media_spends
    ) %>%
    rename("date" = 1) %>%
    mutate(date = as.Date(.data$date)) %>%
    filter(.data$date >= window_start & .data$date <= window_end)
  total_spend <- sum(tempdf[, -c(1:2)])

  # Model results
  perf <- robyn_performance(
    InputCollect, OutputCollect,
    solID = selected_model,
    start_date = report_start,
    end_date = report_end,
    marginals = TRUE,
    quiet = TRUE
  )
  model_metric <- perf$performance[perf$channel == "PROMOTIONAL TOTAL"]
  rsq <- ifelse(
    isTRUE(!is.na(tempjson$ExportedModel$errors$rsq_test)),
    tempjson$ExportedModel$errors$rsq_test,
    tempjson$ExportedModel$errors$rsq_train
  )

  # Contribution plots
  baseline <- c(
    "(Intercept)", "trend", "season", "holiday", "weekday", "monthly", "weekly", "daily",
    InputCollect$context_vars
  )
  mkt_ch <- InputCollect$all_media
  contrs_mkt <- perf$contribution[perf$channel == "PROMOTIONAL TOTAL"]
  contrs_top <- perf$channel[perf$channel %in% mkt_ch]
  contrs_top2 <- sum(head(perf$contribution[perf$channel %in% mkt_ch], 2))
  contrs_txt1 <- sprintf(
    paste(
      "Promotional activities contributed to %s of the total amount of %s.",
      "The channels %s and %s contributed the most, with %s combined."
    ),
    formatNum(contrs_mkt * 100, 0, pos = "%"),
    kpi_name,
    gsub("_", " ", contrs_top[1]),
    gsub("_", " ", contrs_top[2]),
    formatNum(contrs_top2 * 100, 1, pos = "%")
  )
  contrs_txt2 <- sprintf(
    "Total baseline contribution of %s explains base factors that drive revenue. Reporting window: %s.",
    formatNum((1 - contrs_mkt) * 100, 0, pos = "%"),
    reporting_window
  )
  contrs_plot1 <- perf %>%
    mutate(
      type = ifelse(
        .data$channel %in% InputCollect$paid_media_spends, "Paid Channels",
        ifelse(.data$channel %in% InputCollect$organic_vars, "Organic", "Baseline")
      ),
      type = factor(.data$type, levels = c("Paid Channels", "Organic", "Baseline"))
    ) %>%
    filter(!grepl(" TOTAL", .data$channel)) %>%
    ungroup() %>%
    dplyr::rowwise() %>%
    mutate(
      channel = lares::autoline(gsub("_", " ", .data$channel), top = 20),
      lab = formatNum(100 * .data$contribution, signif = 2, pos = "%")
    ) %>%
    filter(.data$contribution != 0) %>%
    ggplot(aes(x = reorder(.data$channel, -abs(.data$contribution)), y = .data$contribution)) +
    geom_col(fill = "#FFAA55") +
    theme_lares(grid = "", size = 10, legend = "none") +
    geom_text(
      aes(
        y = .data$contribution,
        label = .data$lab
      ),
      vjust = -0.2, size = 3
    ) +
    scale_x_discrete(expand = c(0, 1)) +
    labs(x = NULL, y = NULL) +
    facet_grid(. ~ .data$type, space = "free", scales = "free_x") +
    coord_cartesian(clip = "off") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.line.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )

  dir_ins <- sprintf(
    "Drivers of %s %s: c.a. %s baseline, %s%% %s",
    brand_market,
    kpi_name,
    formatNum(100 - contrs_mkt * 100, 0, pos = "%"),
    round(100 * head(perf$contribution[perf$channel %in% mkt_ch], 1)),
    head(contrs_top, 1)
  )

  # Get plots from one-pager
  op <- Rosa_onepagers(
    InputCollect, OutputCollect,
    select_model = selected_model,
    report_start = report_start,
    report_end = report_end,
    export = FALSE, ...
  )
  reg_plot <- op[[1]][[2]] +
    theme_lares(background = "transparent", legend = "bottom", grid = "Yy", size = 12) +
    labs(title = NULL, x = NULL, y = cleanText(kpi_name, title = TRUE))

  # Channel performance
  chn_perf_df <- perf %>%
    filter(.data$channel %in% InputCollect$paid_media_spends) %>%
    mutate(
      resp_share = .data$response / sum(.data$response),
      spend_share = .data$spend / sum(.data$spend),
      Diff = abs(.data$resp_share - .data$spend_share)
    ) %>%
    arrange(desc(.data$Diff))
  top_organic <- perf %>%
    filter(.data$channel %in% InputCollect$organic_vars) %>%
    arrange(desc(.data$contribution)) %>%
    pull("channel") %>%
    head(1)
  no_impact <- perf %>%
    filter(
      .data$channel %in% InputCollect$all_media,
      .data$contribution <= 0
    ) %>%
    pull("channel") %>%
    v2t(and = " and ")
  chn_perf_plot <- chn_perf_df %>%
    tidyr::pivot_longer(cols = c("spend_share", "performance", "marginal")) %>%
    dplyr::rowwise() %>%
    mutate(
      labs = ifelse(
        .data$name %in% c("performance", "marginal"),
        formatNum(signif(.data$value, 3)),
        formatNum(signif(100 * .data$value, 2), pos = "%")
      ),
      name = factor(case_when(
        name == "marginal" ~ paste0("m", kpi_metric),
        name == "performance" ~ kpi_metric,
        name == "spend_share" ~ "Spend\nShare",
      ), levels = c("Spend\nShare", kpi_metric, paste0("m", kpi_metric)))
    ) %>%
    mutate(channel = lares::autoline(gsub("_", " ", .data$channel), top = 20)) %>%
    ggplot(aes(
      x = reorder(.data$channel, -.data$spend),
      y = .data$value, fill = .data$name
    )) +
    geom_col(position = position_dodge()) +
    labs(x = NULL, y = NULL, fill = NULL, caption = paste("Reporting window:", reporting_window)) +
    theme_lares(grid = "", size = 12, legend = "none") +
    coord_cartesian(clip = "off") +
    facet_grid(.data$name ~ ., scales = "free", switch = "both") +
    scale_fill_manual(values = alpha(c("orange", "grey40", "grey70"), .3)) +
    geom_text(aes(y = .data$value, label = .data$labs),
      vjust = -0.2, size = 4, position = position_dodge(width = .9)
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.line.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )

  # Budget allocator scenarios function
  allocator_results <- function(allocator) {
    dfb <- allocator$dt_optimOut
    allocator_dates <- sprintf(
      "%s to %s",
      format(as.Date(dfb$date_min[1]), "%B %d, %Y"),
      format(as.Date(dfb$date_max[1]), "%B %d, %Y")
    )
    spend <- dfb$initSpendTotal[1]
    delta <- round(dfb$optmSpendTotal[1] - spend)
    improvement <- sprintf(
      "%s (%s) per %s",
      formatNum(allocator$dt_optimOut$optmResponseTotal[1] -
        allocator$dt_optimOut$initResponseTotal[1], abbr = TRUE, sign = TRUE),
      formatNum(100 * allocator$dt_optimOut$optmResponseUnitTotalLift[1], decimals = 1, sign = TRUE, pos = "%"),
      InputCollect$intervalType
    )

    alloc_txt1 <- glued(
      "Budget increase: {deltaf} ({deltaP}%) per {intervalType}
  {dep_var_type} increase: {improvement}",
      deltaf = formatNum(delta, signif = 4, abbr = TRUE),
      deltaP = formatNum(100 * delta / spend, decimals = 1, sign = TRUE),
      intervalType = InputCollect$intervalType,
      dep_var_type = cleanText(kpi_name, title = TRUE),
      A = formatNum(
        dfb$optmResponseTotal[1] - dfb$initResponseTotal[1],
        abbr = TRUE, sign = TRUE
      ),
      B = formatNum(100 * dfb$optmResponseUnitTotalLift[1], decimals = 1, sign = TRUE)
    )

    alloc_txt2 <- sprintf(
      paste(
        "To optimize %s with the same budget, the model recommends the following budget",
        "allocation (%sly budget adjustments) based on past performances and mean spends [Period: %s].",
        "All paid media has default reallocation constraint of 50%% and 200%% of the average spend level."
      ),
      kpi_metric, InputCollect$intervalType, allocator_dates
    )

    alloc_plot1 <- tibble(
      type = c("Initial spend", "Initial response", "Optimized spend", "Optimized response"),
      type2 = c(rep("Initial", 2), rep("Optimized", 2)),
      type3 = rep(c(
        paste0("Period's total budget comparison\n "),
        paste0("Period's ", kpi_name, " uplift\n ")
      ), 2),
      value = unlist(select(
        dfb,
        "histSpendWindowTotal", "initResponseTotal",
        "optmSpendTotal", "optmResponseTotal"
      )[1, ])
    ) %>%
      mutate(label = formatNum(.data$value, abbr = TRUE)) %>%
      ggplot(aes(x = .data$type2, y = .data$value, fill = .data$type2)) +
      geom_col() +
      geom_text(
        aes(
          y = .data$value,
          label = formatNum(.data$value, signif = 3, abbr = TRUE)
        ),
        vjust = -0.2, size = 4, position = position_dodge(width = .9)
      ) +
      facet_wrap(.data$type3 ~ ., ncol = 1, scales = "free_y") +
      labs(x = NULL, y = NULL) +
      scale_fill_manual(values = alpha(c("grey", "#FFAA55"), .3)) +
      theme_lares(grid = "", size = 12, legend = "none") +
      coord_cartesian(clip = "off") +
      geom_hline(yintercept = 0, alpha = 0.5) +
      theme(
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      )

    dfbtemp <- dfb %>%
      select(
        "channels",
        "initSpendUnit", "initSpendShare",
        "initResponseUnit",
        "optmSpendUnit", "optmSpendShareUnit",
        "optmResponseUnit",
        "optmSpendUnitDelta"
      ) %>%
      mutate(optmSpendUnitDelta = (.data$optmSpendUnit - .data$initSpendUnit) / .data$initSpendUnit) %>%
      arrange(desc(.data$initSpendUnit)) %>%
      mutate(channels = factor(.data$channels, levels = .data$channels))
    colnames(dfbtemp) <- c("channel", "IS", "ISS", "IR", "OS", "OSS", "OR", "SSD")
    dfbtemp <- dfbtemp %>%
      tidyr::pivot_longer(cols = c("IS", "OS")) %>%
      left_join(select(dfb, channel = .data$channels, .data$initSpendUnit, .data$optmSpendUnit), by = "channel") %>%
      mutate(type = factor(ifelse(grepl("O", .data$name), "Optimized", "Initial"), levels = c("Initial", "Optimized"))) %>%
      filter(!grepl("R", .data$name)) %>%
      mutate(diff = ifelse(.data$name == "OS", formatNum(100 * (.data$SSD), signif = 2, pos = "%", sign = TRUE), "")) %>%
      mutate(channel = lares::autoline(gsub("_", " ", .data$channel), top = 20)) %>%
      mutate(channel = factor(.data$channel, levels = unique(.data$channel)))

    alloc_plot2 <- ggplot(dfbtemp, aes(x = .data$channel, y = .data$value, fill = .data$type)) +
      geom_col(position = position_dodge()) +
      geom_text(aes(label = diff), vjust = -0.1, position = position_dodge(width = .9)) +
      labs(x = NULL, y = NULL, fill = NULL) +
      scale_fill_manual(values = alpha(c("grey", "#FFAA55"), .3)) +
      theme_lares(grid = "Y", size = 12) +
      coord_cartesian(clip = "off") +
      geom_hline(yintercept = 0, alpha = 0.5) +
      theme(
        axis.text.x = element_text(angle = 75, hjust = 1),
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "top", legend.justification = "right",
        legend.key.size = unit(0.5, "line")
      )

    loser <- paste(unique(c(
      allocator$dt_optimOut$channels[which.min(allocator$dt_optimOut$initRoiUnit)],
      allocator$dt_optimOut$channels[which.min(allocator$dt_optimOut$initResponseMargUnit)]
    )), collapse = " and ")

    return(list(
      allocator = allocator,
      allocator_dates = allocator_dates,
      improvement = improvement,
      imp_pct = formatNum(100 * allocator$dt_optimOut$optmResponseUnitTotalLift[1], signif = 2, pos = "%"),
      top_marginal = allocator$dt_optimOut$channels[which.max(allocator$dt_optimOut$initResponseMargUnit)],
      loser = loser,
      alloc_txt1 = alloc_txt1,
      alloc_plot1 = alloc_plot1,
      alloc_txt2 = alloc_txt2,
      alloc_plot2 = alloc_plot2
    ))
  }

  alloc1 <- allocator_results(
    robyn_allocator(
      InputCollect = InputCollect,
      OutputCollect = OutputCollect,
      select_model = selected_model,
      channel_constr_low = 0.5,
      channel_constr_up = 2,
      date_range = "last_12",
      plots = TRUE,
      export = FALSE,
      quiet = TRUE
    )
  )

  alloc2 <- allocator_results(
    robyn_allocator(
      InputCollect = InputCollect,
      OutputCollect = OutputCollect,
      select_model = selected_model,
      total_budget = alloc1$allocator$dt_optimOut$initSpendTotal[1] * 1.2,
      channel_constr_low = 0.5,
      channel_constr_up = 2,
      date_range = "last_12",
      plots = TRUE,
      export = FALSE,
      quiet = TRUE
    )
  )

  # Executive Summary texts
  unknown <- ftext("[[X%]]", prop = fpt)
  approach <- block_list(
    fpar(paste(brand_market, "MMM used", paste0(InputCollect$intervalType, "ly"), "data from", modeling_window)),
    # fpar(
    #   "Included ",
    #   ifelse(cov_true, cov_sf, unknown),
    #   " of Selling Field costs and ",
    #   ifelse(cov_true, cov_ap, unknown),
    #   " of A&P costs (excluded non-promo, DAC, medical spend)",
    #   " to cover ",
    #   ifelse(cov_true,
    #     formatNum(100 * Extras$coverage$coverage_pct, 0, pos = "%"), unknown
    #   ),
    #   " of total SG&A spend"
    # ),
    fpar("Model and report were built using Rosa, supported by the Impact Measurement COE team")
  )

  insights <- c(
    paste0(gsub("\\..*", "", contrs_txt1), "."),
    if (nchar(no_impact) > 0) paste0("Consider shutting down promotional channels like ", no_impact, " which have no detected impact on ", kpi_name, ".") else NULL,
    paste("Incremental sales based on past budget allocation and performance could have been improved", alloc1$imp_pct, "with an optimal allocation."),
    paste("Consider increasing budget on paid channels, potentially switching the budget allocation to focus more on", alloc1$top_marginal, "which has the best", paste0("m", kpi_metric), "(most efficient) amongst paid channels."),
    paste("Deep dive into worst", kpi_metric, "and", paste0("m", kpi_metric), "promotional activities like", alloc1$loser, "to search for opportunities of improvement."),
    if (length(top_organic) > 0) paste0("The promotional organic channel with most impact on sales was ", top_organic, ". Assess the benefits of increasing this activity's frequency.") else NULL,
    "Etc, etc..."
  )

  learnings <- c(
    "Unlock potential of MMM by investing in a proper data foundation (data gathering and cleaning takes most of the time).",
    "Rosa was able to provide the necesarry tools for the team to run models and simulations to answer relevant business questions.",
    "To continue scaling MMM adoption, more trainings and sessions are required.",
    "Etc, etc..."
  )

  # Build the deck and export as a file
  if (TRUE) {
    # Cover
    report <- read_pptx(template) %>%
      add_slide(layout = "cover", master = master) %>%
      ph_with(paste("MMM Insights Report:", brand_market),
        location = ph_location_label(ph_label = "Title")
      ) %>%
      ph_with("Auto-Generated by Rosa", location = ph_location_label(ph_label = "Subtitle")) %>%
      ph_with(
        sprintf(
          "Presentation date: %s",
          cleanText(format(Sys.Date(), "%B %d, %Y"), keep = ",", title = TRUE)
        ),
        location = ph_location_label(ph_label = "Presentation date")
      ) %>%
      ph_with(
        sprintf(
          "Reporting dates: %s",
          cleanText(reporting_window, keep = ",", title = TRUE)
        ),
        location = ph_location_label(ph_label = "Reporting dates")
      ) %>%
      ph_with(
        sprintf(
          "Model date: %s",
          cleanText(
            format(as.Date(min(c(
              tempjson$ModelsCollect$train_timestamp,
              tempjson$ExportedModel$train_timestamp
            ))), "%B %d, %Y"),
            keep = ",", title = TRUE
          )
        ),
        location = ph_location_label(ph_label = "Model date")
      ) %>%
      ph_with(sprintf("Model ID: %s", tempjson$ExportedModel$select_model),
        location = ph_location_label(ph_label = "Model name")
      )

    # Slide: How to use this deck?
    report <- report %>%
      add_slide(layout = "content", master = master) %>%
      ph_with("How to read and use this report?",
        location = ph_location_label(ph_label = "Title")
      ) %>%
      ph_with("Add your own learnings and insights to this report",
        location = ph_location_label(ph_label = "Subtitle")
      ) %>%
      ph_with(c(
        "Marketing mix models (MMM) are an established measurement solution that works hand-in-hand with attribution modelling. It's especially suitable for providing holistic performance overview as well as strategic budget allocation guidance.",
        "The rule-of-thumb threshold for a good model fit is 80% or above. If below, it's recommended to reassess the input data and fine-tune the model in Rosa.",
        "The budget allocation results should be treated as directional recommendations.",
        'Recommended to add and fill with your own objectives, learnings, insights and next steps slides to this deck. Be specific with the "So Whats".'
      ), location = ph_location_label(ph_label = "Text"))

    # Slide: Executive Summary
    report <- report %>%
      add_slide(layout = "exec_summary", master = master) %>%
      ph_with(paste("Executive Summary: MMM Results for", brand_market),
        location = ph_location_label(ph_label = "Title")
      ) %>%
      ph_with("Report objective: AWARENESS (results and learnings)",
        location = ph_location_label(ph_label = "Subtitle")
      ) %>%
      ph_with("Up to 5 clear, focused objectives: (i) for decision, (ii) for input, (iii) for awareness.",
        location = ph_location_label(ph_label = "Text 1")
      ) %>%
      ph_with(approach, location = ph_location_label(ph_label = "Text 2")) %>%
      ph_with(
        block_list(
          fpar(ftext("[[Summarize your Directional Insights slide main points...]]", prop = fpt)),
          fpar(insights[1]),
          fpar(insights[2]),
          fpar(insights[3])
        ),
        location = ph_location_label(ph_label = "Text 3")
      ) %>%
      ph_with(
        block_list(
          fpar(ftext("[[Summarize your Learnings slide main points...]]", prop = fpt)),
          fpar(learnings[1]),
          fpar(learnings[2]),
          fpar(learnings[3])
        ),
        location = ph_location_label(ph_label = "Text 4")
      )

    # Slide: Design
    report <- report %>%
      add_slide(layout = "model_design", master = master) %>%
      ph_with("Modelling approach and data overview",
        location = ph_location_label(ph_label = "Title")
      ) %>%
      ph_with("Overview of the model configuration, variables & historical spend share",
        location = ph_location_label(ph_label = "Subtitle")
      ) %>%
      ph_with(
        paste(
          cleanText(kpi_name, title = TRUE),
          ifelse(!is.null(Extras$conversion_units),
            sprintf(" (%s)", Extras$conversion_units), ""
          )
        ),
        location = ph_location_label(ph_label = "KPI")
      ) %>%
      ph_with(paste(Extras$currency, num_abbr(total_spend, 3)),
        location = ph_location_label(ph_label = "Spend")
      ) %>%
      ph_with(
        sprintf(
          "%s %ss",
          InputCollect$rollingWindowLength,
          InputCollect$intervalType
        ),
        location = ph_location_label(ph_label = "Observations")
      ) %>%
      ph_with(modeling_window,
        location = ph_location_label(ph_label = "Date Range")
      ) %>%
      ph_with(length(InputCollect$paid_media_spends),
        location = ph_location_label(ph_label = "Paid Vars")
      ) %>%
      ph_with(v2t(InputCollect$paid_media_spends, quotes = FALSE),
        location = ph_location_label(ph_label = "Paid Variables")
      ) %>%
      ph_with(length(InputCollect$organic_vars),
        location = ph_location_label(ph_label = "Organic Vars")
      ) %>%
      ph_with(v2t(InputCollect$organic_vars, quotes = FALSE),
        location = ph_location_label(ph_label = "Organic Variables")
      ) %>%
      ph_with(
        length(InputCollect$context_vars) +
          length(InputCollect$prophet_vars),
        location = ph_location_label(ph_label = "Context Vars")
      ) %>%
      ph_with(v2t(c(InputCollect$context_vars, InputCollect$prophet_vars), quotes = FALSE),
        location = ph_location_label(ph_label = "Context Variables")
      )

    # Selected model flow
    if (length(Extras$model_selection) > 0) {
      nmodels <- num_abbr(tempjson$ModelsCollect$iterations * tempjson$ModelsCollect$trials, 2)
      nmodels_pareto <- Extras$model_selection$n_models
      sel_sub <- sprintf(
        "Out of %s generated models, selected #%s based on ranked criteria",
        nmodels, Extras$model_selection$model_rank
      )
      selected_df <- Extras$model_selection$selected_df[, -1] %>%
        arrange(desc(.data$weights)) %>%
        mutate(
          metrics_names = factor(.data$metrics_names, levels = .data$metrics_names),
          p = round(.data$weights / sum(.data$weights), 2)
        ) %>%
        mutate(plab = ifelse(.data$p < 0.05, "", formatNum(100 * .data$p, 1, pos = "%")))
      criteria_plot <- ggplot(selected_df, aes(x = "", y = .data$p, fill = .data$metrics_names)) +
        geom_bar(width = 1, stat = "identity") +
        coord_polar("y") +
        theme_lares(pal = 1, grid = "", legend = "right") +
        theme(axis.text.y = element_blank()) +
        geom_text(aes(label = .data$plab),
          position = position_stack(vjust = 0.5),
          size = 3.7
        ) +
        labs(fill = "Criteria", x = NULL, y = NULL) +
        theme(axis.text.x = element_blank()) +
        guides(fill = guide_legend(ncol = 1))

      colnames(selected_df) <- c("Criteria Used", "Weight", "Metric Value")
      sel_txt <- c(
        "Rosa generates thousands of models, and we must select the one that best reflects the business based on our knowledge and criteria.",
        sprintf("A total of %s models were trained, %s were identified as the best statistical models (lower errors), and were then ranked using these criteria to find the right model.", nmodels, nmodels_pareto),
        sprintf("The criteria with highest weights to rank the best models were %s.", paste(head(selected_df[, 1], 2), collapse = " and "))
      )
      report <- report %>%
        add_slide(layout = "model_select", master = master) %>%
        ph_with("Model Selection based on Metrics and Criteria",
          location = ph_location_label(ph_label = "Title")
        ) %>%
        ph_with(sel_sub, location = ph_location_label(ph_label = "Subtitle")) %>%
        ph_with(nmodels, location = ph_location_label(ph_label = "Total Models")) %>%
        ph_with(nmodels_pareto, location = ph_location_label(ph_label = "Best Models")) %>%
        ph_with(1, location = ph_location_label(ph_label = "Selected Model")) %>%
        ph_with(criteria_plot, location = ph_location_label(ph_label = "Plot")) %>%
        ph_with(sel_txt, location = ph_location_label(ph_label = "Text"))
    }

    # Model reports
    report <- report %>%
      add_slide(layout = "report_plots", master = master) %>%
      ph_with("Promotional contribution & model accuracy",
        location = ph_location_label(ph_label = "Title")
      ) %>%
      ph_with(paste("Understanding promotional and baseline factors contributing to", kpi_name),
        location = ph_location_label(ph_label = "Subtitle")
      ) %>%
      ph_with(cleanText(paste(InputCollect$dep_var, "contribution"), title = TRUE),
        location = ph_location_label(ph_label = "KPI 1 Title")
      ) %>%
      ph_with(formatNum(100 * rsq, 1, pos = "%"),
        location = ph_location_label(ph_label = "KPI 3")
      ) %>%
      ph_with(paste("Promotional", kpi_metric),
        location = ph_location_label(ph_label = "KPI name")
      ) %>%
      ph_with(
        formatNum(model_metric,
          signif = 3,
          pre = ifelse(kpi_metric == "ROAS", "x", paste0(Extras$currency, " ")),
          pos = ifelse(!is.null(Extras$conversion_units), paste(" per", Extras$conversion_units), "")
        ),
        location = ph_location_label(ph_label = "KPI 2")
      ) %>%
      ph_with(paste(contrs_txt1, contrs_txt2),
        location = ph_location_label(ph_label = "Text 1")
      ) %>%
      ph_with('Model accuracy is the Adjusted R^2 and translates to "percentage of variance explained". The closer the two lines in the plot, the higher the model fit. Aim for 80% or higher R^2.',
        location = ph_location_label(ph_label = "Text 2")
      ) %>%
      ph_with(contrs_plot1, location = ph_location_label(ph_label = "Plot 1")) %>%
      ph_with(reg_plot, location = ph_location_label(ph_label = "Plot 3"))

    # Model metrics
    report <- report %>%
      add_slide(layout = "generic", master = master) %>%
      ph_with("Paid channels insights and performance",
        location = ph_location_label(ph_label = "Title")
      ) %>%
      ph_with(sprintf("Paid channels spend share with average and marginal %s", kpi_metric),
        location = ph_location_label(ph_label = "Subtitle")
      ) %>%
      ph_with(chn_perf_plot, location = ph_location_label(ph_label = "Plot"))

    # Slide: Directional Insights
    report <- report %>%
      add_slide(layout = "content", master = master) %>%
      ph_with("Directional Insights of Reported MMM",
        location = ph_location_label(ph_label = "Title")
      ) %>%
      ph_with(dir_ins, location = ph_location_label(ph_label = "Subtitle")) %>%
      ph_with(insights, location = ph_location_label(ph_label = "Text"))

    if (ai_insights) {
      report <- report %>%
        add_slide(layout = "content", master = master) %>%
        ph_with("AI Generated Insights & Recommendations",
          location = ph_location_label(ph_label = "Title")
        ) %>%
        ph_with("Please, be aware these are auto-generated insights for selected model",
          location = ph_location_label(ph_label = "Subtitle")
        ) %>%
        ph_with(insights_ai, location = ph_location_label(ph_label = "Text"))
    }

    # Slide: Key Learnings
    report <- report %>%
      add_slide(layout = "content", master = master) %>%
      ph_with("Key Learnings from running this MMM",
        location = ph_location_label(ph_label = "Title")
      ) %>%
      ph_with("MMM provides new and confirmed data-driven insights",
        location = ph_location_label(ph_label = "Subtitle")
      ) %>%
      ph_with(learnings, location = ph_location_label(ph_label = "Text"))

    # Appendix slides with one-pagers sections
    report <- report %>%
      add_slide(layout = "separator_2", master = master) %>%
      ph_with("Appendix", location = ph_location_label(ph_label = "Title"))

    # Onepager Sections
    for (i in seq_along(op)) {
      report <- report %>%
        add_slide(layout = "generic", master = master) %>%
        ph_with(paste("One Pager: Section #", i), location = ph_location_label(ph_label = "Title")) %>%
        ph_with(op[[i]], location = ph_location_label(ph_label = "Plot"))
    }

    # Budget allocator (default)
    report <- report %>%
      add_slide(layout = "allocator", master = master) %>%
      ph_with(paste("Budget optimization:", ifelse(kpi_name == "revenue", "maximize ROAS", "minimize CPA")),
        location = ph_location_label(ph_label = "Title")
      ) %>%
      ph_with("Keeping same total budget, reallocate it across paid media to maximise return",
        location = ph_location_label(ph_label = "Subtitle")
      ) %>%
      ph_with(alloc1$alloc_plot1, location = ph_location_label(ph_label = "Plot 1")) %>%
      ph_with(alloc1$alloc_txt1, location = ph_location_label(ph_label = "Text 1")) %>%
      ph_with(alloc1$alloc_plot2, location = ph_location_label(ph_label = "Plot 2")) %>%
      ph_with(alloc1$alloc_txt2, location = ph_location_label(ph_label = "Text 2"))

    # Budget allocator (120%)
    report <- report %>%
      add_slide(layout = "allocator", master = master) %>%
      ph_with("Budget optimization: 20% budget increase",
        location = ph_location_label(ph_label = "Title")
      ) %>%
      ph_with("Increasing 20% total budget, allocate it across paid media to maximise return",
        location = ph_location_label(ph_label = "Subtitle")
      ) %>%
      ph_with(alloc2$alloc_plot1, location = ph_location_label(ph_label = "Plot 1")) %>%
      ph_with(alloc2$alloc_txt1, location = ph_location_label(ph_label = "Text 1")) %>%
      ph_with(alloc2$alloc_plot2, location = ph_location_label(ph_label = "Plot 2")) %>%
      ph_with(alloc2$alloc_txt2, location = ph_location_label(ph_label = "Text 2"))

    # Glossary slide
    report <- report %>%
      add_slide(layout = "glossary", master = master) %>%
      ph_with("Glossary", location = ph_location_label(ph_label = "Title"))

    # Add JSON file to replicate model / report
    report <- report %>%
      add_slide(layout = "json", master = master) %>%
      ph_with("JSON to replicate model and report", location = ph_location_label(ph_label = "Title")) %>%
      ph_with(as.character(jsonlite::toJSON(tempjson)), location = ph_location_label(ph_label = "Text"))
    # Hide last slide (requires officer >= 0.6.8.004)
    try(officer::slide_visible(report, hide = length(report)))

    # Export file
    print(report, target = filename)
    toc("mmm_pptx")
    return(report)
  }
}

################ PLAYGROUND

if (FALSE) {
  # library(Robyn)
  # library(dplyr)
  # library(ggplot2)
  # library(lares)
  # library(stringr)
  # library(patchwork)
  # library(officer)

  # Import model results
  json_file <- "RosaModel-2_233_15-20241127094009-DE-Nucala.json"
  template <- "RosaTemplate.pptx"
  filename <- "RosaTemplateDemo.pptx"
  master <- "Rosa"
  recreated <- robyn_recreate(json_file)
  json <- robyn_read(json_file)

  # BUILD THE DECK
  deck <- mmm_pptx(
    recreated$InputCollect,
    recreated$OutputCollect,
    json$Extras,
    extract_content_within_parentheses(input$models),
    template = template,
    master = master,
    filename = filename
  )

  # Check template available slides
  (pptx <- read_pptx(template))
  # Check template's slide available properties
  layout_properties(pptx, "generic")
}
