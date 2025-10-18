is_server <- function() grepl("\\.com", Sys.info()[["nodename"]])

is_rstudio <- function() Sys.getenv("RSTUDIO") == "1"

fix_double_slash <- function(x) gsub("\\/\\/", "\\/", x)

list_null_to_char <- function(...) {
  if (!is.null(names(list(...)))) {
    original_names <- names(list(...))
  } else {
    original_names <- LETTERS[seq_along(list(...))]
  }
  ret <- lapply(list(...), function(x) {
    unlist(lapply(x, function(y) if (is.null(y)) NA else y))
  })
  names(ret) <- original_names
  return(ret)
}

with_cache <- function(x, cache_code = "Rosa", use_cache = TRUE, overwrite = TRUE, quiet = TRUE, ...) {
  if (!use_cache) {
    if (!quiet) message(">>> Cache not enabled...")
    return(x)
  }
  if (cache_exists(cache_code, ...)) {
    if (!quiet) message(">>> Cache enabled and found. Loading...")
    x <- cache_read(cache_code, ...)
    return(x)
  } else {
    if (overwrite || !cache_exists(cache_code, ...)) {
      if (!quiet) message(">>> Cache storing...")
      if (!dir.exists(getOption("LARES_CACHE_DIR"))) {
        dir.create(getOption("LARES_CACHE_DIR"))
        if (!quiet) message("> Created cache dir...")
      }
      cache_write(x, cache_code, overwrite = overwrite, ...)
    }
    return(x)
  }
}

extract_content_within_parentheses <- function(text) {
  unlist(lapply(text, function(x) {
    if (grepl("\\(", x)) {
      # Find all matches of content within parentheses
      matches <- gregexpr("\\(([^()]*)\\)", x, perl = TRUE)
      matches_list <- regmatches(x, matches)
      # Extract the last match
      last_match <- tail(matches_list[[1]], 1)
      # Remove the parentheses
      result <- gsub("\\(|\\)", "", last_match)
      return(result)
    } else {
      return(x)
    }
  }))
}

toc_secs <- function(id) {
  lares::toc(id, type = "seconds", quiet = TRUE)$time
}

check_nas <- function(df, session) {
  if (sum(is.na(df)) > 0) {
    naVals <- lares::missingness(df)
    strs <- sprintf("%s (%s | %s%%)", naVals$variable, naVals$missing, naVals$missingness)
    msg <- paste0(
      "Dataset contains missing values. ",
      "These values must be removed or fixed for Rosa to properly work.\n  Missing values: ",
      paste(strs, collapse = ", ")
    )
    sendSweetAlert(
      session,
      title = "Missing values in dataset",
      text = msg, type = "warning"
    )
  }
}

update_signs <- function(session, input, variable) {
  d_ch <- length(input[[variable]][unlist(lapply(list(input[[variable]]), function(x) endsWith(x, "d")))])
  p_ch <- length(input[[variable]][unlist(lapply(list(input[[variable]]), function(x) endsWith(x, "p")))])
  n_ch <- length(input[[variable]][unlist(lapply(list(input[[variable]]), function(x) endsWith(x, "n")))])
  choices_d <- seq(1, d_ch + 1, by = 1)
  choices_p <- seq(1, p_ch + 1, by = 1)
  choices_n <- seq(1, n_ch + 1, by = 1)
  choices_d <- sapply(choices_d, paste, "d", sep = "")
  choices_p <- sapply(choices_p, paste, "p", sep = "")
  choices_n <- sapply(choices_n, paste, "n", sep = "")
  names(choices_d) <- rep("default", length(choices_d))
  names(choices_p) <- rep("positive", length(choices_p))
  names(choices_n) <- rep("negative", length(choices_n))
  choices_new <- append(append(choices_d, choices_p), choices_n)
  updateSelectizeInput(session, variable, choices = choices_new, selected = isolate(input[[variable]]))
}


budget_calc <- function(ds, chs, dt, dt_rg) {
  sum_bud <- 0
  if (dt %in% names(ds)) {
    ds[[dt]] <- as.Date(ds[[dt]], format = "%Y-%m-%d")
    ds <- ds[(ds[[dt]] >= dt_rg[1] & ds[[dt]] <= dt_rg[2]), ]
    for (ch in chs) {
      sum_bud <- sum_bud + sum(ds[[ch]])
    }
  }
  return(round(sum_bud, 0))
}

chatModal <- function(
    question = "gpt_question",
    answer = "gpt_answer",
    button = "gpt_ask",
    title = "Rosa AI Assistant") {
  modalDialog(
    list(
      tags$h2(title),
      fluidRow(
        column(10, textAreaInput(
          inputId = question,
          "Ask me about MMM or Rosa related topics:",
          width = "100%"
        )),
        column(2, br(), actionButton(
          button, "",
          icon = icon("play"), height = "350px", width = "100%",
          style = "display: inline-block; float:right"
        )),
        column(
          12,
          uiOutput(answer),
          helpText("Replies are based on our documentation and OpenAI GPT-4.1's MMM knowledge."),
          helpText("AI-generated content may be incorrect. We recommend to verify accuracy of content.")
        )
      )
    ),
    size = "l",
    easyClose = TRUE
  )
}

create_hover_action_button <- function(
    id, align, label, icon_name, icon_animation,
    button_animation = NULL, style = NULL) {
  hover_action_button(
    id,
    align = align, label, icon = icon(icon_name),
    icon_animation = icon_animation, button_animation = button_animation,
    style = style
  )
}

# next_date(c("2021-01-01", "2021-02-01"))
# next_date(c("2021-01-01", "2021-01-08", "2021-01-15"))
# next_date(c(Sys.Date() - 1, Sys.Date()))
next_date <- function(dates) {
  dates <- as.Date(dates)
  diffs <- diff(dates)
  if (all(diffs == 1)) {
    frequency <- "daily"
  } else if (all(diffs == 7)) {
    frequency <- "weekly"
  } else if (all(format(dates[-length(dates)], "%Y-%m") != format(dates[-1], "%Y-%m"))) {
    frequency <- "monthly"
  } else {
    warning("Unable to determine frequency to calculate next logical date. Returning last available date.")
    return(as.Date(tail(dates, 1)))
  }
  next_date <- switch(frequency,
    "daily" = dates[length(dates)] + 1,
    "weekly" = dates[length(dates)] + 7,
    "monthly" = seq(dates[length(dates)], by = "1 month", length.out = 2)[2]
  )
  return(as.Date(next_date))
}
