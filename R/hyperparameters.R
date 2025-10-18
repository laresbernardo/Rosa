setup_hyps_levers <- function(input, rv, channels, InputCollect = NULL) {
  aux <- c("alphas", "gammas", "thetas", "scales", "shapes")
  if (length(channels) > 0) {
    if (length(channels) != length(unique(channels))) {
      rep_channels <- names(which(table(channels) > 1))
      showNotification(
        paste(
          "Repeated channels for Paid and Organic variables:", v2t(rep_channels)
        ),
        type = "warning"
      )
      return(NULL)
    }
    # Don't run when loading demo data or loading configuration files (treated as demo)
    if (rv$demo) {
      message("Skipping hyperparameters overwrite")
      rv$demo <- FALSE # So next time it follows UI changes
      # Remove previous levers
      todelete <- names(input)[which(
        grepl(paste0(aux, collapse = "|"), names(input)) &
          !grepl(paste0(channels, collapse = "|"), names(input))
      )]
      lapply(todelete, function(x) {
        shiny::removeUI(selector = paste0("#", x), immediate = TRUE)
      })
    } else {
      if (!is.null(InputCollect)) {
        message("Setting up imported hyperparameters")
        adstock <- InputCollect$adstock
        rv$hyps_paid_org <- InputCollect$hyperparameters
        rv$hyp_list <- c(InputCollect$paid_media_spends, InputCollect$organic_vars)
        rv$demo <- TRUE # To treat as demo and don't overwrite with default hyperparameters
      } else {
        # message("Setting up default hyperparameters")
        adstock <- input$adstock
        rv$hyp_list <- c(input$paid_media_spends, input$organic_vars)
        media_type <- rep("default", length(channels))
        media_type[which(input$offline_vars %in% channels)] <- "offline"
        rv$hyps_paid_org <- robyn_hypsbuilder(
          channels,
          adstock = adstock,
          media_type = media_type,
          date_type = "skip" # rv$intervalType
        )
      }
      # Remove previous levers
      todelete <- names(input)[which(grepl(paste0(aux, collapse = "|"), names(input)))]
      lapply(todelete, function(x) {
        shiny::removeUI(selector = paste0("#", x), immediate = TRUE)
      })
      # Same order as UI
      channels <- channels[order(match(channels, rv$hyp_list[rv$hyp_list %in% channels]))]
      for (i in channels) {
        # Alphas
        shiny::insertUI(
          selector = "#hyper_ch_a",
          where = "beforeEnd",
          ui = div(
            id = paste0(i, "_alphas"),
            sliderInput(paste0(i, "_alphas"),
              label = paste0(i, "_alphas"),
              min = 0.001, max = 10, # print(Robyn::hyper_limits())
              value = c(
                rv$hyps_paid_org[[paste0(i, "_alphas")]][1],
                rv$hyps_paid_org[[paste0(i, "_alphas")]][2]
              ), step = 0.01
            )
          )
        )
        # Gammas
        shiny::insertUI(
          selector = "#hyper_ch_g",
          where = "beforeEnd",
          ui = div(
            id = paste0(i, "_gammas"),
            sliderInput(paste0(i, "_gammas"),
              label = paste0(i, "_gammas"),
              min = 0.001, max = 1, # print(Robyn::hyper_limits())
              value = c(
                rv$hyps_paid_org[[paste0(i, "_gammas")]][1],
                rv$hyps_paid_org[[paste0(i, "_gammas")]][2]
              ), step = 0.01
            )
          )
        )
        if (adstock == "geometric") {
          # Thetas
          shiny::insertUI(
            selector = "#hyper_ch_t",
            where = "beforeEnd",
            ui = div(
              id = paste0(i, "_thetas"),
              sliderInput(paste0(i, "_thetas"),
                label = paste0(i, "_thetas"),
                min = 0, max = 0.99, # print(Robyn::hyper_limits())
                value = c(
                  rv$hyps_paid_org[[paste0(i, "_thetas")]][1],
                  rv$hyps_paid_org[[paste0(i, "_thetas")]][2]
                ), step = 0.01
              )
            )
          )
        } else {
          # Shape (weibull)
          shiny::insertUI(
            selector = "#hyper_ch_sh",
            where = "beforeEnd",
            ui = div(
              id = paste0(i, "_shapes"),
              sliderInput(paste0(i, "_shapes"),
                label = paste0(i, "_shapes"),
                min = 0, max = 20, # print(Robyn::hyper_limits())
                value = c(
                  rv$hyps_paid_org[[paste0(i, "_shapes")]][1],
                  rv$hyps_paid_org[[paste0(i, "_shapes")]][2]
                ), step = 0.5
              )
            )
          )
          # Scale (weibull)
          shiny::insertUI(
            selector = "#hyper_ch_sc",
            where = "beforeEnd",
            ui = div(
              id = paste0(i, "_scales"),
              sliderInput(paste0(i, "_scales"),
                label = paste0(i, "_scales"),
                min = 0, max = 1, # print(Robyn::hyper_limits())
                value = c(
                  rv$hyps_paid_org[[paste0(i, "_scales")]][1],
                  rv$hyps_paid_org[[paste0(i, "_scales")]][2]
                ), step = 0.01
              )
            )
          )
        }
      }
    }
  } else {
    if (!rv$demo) {
      # Empty list when no channel (and it's not demo)
      todelete <- names(input)[which(grepl(paste0(aux, collapse = "|"), names(input)))]
      lapply(todelete, function(x) {
        shiny::removeUI(selector = paste0("#", x), immediate = TRUE)
      })
    }
  }
  return(rv)
}
