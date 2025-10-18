# Define server logic
server <- function(input, output, session) {
  options(LARES_CACHE_DIR = paste(getwd(), "cache", sep = "/"))
  virtualenv_create("r-reticulate")
  use_virtualenv("r-reticulate", required = FALSE)

  shinyjs::hide(selector = '.navbar-nav a[data-value="Configuration"]')
  shinyjs::hide(selector = '.navbar-nav a[data-value="Execution"]')
  shinyjs::hide(selector = '.navbar-nav a[data-value="Results"]')
  shinyjs::disable("run_next")
  shinyjs::runjs("document.getElementById('DS').focus();")

  rv <- reactiveValues(
    dataset = NULL, dataframe = NULL, exec = 0, intervalType = NULL, ts = NULL,
    demo = FALSE, reticulate_loaded = FALSE, last_log = NULL, projects = NULL,
    nav_matrix = matrix(0, 4, 3), calibration_input = NULL, reports = 0,
    chat_msg = 0, continue_download = FALSE,
    start_time = Sys.time(), querychat = NULL, rag_client = NULL
  )
  get_user <- reactive({
    user <- as.character(session$user)
    user <- ifelse(length(user) == 0, Sys.info()[["user"]], user)
    return(user)
  })
  observe({
    rv$user <- get_user()
    if (!grepl("0033|0036", Sys.info()[["nodename"]])) {
      rv$last_log <- log_action("start", extra = list(
        url_pathname = session$clientData$url_pathname
      ), user = rv$user)
    }
  })

  button_style <- "margin-top:24px; border-color:#006400; text-padding: -2px; text-align:center; border-radius: 5px; border-width: 2px; width: 100%"
  countries <- unique(Robyn::dt_prophet_holidays$country)
  updateSelectInput(session, "holidays_country", choices = c("", countries), selected = NULL)

  # Backlog button
  backlog_file <- "BACKLOG"
  if (file.exists(backlog_file)) {
    output$show_backlog <- renderUI(
      actionButton("show_backlog", "Show Backlog", size = "sm", icon = icon("tools"))
    )
  }
  observeEvent(input$show_backlog, {
    bl <- readLines(backlog_file)
    showModal(modalDialog(
      list(
        tags$h2("Current dev backlog for Rosa"),
        lapply(bl[-1], function(x) {
          if (grepl("^(test:|fix:|feat:|evaluate:|implemented:|changelog:|abandoned:)", tolower(x))) {
            shiny::strong(shiny::h3(x))
          } else {
            shiny::p(x)
          }
        })
      ),
      size = "l", easyClose = TRUE
    ))
  })

  # AI Assistants Buttons
  if (enable_gpt) {
    if (Sys.getenv("OPENAI_API") != "") {
      message("OpenAI tools enabled...")
      output$chatme <- renderUI(
        actionButton(
          "chatme", "",
          icon = icon("robot"),
          style = "border-radius: 50%; width: 30px; height: 30px; padding: 4px;"
        )
      )
      if (Sys.getenv("OPENAI_ASSISTANT") != "") {
        gpt_assistant(input, output, session, rv)
      } else {
        # QA Assistant with docs as context
        # output$askme <- renderUI(actionButton("askme", "AI Assistant", size = "sm", icon = icon("robot")))
        # gpt_contexted(input, output, session, rv)

        # Chat Assistant with RAG for docs
        if (!file.exists(store_location)) {
          message("Enabled OpenAI Chat Assistant WITHOUT documentation")
          rv$rag_client <- ellmer::chat_openai(
            model = "gpt-4.1",
            api_key = Sys.getenv("OPENAI_API"),
            system_prompt = system_prompt
          )
        } else {
          message("Enabled OpenAI RAG Chat Assistant WITH documentation")
          rv$rag_client <- ragnardb_client()
        }
        gpt_rag(input, output, session, rv)
      }
    }
  }

  ### Dataset UI #########################################################################################
  output$exec_type_txt <- renderUI({
    if (input$exec_type == 1) {
      txt <- p(strong("Train MMM from scratch"), p("Steps: provide data, define the modeling parameters, run simulations, select a final model, and export."))
    }
    if (input$exec_type == 3) {
      txt <- p(strong("Recreate a previously trained MMM to access the one-pager and budget allocator"), p("Steps: provide model's JSON file (and data if not available in JSON file), and recreate model."))
    }
    if (input$exec_type == 2) {
      txt <- p(strong("Retrain a previously selected MMM with additional data available"), p("Steps: provide model's JSON file and data (original + new), define refresh parameters, run simulations, and export."))
    }
    if (input$exec_type == 4) {
      txt <- p(strong("Compare multiple MMM models"), p("Steps: upload multiple models JSON files with data OR select an available pre-processed project."))
    }
    return(txt)
  })

  observeEvent(input$exec_type, {
    # Check what upper tabs should be shown or hidden
    nav_fun(input$exec_type, rv$nav_matrix)
    if (input$exec_type %in% c(2, 3)) {
      shinyjs::runjs("document.getElementById('JS').focus();")
      shinyjs::runjs("document.getElementById('JSmanual').focus();")
    } else {
      shinyjs::runjs("document.getElementById('DS').focus();")
    }
    if (input$exec_type == 1) {
      rv$demo <- FALSE
    }
  })

  output$dat_next_button <- renderUI(dat_next_button(input$exec_type))

  output$dat_json_upload <- renderUI({
    if (input$exec_type %in% c(2, 3)) {
      fluidRow(
        column(12, shiny::wellPanel(
          useShinyjs(),
          h3(strong("Configurations to recreate model")),
          tabsetPanel(
            id = "model_jsondata",
            tabPanel(
              title = "JSON file",
              helpText("Upload a JSON file to replicate your previously trained and exported model. If it contains the data it will automatically load for you and there's no need to upload the data below."),
              tags$div(
                fileInput("JS", NULL, multiple = FALSE, accept = c(".json")),
                id = "JS_tag"
              )
            ),
            tabPanel(
              title = "JSON text",
              helpText("Paste JSON formatted text below to replicate a previously trained and exported model. If it contains the data it will automatically load for you and there's no need to upload the data below."),
              textInput("json_datamodel", label = NULL, width = "100%"),
              actionButton("JSmanual", label = "Import", icon = icon("gear"), width = "100%")
            )
          )
        ))
      )
    }
  })

  output$dat_data_upload <- renderUI({
    if (input$exec_type %in% c(1, 2, 3)) {
      sidebarLayout(
        sidebarPanel(
          useShinyjs(),
          h3(strong("Dataset")),
          tabsetPanel(
            id = "dataset_type",
            tabPanel(
              title = "CSV",
              helpText("CSV setups before uploading"),
              splitLayout(
                radioButtons("csv_sep", "Separator", choices = c("," = ",", ";" = ";"), selected = ",", inline = TRUE),
                radioButtons("csv_dec", "Decimals", choices = c("." = ".", "," = ","), selected = ".", inline = TRUE)
              ),
              # textInput("csv_date_format", label = "Date Format", value = "%Y-%m-%d", width = "100%"),
              fileInput("DS", "Upload Dataset (.csv)", multiple = FALSE, accept = c(".csv"))
            ),
            tabPanel(
              title = "JSON",
              helpText("Paste JSON formatted data below"),
              textInput("json_data", label = NULL, width = "100%"),
              actionButton("json_data_process", label = "Import", icon = icon("gear"), width = "100%")
            )
          ),
          hr(),
          h5(strong("If you don't have your own data")),
          actionButton("test_data",
            label = "... load dummy dataset",
            icon = icon("flask"), width = "100%"
          ),
          hr(),
          h3(strong("Calibration")),
          helpText("[Optional] Provide incrementality experiments results to calibrate:"),
          helpText(HTML(paste(
            "Read more about calibration",
            tags$a("here.", target = "_blank", href = "https://bit.ly/robyn-calibration")
          ))),
          fileInput("calibration_data", "Upload Dataset (.csv)", multiple = FALSE, accept = c(".csv"))
        ),
        mainPanel(
          style = "position: auto; overflow-x: auto; overflow-y: auto",
          do.call(splitLayout, c(
            list(cellArgs = list(style = "padding: 5px")),
            Filter(Negate(is.null), list(
              if (enable_dataset_ai) uiOutput("show_dataset_ai") else NULL,
              uiOutput("show_dataset_button"),
              uiOutput("show_highlight_outliers"),
              uiOutput("show_corrs_check"),
              uiOutput("show_tableau")
            ))
          )),
          tableOutput("dataset")
        )
      )
    }
  })

  output$dat_dashboard <- renderUI({
    if (input$exec_type %in% 4) {
      fluidRow(
        column(12, shiny::wellPanel(
          useShinyjs(),
          h4(strong("Upload multiple models:")),
          p("Upload multiple models (with data) at the same time. Each model is a JSON file exported from Rosa. Rename your files to have useful reference names."),
          tags$div(
            fileInput("dash", "Upload Multiple Models (.json files)", multiple = TRUE, accept = ".json"),
            id = "dash_tag"
          ),
          uiOutput("dash_name"),
          tableOutput("dash_files"),
          uiOutput("dash_process"),
          if (isTRUE(length(rv$projects) > 0)) {
            list(
              hr(),
              h4(strong("OR select an available pre-built project:")),
              fluidRow(
                column(9, selectizeInput("projects", label = NULL, choices = NULL, selected = NULL, width = "100%")),
                column(3, actionButton("dash_go", "Load project", icon = icon("check"), width = "100%"))
              )
            )
          }
        ))
      )
    }
  })

  observeEvent(input$DS, {
    read <- TRUE
    tryCatch(
      {
        raw <- readLines(input$DS$datapath, n = 2L)
        if (grepl(";", raw[1]) && input$csv_sep == ",") {
          sendSweetAlert(
            title = "Upload issue",
            text = "You may want to try to set 'Semicolon' as separator",
            type = "error"
          )
          read <- FALSE
        }
        if (grepl(",", raw[2]) && input$csv_dec == "." && input$csv_sep != ",") {
          sendSweetAlert(
            title = "Upload issue",
            text = "You may want to try to set 'Comma' for decimals",
            type = "error"
          )
          read <- FALSE
        }
        if (read) {
          updateSelectInput(session, "adstock", selected = "weibull_pdf")
          df <- read.csv(input$DS$datapath, sep = input$csv_sep, dec = input$csv_dec)
          date_format <- function(x) grepl("^\\d{4}-\\d{2}-\\d{2}$", x)
          if (!any(date_format(df[1, ]))) {
            showNotification("No date column(s) detected. Dates must be in format YYYY-MM-DD", type = "error")
          } else {
            rv <- process_data(df, rv, input, session)
            rv$demo <- FALSE
            showNotification("Data imported from CSV", type = "message")
          }
        }
      },
      error = function(e) showNotification(e)
    )
  })

  # Calibration data
  observeEvent(input$calibration_data, {
    rv$calibration_input <- read.csv(input$calibration_data$datapath, sep = input$csv_sep, dec = input$csv_dec)
    these <- c(
      "channel", "liftStartDate", "liftEndDate",
      "liftAbs", "spend", "confidence", "metric", "calibration_scope"
    )
    if (!all(these %in% names(rv$calibration_input))) {
      rv$calibration_input <- NULL
      url <- a("Calibration Guide from Robyn's demo",
        href = "https://github.com/facebookexperimental/Robyn/blob/8715f0dcb777d402f103d510ea0d8c84323f2c56/demo/demo.R#L210"
      )
      sendSweetAlert(
        title = "Upload issue",
        text = list(
          p(paste("Calibration data must contain columns:", v2t(these))),
          p("Read more about it here:"),
          tagList(url)
        ),
        type = "error", html = TRUE
      )
    }
  })

  # Use demo data
  observeEvent(input$test_data, {
    demo_data <- system.file("data/demo_data.RData", package = "Rosa")
    if (file.exists(demo_data)) {
      load(demo_data)
      rv$last_log <- log_action("demo_data", "Rosa", user = rv$user)
    } else {
      demo_data <- Robyn::dt_simulated_weekly
      rv$last_log <- log_action("demo_data", "Robyn", user = rv$user)
    }
    rv <- process_data(demo_data, rv, input, session)
    rv$dataset <- mutate_if(rv$dataset, is.numeric, round)
    msg <- "Dummy data imported"
    showNotification(msg, type = "message")
    message(msg)
    rv$demo <- TRUE
  })

  observeEvent(input$JS, {
    tryCatch(
      {
        js_file <- robyn_read(input$JS$datapath)
        print(js_file)
        rv$JS_datapath <- gsub("\\\\", "/", input$JS$datapath)
        rv$js_select_model <- js_file$ExportedModel$select_model
        showNotification("JSON file loaded!", "", type = "message")
        # Data (when available)
        rv <- import_json_data(js_file, rv, input, session)
      },
      error = function(e) {
        showNotification("Not a valid JSON file", "", type = "error")
        return()
      }
    )
  })

  observeEvent(input$json_data_process, {
    tryCatch(
      {
        # Translate JSON to a list
        js_file <- jsonlite::fromJSON(input$json_data)
        if ("json" %in% names(js_file)) js_file <- js_file$json
        # Data (when available)
        rv <- import_json_data(js_file, rv, input, session)
      },
      error = function(e) showNotification(e)
    )
  })

  observeEvent(input$JSmanual, {
    tryCatch(
      {
        # Translate JSON to a list
        js_file <- jsonlite::fromJSON(input$json_datamodel)
        if ("json" %in% names(js_file)) js_file <- js_file$json
        rv <- import_json_data(js_file, rv, input, session)

        # Data & Configurations
        if (!"ExportedModel" %in% names(js_file) || length(js_file$ExportedModel$select_model) != 1) {
          showNotification(
            "JSON text contains none or +1 models. Are you trying to replicate a single model?",
            type = "error"
          )
        } else {
          tempjson <- paste0(tempdir(), "/", js_file$ExportedModel$select_model, ".json")
          jsonlite::write_json(js_file, tempjson)
          rv$JS_datapath <- tempjson
          rv$js_select_model <- js_file$ExportedModel$select_model
          rv$rec$notes[[js_file$ExportedModel$select_model]] <- js_file$notes
          showNotification("JSON file loaded!", "", type = "message")
        }
      },
      error = function(e) {
        showNotification(e, type = "error")
        return()
      }
    )
  })

  observeEvent(input$dat_next, {
    # Allow to continue if dataset is uploaded for building a model
    if ((input$exec_type == 1 && !is.null(rv$dataset)) ||
      # Allow to continue if JSON file / model ID and data is available to refresh
      (input$exec_type == 2 && !is.null(rv$dataset) && !is.null(rv$js_select_model))) {
      # shinyjs::enable(selector = '.navbar-nav a[data-value="Configuration"]')
      rv$nav_matrix[as.numeric(input$exec_type), 1] <- 1
      shinyjs::show(selector = '.navbar-nav a[data-value="Configuration"]')
      updateTabsetPanel(session, "tabs", selected = "Configuration")
      shinyjs::runjs("window.scrollTo(0, 0)")
      shinyjs::hide("res_back4")
      shinyjs::show("res_back")

      demo_json <- system.file("data/demo_json.RData", package = "Rosa")
      if (rv$demo && file.exists(demo_json)) {
        try({
          load(demo_json)
          config_json(demo_json, rv$dataset, session)
          rv <- setup_hyps_levers(input, rv, demo_json$InputCollect$all_media, demo_json$InputCollect)
          showNotification("Demo settings loaded", "", type = "message")
        })
      }
    } else {
      showNotification("Upload required files", "", type = "error")
    }
  })

  # Show the uploaded data
  observe({
    req(rv$dataset)
    rv$gttable <- gt(rv$dataset) %>%
      opt_interactive(
        use_compact_mode = TRUE,
        use_page_size_select = TRUE,
        use_highlight = TRUE,
        page_size_default = 20
      )
  })
  observeEvent(input$highlight_outliers, {
    req(rv$gttable)
    rv$last_log <- log_action("outliers", user = rv$user)
    num <- colnames(rv$dataset)[unlist(
      lapply(rv$dataset, function(x) any(class(x) %in% c("integer", "numeric")))
    )]
    outliers <- lapply(num, function(i) {
      y <- unlist(rv$dataset[, i])
      return(lares::outlier_zscore(y))
    })
    names(outliers) <- num
    obs <- lapply(num, function(x) {
      dat <- unlist(rv$dataset[, x])
      out <- outliers[[x]]
      data.frame(variable = rep(x, sum(out)), value = dat[out], observation = which(out))
    })
    check_these <- as_tibble(bind_rows(obs)) %>%
      rowwise() %>%
      mutate(col = which(colnames(rv$dataset) == .data$variable))
    if (!is.null(rv$dataset) && input$date %in% colnames(rv$dataset)) {
      check_these$date <- rv$dataset[[input$date]][check_these$observation]
    }
    sendSweetAlert(
      title = "Numeric Outliers",
      text = list(
        sprintf(
          "Found %s outliers throughout your numerical columns,
          using 3 standard deviations (sd) as outlier detection criteria.
          Please, check if they are not data error; if they are not,
          consider adding additional column(s) to help explain their behaviour.",
          nrow(check_these)
        ),
        gt(select(check_these, -.data$col)) %>%
          opt_interactive(use_compact_mode = TRUE, use_highlight = TRUE)
      ),
      html = TRUE,
      width = "85%",
      showCloseButton = TRUE
    )
    rv$gttable <- rv$gttable %>%
      tab_style(
        style = list(
          cell_fill(color = "red"),
          cell_text(color = "white", weight = "bold")
        ),
        locations = lapply(seq_along(check_these$observation), function(i) {
          cells_body(
            columns = check_these$col[i],
            rows = check_these$observation[i]
          )
        })
      ) %>%
      opt_interactive(page_size_default = nrow(rv$dataset))
  })
  observe({
    req(rv$gttable)
    output$dataset <- gt::render_gt(rv$gttable)
  })

  # Highlight outliers option
  output$show_highlight_outliers <- renderUI({
    req(rv$dataset)
    return(list(actionButton("highlight_outliers", "Outliers", icon = icon("arrows-up-down"), width = "100%")))
  })
  output$show_corrs_check <- renderUI({
    req(rv$dataset)
    return(list(actionButton("corrs_check", "Correlations", icon = icon("bar-chart"), width = "100%")))
  })

  # Show dataset summary option
  output$show_dataset_button <- renderUI({
    req(rv$dataset)
    return(list(actionButton("get_dataset_summary", label = "Stats", icon = icon("check"), width = "100%")))
  })
  observeEvent(input$get_dataset_summary, {
    rv$last_log <- log_action("dataset_summary", user = rv$user)
    sendSweetAlert(
      session = session,
      title = "Dataset Summary Stats",
      text = list(
        gtExtras::gt_plt_summary(rv$dataset, title = "")
      ),
      html = TRUE,
      width = "75%",
      showCloseButton = TRUE
    )
  })

  # Tableau like tool option
  output$show_tableau <- renderUI({
    req(rv$dataset)
    return(list(actionButton("get_tableau", label = "Analyzer", icon = icon("star"), width = "100%")))
  })
  observeEvent(input$get_tableau, {
    rv$last_log <- log_action("analyzer", user = rv$user)
    sendSweetAlert(
      session = session,
      text = list(
        GWalkR::gwalkr(rv$dataset)
      ),
      html = TRUE,
      width = "98%",
      showCloseButton = TRUE,
      title = NULL,
      btn_labels = "Close"
    )
  })

  # New cross-correlation plot
  output$corr_plot <- renderPlot(
    lares::corr_cross(rv$dataset[, input$corr_vars], top = 20, quiet = TRUE),
    res = 100
  )
  observeEvent(input$corrs_check, {
    rv$last_log <- log_action("correlations", user = rv$user)
    sendSweetAlert(
      session = session,
      title = "Variables Cross-Correlations",
      text = list(
        pickerInput(
          "corr_vars", "Select variables to check",
          colnames(rv$dataset), colnames(rv$dataset),
          multiple = TRUE, width = "100%",
          options = list(`actions-box` = TRUE)
        ),
        plotOutput("corr_plot", width = "100%", height = "500px")
      ),
      html = TRUE,
      btn_labels = "OK",
      width = "70%",
      height = "95%",
      showCloseButton = TRUE
    )
  })

  # New Dataset AI Assistant
  if (enable_dataset_ai) {
    if (Sys.getenv("OPENAI_API") != "") {
      observe({
        req(rv$dataset)
        # Only initialize if rv$querychat is not already set
        if (is.null(rv$querychat)) {
          # try_require("querychat") # devtools::install_github("posit-dev/querychat/pkg-r")
          api_key <- Sys.getenv("OPENAI_API")
          data_description <- "
      This is not a dashboard but a raw table with a dataset for training Marketing Mix Models (MMM).
      Please, don't print any error messages to the user's end messages.
      Allow for the user to suppress or get rid of columns. Also allow for the user to create or add columns or variables to the table.
      Always end variable names with the type of promotional data it contains when creating a variable:
      '_S' if it is a spend channel
      '_O' if it is an organic channel
      '_E' if it is an exposure or impression metric (associated to a spend channel)
      '_C' if it is a contextual variable
    "
          df <- isolate(rv$dataset)
          querychat_config <- querychat_init(
            querychat_data_source(df, tbl_name = "df"),
            client = ellmer::chat_openai(
              model = "gpt-4.1",
              api_key = Sys.getenv("OPENAI_API")),
            data_description = data_description,
            greeting = "Ask anything...",
            extra_instructions = "
        You are always allowed to modify the original data as the user asks for,
        but ask for confirmation before you do"
          )
          rv$querychat <- querychat_server("chat_table", querychat_config)
        }
      })
      # Render UI for the button
      output$show_dataset_ai <- renderUI({
        req(rv$dataset)
        list(actionButton("open_chat", label = "Dataset AI", icon = icon("lightbulb"), width = "100%"))
      })
      # Show the modal when the button is clicked
      observeEvent(input$open_chat, {
        req(rv$querychat)
        rv$last_log <- log_action("dataset_ai", "loaded", user = rv$user)
        showModal(modalDialog(
          title = "Interact with your data using our Dataset AI",
          querychat_ui("chat_table"),
          easyClose = TRUE,
          size = "m",
          footer = modalButton("Close")
        ))
      })
      # Log user input from the chat
      observeEvent(input$`chat_table-chat_user_input`, {
        req(rv$querychat)
        rv$last_log <- log_action("dataset_ai_prompt", input$`chat_table-chat_user_input`, user = rv$user)
      })
      # Update dataset based on querychat interactions
      observe({
        req(rv$querychat, rv$querychat$df())
        new_df <- rv$querychat$df()
        if (!identical(new_df, rv$dataset)) {
          if (length(rv$querychat$title()) > 0) {
            showNotification(
              paste("Dataset updated:", rv$querychat$title()),
              duration = 5, type = "message"
            )
            message(paste("Dataset updated:", rv$querychat$title())) 
          }
          rv$dataset <- new_df
          rv <- process_data(rv$dataset, rv, input, session, query_chat = TRUE)
          rv$gttable <- gt(rv$dataset) %>%
            opt_interactive(
              use_compact_mode = TRUE,
              use_page_size_select = TRUE,
              use_highlight = TRUE,
              page_size_default = 20
            )
        }
      })
    }
  }

  ### Configuration #########################################################################################

  observeEvent(input$glossary, {
    glossary <- system.file("data/glossary.RData", package = "Rosa")
    if (file.exists(glossary)) {
      load(glossary)
      sendSweetAlert(
        session = session,
        title = "Inputs and Parameters Glossary",
        text = tabsetPanel(
          tabPanel(
            "Dataset Inputs and Variables",
            gt(glossary$inputs) %>%
              opt_interactive(
                use_highlight = TRUE, use_pagination = FALSE,
                use_pagination_info = FALSE, use_sorting = FALSE
              )
          ),
          tabPanel(
            "Modeling Parameters",
            gt(select(glossary$params, -.data$Default)) %>%
              opt_interactive(
                use_highlight = TRUE, use_pagination = FALSE,
                use_pagination_info = FALSE, use_sorting = FALSE
              )
          )
        ),
        btn_labels = NA,
        width = "96%", height = "85%",
        showCloseButton = TRUE
      )
    }
  })

  # LOAD SAVED CONFIGURATIONS (JSON)
  observeEvent(input$CF2, {
    tryCatch(
      {
        readthis <- ifelse(!is.null(rv$JS_datapath), rv$JS_datapath, input$CF2$datapath)
        cf <- robyn_read(readthis)
        config_json(cf, rv$dataset, session)
        # Hyperparameters
        rv <- setup_hyps_levers(input, rv, cf$InputCollect$all_media, cf$InputCollect)
        showNotification("JSON: Configuration and hyperparameters loaded", "", type = "message")
      },
      error = function(e) {
        msg <- paste("Error:", e)
        message(msg)
        showNotification(msg, type = "error")
        return()
      }
    )
  })

  # LOAD SAVED CONFIGURATIONS (JSON) FROM PASTE BOX
  observeEvent(input$json_conf_process, {
    req(input$json_conf)
    tryCatch(
      {
        cf <- jsonlite::fromJSON(input$json_conf)
        if ("json" %in% names(cf)) cf <- cf$json
        config_json(cf, rv$dataset, session)
        # Hyperparameters
        rv <- setup_hyps_levers(input, rv, cf$InputCollect$all_media, cf$InputCollect)
        showNotification("PASTE: Configuration and hyperparameters loaded", "", type = "message")
      },
      error = function(e) {
        msg <- paste("Error:", e)
        message(msg)
        showNotification(msg, type = "error")
        return()
      }
    )
  })

  observeEvent(input$reset, {
    updateTextInput(session, "robyn_path", value = default_path)
  })
  observeEvent(input$tempdir, {
    updateTextInput(session, "robyn_path", value = gsub("\\\\", "\\/", tempdir()))
  })

  output$outputs_dir <- renderUI({
    if (!is_server()) {
      list(
        textInput("robyn_path", "Rosa outputs folder: ", default_path),
        splitLayout(
          actionButton("reset", "Default", width = "100%"),
          actionButton("tempdir", "Hidden", width = "100%")
        )
      )
    }
  })

  observe({
    if (length(input$paid_media_vars) < 1) {
      rv$impressions <- input$paid_media_spends
    } else {
      rv$impressions <- input$paid_media_vars
    }
  })

  output$date_sales_plot <- renderPlot(
    {
      req(rv$dataset)
      req(input$date)
      if (input$date %in% colnames(rv$dataset)) {
        rv$dataset[[input$date]] <- as.Date(rv$dataset[[input$date]], origin = "1970-01-01")
        ggplot(rv$dataset, aes_string(x = input$date, y = input$sales)) +
          labs(
            x = sprintf("%s (%s)", input$date, rv$intervalType),
            y = sprintf("%s (%s)", input$sales, input$dep_var_type)
          ) +
          geom_line() +
          theme_minimal() +
          theme(
            panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()
          ) +
          scale_y_abbr()
      }
    },
    bg = "transparent"
  )

  output$conversion_units <- renderUI({
    req(rv$dataset)
    req(input$dep_var_type)
    if (input$dep_var_type == "conversion") {
      list(
        helpText("What does a unit of your conversions dependent variable refer to? (i.e., doses administered, packs of 6 vaccines, etc.)"),
        textInput("conversion_units", "Unit of conversions:", value = "", width = "100%")
      )
    }
  })

  output$currency <- renderUI({
    req(rv$dataset)
    req(input$dep_var_type)
    selectInput("currency", "Select Spends Currency",
      choices = sort(c("USD", "EUR", "GBP")),
      selected = "EUR", width = "100%"
    )
  })

  output$paid_media_spends_summary <- renderTable(
    {
      req(rv$dataset)
      req(input$date)
      req(input$paid_media_spends)
      df <- rv$dataset %>%
        rename("date" = input$date) %>%
        filter(
          .data$date >= input$window[1],
          .data$date <= input$window[2]
        )
      paid_media_spends_summary <- df %>%
        select(input$paid_media_spends) %>%
        tidyr::gather() %>%
        group_by(.data$key) %>%
        summarise(total = sum(.data$value), .groups = "drop") %>%
        arrange(desc(.data$total))

      # Calculate cost per actions when provided as exposure metrics
      if (length(input$paid_media_vars) == length(input$paid_media_spends)) {
        cpas <- df %>%
          select(input$paid_media_vars) %>%
          tidyr::gather(key = "paid_media_vars") %>%
          group_by(.data$paid_media_vars) %>%
          summarise(actions = sum(.data$value), .groups = "drop") %>%
          arrange(factor(.data$paid_media_vars, levels = input$paid_media_vars))
        paid_media_spends_summary$CPE <- paid_media_spends_summary$total / cpas$actions
      }
      paid_media_spends_summary %>%
        mutate(
          p = formatNum(100 * .data$total / sum(.data$total), signif = 3, pos = "%"),
          total = formatNum(.data$total, 3, abbr = TRUE)
        ) %>%
        select("key", "total", "p", everything()) %>%
        rename("Paid channel" = "key", "Total*" = "total", "%" = "p")
    },
    width = "100%"
  )

  # observeEvent(input$paid_media_spends, {
  #   updateSelectizeInput(session, "paid_media_spends", selected = isolate(input$paid_media_spends))
  # })
  #
  # observeEvent(input$paid_media_vars, {
  #   updateSelectizeInput(session, "paid_media_vars", selected = isolate(input$paid_media_vars))
  # })
  #
  # observeEvent(input$organic_vars, {
  #   updateSelectizeInput(session, "organic_vars", selected = isolate(input$organic_vars))
  # })

  observeEvent(input$context_signs, {
    update_signs(session, input, variable = "context_signs")
  })
  observeEvent(input$organic_signs, {
    update_signs(session, input, variable = "organic_signs")
  })

  # Hyperparameters
  channels <- reactive(c(input$paid_media_spends, input$organic_vars))
  observeEvent(c(input$paid_media_spends, input$organic_vars, input$adstock, input$offline_vars), {
    req(input$dat_next)
    req(input$sales)
    isolate({
      # Online/Offline to set alpha ranges (offline sets it to 1 => C curves)
      updateSelectizeInput(session, "offline_vars", choices = channels(), selected = input$offline_vars)
      # Update choices for Menu 1
      updateSelectizeInput(session, "paid_media_spends", choices = rv$nums_s, selected = input$paid_media_spends)
      updateSelectizeInput(session, "paid_media_vars", choices = setdiff(rv$nums_ns, input$sales), selected = input$paid_media_vars)
      # Update choices for Menu 2, excluding the selected value in Menu 1
      new_choices <- setdiff(rv$nums_ns, c(input$sales, input$paid_media_spends))
      updateSelectizeInput(session, "organic_vars", choices = new_choices, selected = input$organic_vars)
      # Update choices for Menu 3, excluding the selected values in Menu 1 and Menu 2
      new_choices <- setdiff(rv$contx, c(input$paid_media_spends, input$organic_vars, input$sales))
      updateSelectizeInput(session, "context_vars", choices = new_choices, selected = input$context_vars)
      updateSelectizeInput(session, "factor_vars", choices = setdiff(rv$contx, input$sales), selected = input$factor_vars)
      rv <- setup_hyps_levers(input, rv, channels())
    })
  })

  # Saturation and Adstock Curves Helper
  output$plot_adstock <- renderPlot(isolate(Robyn::plot_adstock()))
  output$plot_saturation <- renderPlot(isolate(Robyn::plot_saturation()))
  observeEvent(input$curves_help, {
    sendSweetAlert(
      session = session,
      title = "Saturation & Adstock Curves",
      text = splitLayout(
        cellWidths = c("38%", "58%"),
        plotOutput("plot_saturation", width = "93%"),
        plotOutput("plot_adstock", width = "93%")
      ),
      html = TRUE,
      btn_labels = NA,
      width = "96%",
      showCloseButton = TRUE
    )
  })

  observeEvent(input$con_next, {
    if (!is.null(rv$dataframe)) {
      dirname <- ifelse(is.null(input$robyn_path), default_path, input$robyn_path)
      if (!dir.exists(dirname)) dir.create(dirname, recursive = TRUE)
      # shinyjs::enable(selector = '.navbar-nav a[data-value="Execution"]')
      rv$nav_matrix[, 2] <- 0
      rv$nav_matrix[as.numeric(input$exec_type), 2] <- 1
      shinyjs::show(selector = '.navbar-nav a[data-value="Execution"]')
      shinyjs::show("run_back")
      shinyjs::hide("run_back3")
      updateTabsetPanel(session, "tabs", selected = "Execution")
      shinyjs::runjs("window.scrollTo(0, 0)")
    }
  })

  observeEvent(input$dat_next3, {
    if (!is.null(rv$dataframe) && !is.null(rv$js_select_model)) {
      # shinyjs::enable(selector = '.navbar-nav a[data-value="Execution"]')
      shinyjs::show(selector = '.navbar-nav a[data-value="Execution"]')
      rv$nav_matrix[, 2] <- 0
      rv$nav_matrix[as.numeric(input$exec_type), 2] <- 1
      updateTabsetPanel(session, "tabs", selected = "Execution")
      shinyjs::hide("run_back")
      shinyjs::show("run_back3")
      shinyjs::runjs("window.scrollTo(0, 0)")
    } else {
      showNotification("Upload required files!", type = "error")
    }
  })

  observeEvent(input$con_back, {
    updateTabsetPanel(session, "tabs", selected = "Dataset")
    shinyjs::runjs("window.scrollTo(0, 0)")
  })

  ### Execution #########################################################################################
  observeEvent(input$reset_cache, cache_clear())

  output$save_inputs <- downloadHandler(
    filename = function() {
      filename <- "Rosa_inputs.json"
      message("Downloading file: ", filename)
      return(filename)
    },
    content = function(file) {
      tryCatch(
        {
          req(rv$hypers)
          json_inputs <- mmm_wrapper(
            input$exec_type, input$robyn_path, rv$dataframe, input$int_use, input$int_sign,
            input$ts_val, input$trials, input$iters, input$date, input$sales, input$dep_var_type,
            input$conversion_units, input$currency, input$holidays_country,
            input$prophet_vars, input$prophet_signs, input$seed,
            input$window[1], input$window[2], input$adstock, rv$hypers,
            input$context_vars, rv$context_signs, input$paid_media_spends, rv$impressions,
            input$organic_vars, rv$organic_signs, input$factor_vars,
            input$add_pen, rv$calibration_input, input$cores, rv$user,
            return_inputs = TRUE
          )
          jsonlite::write_json(json_inputs, file, pretty = TRUE)
          rv$last_log <- log_action("download_inputs", json_inputs, user = rv$user)
        },
        error = function(e) {
          showNotification("Error", e$message, type = "error")
          rv$last_log <- log_action("error_download_inputs", e$message, user = rv$user)
        }
      )
    }
  )

  # Write Execution Summary
  output$summary <- renderTable(
    {
      shinyjs::html("console", "Preparing to run...")
      # Load reticulate and nevergrad (once per session)
      if (input$exec_type %in% c(1, 2)) {
        rv$reticulate_loaded <- load_reticulate(rv$reticulate_loaded)
      }
      shinyjs::html("console", sprintf(
        "<b>Ready to run...</b><br>Execution type: %s",
        dplyr::case_match(
          as.integer(input$exec_type),
          1 ~ "Train new models",
          2 ~ "Recreate existing model",
          3 ~ "Refresh model with new data",
          4 ~ "Recreate multiple models",
          TRUE ~ "Other non-registered"
        )
      ))

      rv$context_signs <- NULL
      if (length(input$context_signs) >= 1) {
        rv$context_signs <- lapply(1:length(input$context_signs), function(x) {
          ifelse(endsWith(input$context_signs[x], "d"), "default", ifelse(endsWith(input$context_signs[x], "p"), "positive", "negative"))
        })
      }
      rv$organic_signs <- NULL
      if (length(input$organic_signs) >= 1) {
        rv$organic_signs <- lapply(1:length(input$organic_signs), function(x) {
          ifelse(endsWith(input$organic_signs[x], "d"), "default", ifelse(endsWith(input$organic_signs[x], "p"), "positive", "negative"))
        })
      }

      t_s <- list("train_size" = if (input$train_size[1] == input$train_size[2]) {
        input$train_size[1]
      } else {
        input$train_size
      })

      s1a <- data.frame(
        Name = c(
          "Model", "Output folder",
          "Dep. variable", "Dep. var. Type",
          # "Market", "Brand",
          "Currency",
          "Date column", "Modeling Window",
          "Iterations", "Trials", "Steps", "Cores",
          "Spend Channels", "Impressions", "Organic vars", "Organic signs",
          "Context vars", "Context signs", "Factor vars",
          "Intercept", "Intercept sign", "Prophet vars", "Prophet signs", "Seed",
          "Country", "Split train/test", "Train size",
          "Penalty Factor", "Adstock"
        ),
        Value = as.character(c(
          ifelse(is.null(rv$js_select_model), TRUE, rv$js_select_model),
          ifelse(is.null(input$robyn_path), default_path, input$robyn_path),
          input$sales,
          input$dep_var_type,
          # ifelse(is.null(input$market), "", input$market),
          # ifelse(is.null(input$brand), "", input$brand),
          ifelse(is.null(input$currency), "", input$currency),
          input$date,
          paste(input$window, collapse = ", "),
          input$iters,
          input$trials,
          ifelse(is.null(input$steps), "N/A", input$steps),
          input$cores,
          paste(input$paid_media_spends, collapse = ", "),
          paste(rv$impressions, collapse = ", "),
          paste(input$organic_vars, collapse = ", "),
          paste(rv$organic_signs, collapse = ", "),
          paste(input$context_vars, collapse = ", "),
          paste(rv$context_signs, collapse = ", "),
          paste(input$factor_vars, collapse = ", "),
          input$int_use,
          input$int_sign,
          paste(input$prophet_vars, collapse = ", "),
          paste(input$prophet_signs, collapse = ", "),
          input$seed,
          input$holidays_country,
          input$ts_val,
          ifelse(isTRUE(input$ts_val), paste(unlist(t_s), collapse = ", "), 1),
          input$add_pen,
          input$adstock
        )),
        stringsAsFactors = FALSE
      )

      # Gather hyperparameters
      s1b_names <- if (input$adstock == "geometric") {
        unlist(lapply(channels(), function(i) {
          append(append(paste0(i, "_alphas"), paste0(i, "_gammas")), paste0(i, "_thetas"))
        }))
      } else {
        unlist(lapply(channels(), function(i) {
          append(append(append(paste0(i, "_alphas"), paste0(i, "_gammas")), paste0(i, "_shapes")), paste0(i, "_scales"))
        }))
      }
      s1b_values <- sapply(s1b_names, function(x) {
        paste(input[[x]], collapse = ", ")
      })
      s1b <- data.frame(
        Name = s1b_names,
        Value = as.character(s1b_values),
        stringsAsFactors = FALSE
      )
      rv$hypers <- append(setNames(lapply(s1b_names, function(x) input[[x]]), s1b_names), t_s)

      if (is_server()) s1a <- filter(s1a, !.data$Name %in% c("Output folder"))
      s1 <- filter(rbind(s1a, s1b), !.data$Name %in% c("Steps", "Model", "Market", "Currency")) %>%
        mutate(Name = gsub("_", " ", .data$Name))
      s2 <- filter(s1a, .data$Name %in% c(
        "Model", "Output folder", "Market", "Currency", "Cores", "Iterations", "Trials", "Steps", "Seed"
      )) %>%
        mutate(Name = gsub("_", " ", .data$Name))
      s3 <- filter(s1a, .data$Name %in% c("Model", "Output folder", "Seed", "Market", "Currency")) %>%
        mutate(Name = gsub("_", " ", .data$Name))

      if (input$exec_type == 1) {
        return(s1)
      }
      if (input$exec_type == 2) {
        return(s2)
      }
      if (input$exec_type == 3) {
        if (file.exists(rv$JS_datapath)) {
          temp <- robyn_read(rv$JS_datapath, quiet = TRUE)
          df <- data.frame(
            Name = names(temp$ExportedModel$hyper_values),
            Value = signif(unlist(temp$ExportedModel$hyper_values), 4)
          )
          rv$ts <- as.character(as.POSIXct(temp$ExportedModel$train_timestamp))
          rv$ts <- ifelse(length(rv$ts) == 0, "", rv$ts)
          other <- data.frame(Name = c("Model's Date"), Value = rv$ts)
          s3 <- rbind(s3, other, df) %>%
            mutate(Name = gsub("_", " ", .data$Name))
        }
        return(s3)
      }
    },
    striped = TRUE,
    hover = TRUE,
    spacing = "s",
    width = "100%"
  )

  # RUN Rosa RUN
  observeEvent(input$run_robyn, {
    rv$exec <- 0
    rv$nav_matrix[, 3] <- 0
    shinyjs::hide(selector = '.navbar-nav a[data-value="Results"]')
    shinyjs::disable("run_next")
    shinyjs::disable("run_robyn")
    path <- ifelse(is.null(input$robyn_path), default_path, input$robyn_path)
    updateTextInput(session, "robyn_path", value = path)
    tic("running_robyn")
    withCallingHandlers(
      {
        shinyjs::html("console", "<b>Running Robyn...</b><br>")
        output$moo_points <- NULL
        Sys.sleep(1.5)
        message(paste(format(Sys.Date(), "%Y-%m-%d"), paste0(format(Sys.time(), "%H:%M:%S"))))
        tryCatch(
          {
            all_negative <- unlist(lapply(rv$dataframe, function(x) all(x <= 0)))
            cols <- names(which(all_negative))
            rv$dataframe[cols] <- lapply(rv$dataframe[cols], function(x) abs(x))
            message(
              "Enabled cache: ",
              ifelse(use_cache && input$use_cache, TRUE, FALSE)
            )
            if (any(is.na(as.Date(rv$dataframe[, input$date][[1]], origin = "1970-01-01")))) {
              stop("There is an error with at least one of your dates. Please check them...")
            }
            if (use_cache) tic("robyn_fun")
            rv$ro <- with_cache(
              robyn_fun(
                input$exec_type, create_files, path, rv$dataframe,
                input$int_use, input$int_sign, input$ts_val, input$trials, input$iters,
                input$date, input$sales, input$dep_var_type, input$conversion_units, input$currency,
                input$holidays_country, input$prophet_vars, input$prophet_signs, input$seed,
                input$window[1], input$window[2], input$adstock, rv$hypers, input$context_vars, rv$context_signs,
                input$paid_media_spends, rv$impressions, input$organic_vars,
                rv$organic_signs, input$factor_vars, rv$JS_datapath, rv$js_select_model,
                input$steps, input$add_pen, rv$calibration_input, input$cores, rv$user
              ),
              use_cache = (use_cache && input$use_cache),
              overwrite = input$rewrite_cache
            )
            # Update models list and select the only available one when recreate or refresh
            choices <- rv$ro$modelselect$data$solID
            if (input$exec_type %in% c(1, 4)) choices <- c("Select a model", choices)
            updateSelectInput(session, "models", "Select a model:", choices = choices)

            # To enable dashboard to refresh and recreate
            if (input$exec_type %in% 2:3) {
              rv$rec$recreated[[rv$ro$OutputCollect$selectID]] <- rv$ro
              if (input$exec_type == 2) {
                old_name <- sprintf("Original (%s)", rv$ro$PrevModel$OutputCollect$selectID)
                rv$rec$recreated[[old_name]] <- rv$ro$PrevModel
                rv$ro$PrevModel <- NULL # To reduce memory use
                updateSelectInput(session, "models", "Select a model:", choices = c(choices, old_name))
              }
              updatePickerInput(session, "dash_models", choices = names(rv$rec$recreated), selected = names(rv$rec$recreated))
              brands <- sort(unique(unlist(lapply(rv$rec$recreated, function(x) x$Extras$brand))))
              updatePickerInput(session, "dash_brands", choices = brands, selected = brands)
              markets <- sort(unique(unlist(lapply(rv$rec$recreated, function(x) x$Extras$market))))
              updatePickerInput(session, "dash_markets", choices = markets, selected = markets)
            }

            if (input$exec_type %in% 2:4) {
              # To render notes, market, brand, etc. by passing Extras, treat as recreated
              if ("Extras" %in% names(rv$ro)) {
                rv$rec$recreated[[rv$ro$OutputCollect$selectID]]$Extras <- rv$ro$Extras
              }
              # No need for the step of Model Selection
              updateTabsetPanel(inputId = "results_tabs", selected = "One-Pagers")
              shiny::hideTab(inputId = "results_tabs", target = "Select Models")
            }
            rv$baseline_level <- 0
            # shinyjs::enable(selector = '.navbar-nav a[data-value="Results"]')
            rv$nav_matrix[as.numeric(input$exec_type), 3] <- 1
            # shinyjs::show(selector = '.navbar-nav a[data-value="Results"]')
            shinyjs::enable("run_next")

            # Logs
            if (input$exec_type == 1) {
              rv$last_log <- log_action(
                "outputs",
                extra = list(json = rv$ro$json),
                elapsed = toc_secs("robyn_fun"),
                user = rv$user
              )
            }
            if (input$exec_type == 3) {
              rv$ro$ModelsCollect$train_timestamp <- rv$ts
              rv$last_log <- log_action(
                "recreate",
                extra = list(solID = rv$ro$OutputCollect$selectID, json = rv$ro$json),
                elapsed = toc_secs("robyn_fun"), user = rv$user
              )
            }
            showNotification(paste("Execution completed in", toc("running_robyn")$time))
            rv$exec <- 1
          },
          error = function(e) {
            message(e$message)
            showNotification(e$message, type = "error")
            log_action("error_execute", e$message, user = rv$user)
            return()
          }
        )
        shinyjs::enable("run_robyn")
      },
      message = function(m) {
        rv$run_msg <- c(rv$run_msg, m$message)
        shinyjs::html(
          id = "console", html = paste0(m$message, "<br>"),
          add = input$live_logs
        )
      }
    )
  })

  # MOO Cloud plot
  observeEvent(input$run_robyn, {
    if (rv$exec == 1) {
      output$moo_points <- renderPlot(
        rv$ro$OutputCollect$OutputModels$convergence$moo_cloud_plot
      )
    } else {
      output$moo_points <- renderPlot(noPlot(" "))
    }
  })

  output$noplot <- renderPlot(noPlot(" "))
  output$noplot_select <- renderPlot(noPlot("Select a model"))
  output$noplot_ba0 <- renderPlot(noPlot("Select a model first"))
  output$noplot_ba1 <- renderPlot(noPlot("Run budget allocator to show max response results"))
  output$noplot_ba2 <- renderPlot(noPlot("Run budget allocator to show target value results"))

  observeEvent(input$run_next, {
    shinyjs::show(selector = '.navbar-nav a[data-value="Results"]')
    if (input$exec_type == 4) {
      shinyjs::hide("res_back")
      shinyjs::show("res_back4")
    } else {
      shinyjs::hide("res_back4")
      shinyjs::show("res_back")
    }
    updateTabsetPanel(session, "tabs", selected = "Results")
    shinyjs::runjs("window.scrollTo(0, 0)")
  })

  observeEvent(input$run_back, {
    updateTabsetPanel(session, "tabs", selected = "Configuration")
    shinyjs::runjs("window.scrollTo(0, 0)")
  })

  observeEvent(input$run_back3, {
    updateTabsetPanel(session, "tabs", selected = "Dataset")
    shinyjs::runjs("window.scrollTo(0, 0)")
  })

  ### Results #########################################################################################

  output$download_options <- renderUI(list(
    checkboxInput("json_only", "Model JSON file only", FALSE),
    checkboxInput("final_model", "Mark as final model", FALSE),
    if (Sys.getenv("AZURE_CONTAINER") == "mmm") {
      message("Enabled Azure uploads option")
      checkboxInput("azure_upload", "Upload model to Azure", FALSE)
    } else {
      NULL
    }
  ))

  mod_sel_snap <- function() {
    if (input$exec_type == 1) {
      solID <- extract_content_within_parentheses(input$models)
      selected_df <- data.frame(
        metrics = new_modsel()$metrics[new_modsel()$weights > 0],
        metrics_names = levels(new_modsel()$plot$data$name_metrics),
        weights = new_modsel()$weights[new_modsel()$weights > 0]
      )
      selected_values <- new_modsel()$data[new_modsel()$data$solID == solID, ] %>%
        select(all_of(selected_df$metrics)) %>%
        tidyr::gather("key" = "metrics")
      msel <- list(
        selected_df = left_join(selected_df, selected_values, "metrics"),
        baseline_ref = new_modsel()$baseline_ref,
        model_rank = which(new_modsel()$data$solID == solID),
        n_models = nrow(new_modsel()$data)
      )
      return(msel)
    } else {
      return(rv$ro$Extras$model_selection)
    }
  }

  # EXPORT (AND UPLOAD) MODEL AS JSON OR ZIP WHEN MODEL IS SELECTED
  observeEvent(input$models, {
    req(input$models)
    now_time <- gsub("\\-|\\:|\\ ", "", round(Sys.time()))
    if (!is.null(input$models) && input$models != "Select a model") {
      output$download_model <- renderUI(
        shiny::downloadButton(
          "save_model",
          label = if (!isTRUE(input$azure_upload)) "Export Selected Model" else "Upload to Azure and Export",
          icon = if (!isTRUE(input$azure_upload)) icon("download") else icon("upload"),
          style = "vertical-align: middle; width: 100%;"
        )
      )
      output$build_report <- renderUI(list(
        br(), shiny::downloadButton(
          "mmm_report",
          label = "Build PPTX Report",
          icon = icon("person-chalkboard"),
          style = "vertical-align: middle; width: 100%;"
        )
      ))
      output$save_model <- downloadHandler(
        filename = function() {
          if (!is.null(input$models) && input$models != "Select a model") {
            filename <- sprintf(
              "RosaModel-%s-%s-%s-%s.%s",
              extract_content_within_parentheses(input$models),
              now_time, input$market, input$brand,
              ifelse(!input$json_only, "zip", "json")
            )
          } else {
            filename <- "SELECT.MODEL"
          }
          message("Downloading file: ", filename)
          return(filename)
        },
        content = function(file) {
          tryCatch(
            {
              if (!is.null(input$models) && input$models != "Select a model") {
                if (input$brand == "" || input$market == "") {
                  showNotification(
                    "Please, define brand and market before exporting your model.",
                    type = "warning", duration = NULL
                  )
                  return(NULL)
                }
                if ((input$final_model || isTRUE(input$azure_upload)) &&
                  (is.null(input$add_notes) || isTRUE(nchar(input$add_notes) < 10))) {
                  showNotification(
                    "Please, add a significant note about your model before submitting.",
                    type = "warning", duration = NULL
                  )
                  return(NULL)
                }

                # We also want to add selected model's JSON first to the folder
                solID <- extract_content_within_parentheses(input$models)
                temp <- robyn_write(
                  rv$ro$InputCollect, rv$ro$OutputCollect, solID,
                  dir = rv$ro$Folder, export = TRUE, quiet = TRUE,
                  raw_data = rv$ro$InputCollect$dt_input,
                  currency = input$currency,
                  conversion_units = input$conversion_units,
                  brand = input$brand,
                  market = input$market,
                  final_model = input$final_model,
                  notes = input$add_notes,
                  baseline_level = input$baseline_level,
                  model_selection = mod_sel_snap()
                )
                if (input$exec_type == 3) {
                  temp$ModelsCollect$train_timestamp <- rv$ts
                  temp$ExportedModel$train_timestamp <- rv$ts
                }
                # Update and show notes
                rv$rec$notes[[input$models]] <- sprintf(
                  "<b>Brand:</b> %s<br><br><b>Market:</b> %s<br><br><b>%s:</b> %s<br><br><b>Notes:</b> %s",
                  input$brand, input$market,
                  ifelse(input$dep_var_type == "revenue", "Currency", "Units"),
                  input$currency, input$add_notes
                )

                # Upload file to Azure
                if (isTRUE(input$azure_upload)) {
                  if (!input$json_only) {
                    showNotification("Please, upload as 'JSON only' file", duration = NULL, type = "warning")
                  } else {
                    temp$Extras$mud_id <- rv$user
                    jsonlite::write_json(temp, file, pretty = TRUE, digits = 10)
                    filename <- sprintf(
                      "RosaModel-%s-%s-%s-%s.json",
                      extract_content_within_parentheses(input$models),
                      now_time, input$market, input$brand,
                      ifelse(!input$json_only, "zip", "json")
                    )
                    new_file <- paste("Markets", toupper(input$market), filename, sep = "/")
                    ul <- azure_upload(file, new_file)
                    showNotification(paste("Model uploaded:", new_file), "", type = "message")
                    temp <- append(list(filename = new_file), temp)
                    rv$last_log <- log_action("export_upload", temp, user = rv$user)
                  }
                }

                # Download local file as JSON or ZIP
                if (input$json_only) {
                  rv$last_log <- log_action("export_json", temp, user = rv$user)
                  suppressWarnings(jsonlite::write_json(temp, file, pretty = TRUE, digits = 10))
                } else {
                  these <- list.files(rv$ro$Folder, full.names = TRUE)
                  rv$last_log <- log_action("export_zip", append(list(filenames = these), temp), user = rv$user)
                  zip::zip(file, files = these, mode = "cherry-pick")
                }
                showNotification(paste("Model exported:", input$models), "", type = "message")
              } else {
                showNotification("Select a model first...", "", type = "warning")
              }
            },
            error = function(e) {
              message(e$message)
              showNotification("Error", e$message, type = "error", duration = NULL)
              log_action("error_export", e$message, user = rv$user)
            }
          )
        }
      )
    } else {
      output$download_model <- NULL
      output$build_report <- NULL
    }
  })
  observeEvent(input$azure_upload, {
    if (!isTRUE(input$json_only) && isTRUE(input$azure_upload)) {
      updateCheckboxInput(session, inputId = "json_only", value = TRUE)
    }
  })

  # BUILD & DOWNLOAD POWERPOINT PRESENTATION REPORT
  output$mmm_report <- downloadHandler(
    filename = function() {
      sprintf(
        "Rosa Report %s (%s) - %s.pptx",
        input$brand, input$market, input$models
      )
    },
    content = function(file) {
      disable("mmm_report")
      tryCatch(
        {
          if (input$brand == "" || input$market == "") {
            showNotification(
              "Please, define brand and market before building report.",
              type = "error", duration = NULL
            )
            return(NULL)
          }
          withProgress(message = "Generating report for", value = 0, {
            incProgress(0.5, detail = input$models)
            tic("mmm_report")
            rv$reports <- rv$reports + 1
            Extras <- list(
              raw_data = rv$ro$InputCollect$dt_input,
              currency = input$currency,
              conversion_units = input$conversion_units,
              brand = input$brand,
              market = input$market,
              final_model = input$final_model,
              notes = input$add_notes,
              baseline_level = input$baseline_level,
              model_selection = mod_sel_snap()
            )
            incProgress(0.1, detail = input$models)
            mmm_pptx(
              rv$ro$InputCollect,
              rv$ro$OutputCollect,
              Extras,
              extract_content_within_parentheses(input$models),
              template = system.file("www/RosaTemplate.pptx", package = "Rosa"),
              master = "Rosa",
              filename = file,
              ai_insights = FALSE,
              baseline_level = input$baseline_level,
              report_start = input$op_date[1],
              report_end = input$op_date[2]
            )
            incProgress(0.4, detail = input$models)
            rv$last_log <- log_action(
              "mmm_report", list(brand = input$brand, market = input$market, solID = input$models),
              elapsed = toc_secs("mmm_report"),
              user = rv$user
            )
            Sys.sleep(0.5)
            showNotification(paste("Report downloaded:", file), "", type = "message")
          })
        },
        error = function(e) {
          message(e$message)
          showNotification("Error", e$message, type = "error", duration = NULL)
          rv$last_log <- log_action("error_report", e$message, user = rv$user)
        }
      )
      enable("mmm_report")
    }
  )

  # MODEL SELECTOR TAB
  observe({
    if (input$exec_type == 1) {
      showTab(inputId = "results_tabs", target = "Select Models")
      updateTabsetPanel(inputId = "results_tabs", selected = "Select Models")
    } else {
      updateTabsetPanel(inputId = "results_tabs", selected = "One-Pagers")
      hideTab(inputId = "results_tabs", target = "Select Models")
    }
  })

  ############################## DASBOARD TAB
  observe({
    if (!input$exec_type %in% c(2:4)) {
      hideTab(inputId = "results_tabs", target = "Dashboard")
    } else {
      showTab(inputId = "results_tabs", target = "Dashboard")
      updateTabsetPanel(inputId = "results_tabs", selected = "Dashboard")
    }
  })
  observe({
    req(rv$rec)
    rv$rec$dates_range <- range(as.Date(unlist(lapply(rv$rec$recreated, function(x) {
      c(x$InputCollect$window_start, x$InputCollect$window_end)
    })), origin = "1970-01-01"))
    updateDateRangeInput(
      session, "pw_date_range",
      start = min(unlist(rv$rec$dates_range)), end = max(unlist(rv$rec$dates_range)),
      min = min(unlist(rv$rec$dates_range)), max = max(unlist(rv$rec$dates_range))
    )

    rv$rec$markets <- lapply(rv$rec$recreated, function(x) x$Extras$market)
    rv$rec$brands <- lapply(rv$rec$recreated, function(x) x$Extras$brand)
    rv$rec$notes_tbl <- lapply(rv$rec$recreated, function(x) x$Extras$note)
    rv$rec$currency <- lapply(rv$rec$recreated, function(x) x$Extras$currency)
    rv$rec$conversion_units <- lapply(rv$rec$recreated, function(x) x$Extras$conversion_units)
    rv$rec$window_start <- unlist(lapply(rv$rec$recreated, function(x) x$InputCollect$window_start))
    rv$rec$window_end <- unlist(lapply(rv$rec$recreated, function(x) x$InputCollect$window_end))
    rv$rec$intervalType <- unlist(lapply(rv$rec$recreated, function(x) x$InputCollect$intervalType))
    rv$rec$rowCount <- unlist(lapply(rv$rec$recreated, function(x) nrow(x$InputCollect$dt_input)))
    total_window <- unlist(lapply(rv$rec$recreated, function(x) x$InputCollect$rollingWindowLength))
    rv$rec$date_level <- sprintf("%s %ss", total_window, rv$rec$intervalType)
    metrics <- unlist(lapply(rv$rec$recreated, function(x) x$InputCollect$dep_var_type))
    metrics <- ifelse(metrics == "revenue", "ROAS", "CPA")
    rv$rec$metric <- ifelse(length(metrics) > 1, "ROAS | CPA", metrics)
  })

  # KPI BOXES
  output$rep_boxes <- renderUI({
    req(rv$rec$metric)
    return(splitLayout(
      shinydashboard::valueBox(rv$rec$metric, "Metric", icon = icon("gauge-high")),
      shinydashboard::valueBox(length(rv$rec$recreated), "Models", icon = icon("hashtag")),
      shinydashboard::valueBox(length(unique(unlist(rv$rec$markets))), "Markets", icon = icon("flag")),
      shinydashboard::valueBox(length(unique(unlist(rv$rec$brands))), "Brands", icon = icon("syringe"))
    ))
  })

  # SUMMARY TABLE
  output$report_summary <- gt::render_gt({
    req(rv$rec$metric)
    if (length(input$dash_models) > 0 & length(input$dash_brands) > 0 & length(input$dash_markets) > 0) {
      report_summary <- bind_rows(list_null_to_char( # list_null_to_char()?
        model = names(rv$rec$recreated),
        brand = rv$rec$brands,
        market = rv$rec$markets,
        unit = rv$rec$currency,
        # conversion_units = rv$rec$conversion_units,
        note = rv$rec$notes_tbl,
        model_start = rv$rec$window_start,
        model_end = rv$rec$window_end,
        dates_range = rv$rec$date_level
      )) %>%
        filter(extract_content_within_parentheses(.data$model) %in%
          extract_content_within_parentheses(input$dash_models)) %>%
        filter(
          .data$market %in% input$dash_markets,
          .data$brand %in% input$dash_brands
        ) %>%
        removenacols() %>%
        mutate_at(c("model_start", "model_end"), function(x) as.Date(x, origin = "1970-01-01"))
      colnames(report_summary) <- gsub("_", " ", cleanText(
        colnames(report_summary),
        keep = "_", title = TRUE
      ))
      ret <- report_summary %>%
        {
          if (all(rv$rec$notes_tbl == "")) select(., -any_of("Note")) else .
        } %>%
        gt() %>%
        tab_options(
          column_labels.font.weight = "bold",
          table.width = "100%"
        ) %>%
        opt_interactive(
          use_highlight = TRUE,
          use_sorting = TRUE,
          use_compact_mode = TRUE,
          use_pagination = FALSE
        )
      return(ret)
    } else {
      return(NULL)
    }
  })

  # INTERACTIVE PERFORMANCE TABLE
  observe({
    req(rv$rec$metric)
    req(input$dash_models)
    these <- rv$rec$recreated[
      names(rv$rec$recreated) %in% input$dash_models &
        unlist(lapply(rv$rec$recreated, function(x) {
          x$Extras$brand %in% input$dash_brands &
            x$Extras$market %in% input$dash_markets
        }))
    ]
    if (length(these) > 0) {
      pw <- lapply(these, function(x) {
        robyn_performance(
          x$InputCollect, x$OutputCollect,
          solID = x$OutputCollect$selectID,
          start_date = input$pw_date_range[1],
          end_date = input$pw_date_range[2],
          carryovers = "Carryover Response" %in% input$pw_options,
          marginals = "Marginal Performance" %in% input$pw_options,
          non_promo = "Non-Promo" %in% input$pw_options,
          new_version = FALSE
        )
      })
      report_perfo <- bind_rows(pw) %>%
        {
          if (!"Metric" %in% input$pw_options) select(., -any_of("metric")) else .
          if (!"Variable Type" %in% input$pw_options) select(., -any_of("type")) else .
        }
      # Add brands & markets
      bam <- bind_rows(list_null_to_char(
        solID = extract_content_within_parentheses(names(rv$rec$recreated)),
        brand = rv$rec$brands,
        market = rv$rec$markets
      ))
      report_perfo <- left_join(report_perfo, bam, "solID") %>%
        select(c("solID", "market", "brand"), everything())
      report_perfo[sapply(report_perfo, is.infinite)] <- NA
      rv$report_perfo <- report_perfo
    } else {
      rv$report_perfo <- NULL
    }
    # Download performance table button
    output$download_perf <- downloadHandler(
      filename = function() {
        paste0("perfreport-", paste0(cleanText(input$pw_date_range), collapse = "_"), ".csv")
      },
      content = function(file) {
        write.csv(rv$report_perfo, file, row.names = FALSE)
      }
    )
  })
  output$report_perfo <- gt::render_gt({
    req(rv$report_perfo)
    if (nrow(rv$report_perfo) > 0 && !is.nan(rv$report_perfo$performance[1])) {
      rv$report_perfo %>%
        mutate(channel = gsub("_", " ", .data$channel)) %>%
        {
          if (!"Date Range" %in% input$pw_options) select(., -contains("_date")) else .
        } %>%
        {
          if (!"Brand" %in% input$pw_options) select(., -.data$brand) else .
        } %>%
        {
          if (!"Market" %in% input$pw_options) select(., -.data$market) else .
        } %>%
        gt() %>%
        gtExtras::gt_plt_bar_pct(
          column = "contribution", scaled = FALSE, labels = TRUE,
          fill = "blue", background = "lightblue"
        ) %>%
        # This next row is avoiding opt_interactive() to work correctly in gt v0.9.0 (and v0.11.1)
        fmt_number(suffixing = TRUE) %>%
        fmt_percent(columns = gt::matches("carryover"), decimals = 0) %>%
        gtExtras::gt_highlight_rows(
          rows = which(rv$report_perfo$channel == "PROMOTIONAL TOTAL"),
          fill = "#80bcd8"
        ) %>%
        gtExtras::gt_highlight_rows(
          rows = which(rv$report_perfo$channel == "BASELINE"),
          fill = "grey80"
        ) %>%
        gtExtras::gt_highlight_rows(
          rows = which(rv$report_perfo$channel == "GRAND TOTAL"),
          fill = "grey50"
        ) %>%
        cols_label(
          solID = "Model ID",
          gt::matches("start_date") ~ "From Date",
          gt::matches("end_date") ~ "To Date",
          gt::matches("metric") ~ "Metric",
          gt::matches("type") ~ "Variable Type",
          gt::matches("brand") ~ "Brand",
          gt::matches("market") ~ "Market",
          channel = "Channel",
          performance = rv$rec$metric,
          gt::matches("marginal") ~ "Marginal",
          spend = "Input*",
          response = "Response",
          contribution = "Total Contribution",
          gt::matches("carryover") ~ "Carryover Response"
        ) %>%
        gt::tab_footnote("*Input is spend amounts for paid channels in currency or impressions for organic channels.") %>%
        tab_options(
          column_labels.font.weight = "bold",
          table.width = "100%",
          table.font.size = 12
        ) %>%
        # Asked for fix on fmt_number() bug: https://github.com/rstudio/gt/issues/1942
        # Solution: reactR >= 0.6.1
        opt_interactive(
          use_filters = TRUE,
          use_search = TRUE,
          use_sorting = TRUE,
          use_compact_mode = TRUE,
          use_pagination = FALSE
        )
    }
  })

  # ERRORS TABLE
  output$report_errors <- gt::render_gt({
    req(rv$rec$metric)
    req(input$dash_models)
    these <- rv$rec$recreated[
      names(rv$rec$recreated) %in% input$dash_models &
        unlist(lapply(rv$rec$recreated, function(x) {
          x$Extras$brand %in% input$dash_brands &
            x$Extras$market %in% input$dash_markets
        }))
    ]
    if (length(these) > 0) {
      rw <- lapply(these, function(x) robyn_write(x$InputCollect, x$OutputCollect, export = FALSE))
      report_errors <- tibble(model = names(these)) %>%
        cbind(dplyr::bind_rows(lapply(rw, function(x) signif(x$ExportedModel$errors, 3)))) %>%
        removenacols() %>%
        mutate(model = sapply(.data$model, extract_content_within_parentheses))
      colnames(report_errors) <-
        gsub(
          "mape", "MAPE",
          gsub(
            "decomp.rssd", "DECOMP.RSSD",
            gsub(
              "nrmse", "NRMSE",
              gsub(
                "rsq", "R^2",
                gsub("_", " ", colnames(report_errors))
              )
            )
          )
        )
      report_errors %>%
        gt() %>%
        cols_label(model = "Model ID") %>%
        tab_options(
          column_labels.font.weight = "bold",
          table.width = "100%",
        ) %>%
        opt_interactive(
          use_sorting = TRUE,
          use_compact_mode = TRUE,
          use_pagination = FALSE
        )
    } else {
      NULL
    }
  })

  # MODEL SELECTOR PLOT
  output$slider_2 <- renderUI(
    sliderInput(
      "slider_2", ifelse(
        rv$ro$InputCollect$dep_var_type == "revenue", "High ROAS", "Low CPA"
      ),
      width = "100%", min = 0, max = 100, value = 5
    )
  )
  weights <- reactive(c(
    input$slider_1, input$slider_2, input$slider_4, input$slider_5,
    input$slider_6, input$slider_8, input$slider_9, input$slider_3
  ))
  new_modsel <- reactive({
    req(input$slider_2)
    if (length(rv$ro$OutputCollect$allSolutions) > 1) {
      if (!all(weights() == 0)) {
        ret <- robyn_modelselector(
          rv$ro$InputCollect, rv$ro$OutputCollect,
          metrics = c(
            "rsq_train", "performance", "non_zeroes",
            "incluster_models", "baseline_dist",
            "certainty", "cluster_sd", "decomp.rssd"
          ),
          spend_wt = TRUE,
          penalization = 2,
          baseline_ref = input$slider_7 / 100,
          top = 4,
          wt = weights(),
          cache_dir = tempdir(),
          quiet = TRUE
        )
      } else {
        ret <- list(
          data = data.frame(solID = rv$ro$OutputCollect$allSolutions),
          plot = noPlot("At least one criteria must be used to rank the models.")
        )
      }
    } else {
      ret <- list(
        data = data.frame(solID = rv$ro$OutputCollect$allSolutions),
        plot = noPlot("Only 1 model available. Skip this step.")
      )
      hideTab(inputId = "results_tabs", target = "Select Models")
    }
    updateSelectInput(session, "models", "Select a model:", append(
      "Select a model", ret$data$solID
    ), "Select a model")
    return(ret)
  })
  output$modelselector <- renderPlot(
    if (length(rv$ro$OutputCollect$allSolutions) > 1) {
      new_modsel()$plot
    } else {
      NULL
    }
  )

  # SELECTED MODEL
  observeEvent(input$models, {
    req(input$models)
    if (input$exec_type %in% 4) {
      if (!is.null(input$models) && input$models != "Select a model") {
        df <- rv$rec$recreated[[input$models]]$InputCollect$dt_input
        rv <- process_data(df, rv, input, session)
        # Update InputCollect & OutputCollect
        rv$ro$InputCollect <- rv$rec$recreated[[input$models]]$InputCollect
        rv$ro$OutputCollect <- rv$rec$recreated[[input$models]]$OutputCollect
        class(rv$ro$InputCollect) <- c("robyn_inputs", class(rv$ro$OutputCollect))
        class(rv$ro$OutputCollect) <- c("robyn_outputs", class(rv$ro$OutputCollect))
        rv$baseline_level <- 0
        # Budget allocator reactive values
        rv$date_range_sce <- format(c(input$date_range[1], input$date_range[2]), "%Y-%m-%d")
        rv$sum_budget <- budget_calc(
          rv$ro$InputCollect$dt_input, rv$ro$InputCollect$paid_media_spends,
          rv$ro$InputCollect$date_var, rv$date_range_sce
        )
      }
    }
    updateSelectInput(session, "market", "Market",
      selected = rv$ro$InputCollect$prophet_country
    )
    rv$select_model <- select_model <- ifelse(
      grepl("\\(", input$models),
      gsub("\\(|\\)", "", regmatches(
        input$models, regexpr("\\((.*?)\\)", input$models)
      )), input$models
    )
    updateDateRangeInput(
      session,
      inputId = "op_date",
      start = rv$ro$InputCollect$window_start,
      end = next_date(rv$ro$InputCollect$dt_mod$ds) - 1
    )
    shinyWidgets::updateAutonumericInput(session, "target", label = ifelse(
      isolate(rv$ro$InputCollect$dep_var_type) == "revenue",
      "Target ROAS", "Target CPA"
    ))
    shinyjs::hide("sce_pen")
    shinyjs::hide("sce_ok_pen")
  })

  # Available Brands
  brands_file <- system.file("www/brands.txt", package = "Rosa")
  if (file.exists(brands_file)) {
    message("Enabled brands list correctly from brands.txt")
    rv$brands_list <- brands_list <- readLines(brands_file)
    output$brand <- renderUI(selectInput("brand", "Brand", c("", rv$brands_list), selected = "", width = "100%"))
  } else {
    message("No enabled brands on brands.txt")
    rv$brands_list <- brands_list <- ""
    output$brand <- renderUI(textInput("brand", "Brand", "", width = "100%"))
  }
  observe({
    req(input$models)
    req(rv$rec$recreated)
    if (input$models != "Select a model") {
      # Set brand as exported brand if available
      brand <- rv$rec$recreated[[input$models]]$Extras$brand
      if (!is.null(brand)) {
        message("Imported brand: ", brand)
        if (brand %in% rv$brands_list) {
          output$brand <- renderUI(selectInput("brand", "Brand", c("", rv$brands_list), selected = brand, width = "100%"))
        } else {
          output$brand <- renderUI(textInput("brand", "Brand", brand, width = "100%"))
        }
      }
      # Set market country same as prophet's country for selected model
      market <- rv$rec$recreated[[input$models]]$Extras$market
      if (isTRUE(market %in% countries)) {
        updateSelectInput(session, "market", selected = market)
      }
      # Import notes from model if available
      notes <- rv$rec$recreated[[input$models]]$Extras$notes
      updateTextInput(session, "add_notes", value = notes)
    }
  })

  # One-pagers rendered as an image
  observeEvent(c(input$models, input$baseline_level, input$op_date), {
    output$one_pager <- renderUI({
      if (input$models == "Select a model" || input$op_date[1] == Sys.Date()) {
        return(plotOutput("noplot_select"))
      } else {
        withProgress(message = "Generating one-pager for", value = 0, {
          incProgress(0.5, detail = input$models)
          msg <- paste("Generating one-pager:", v2t(input$models, quotes = FALSE))
          if (input$baseline_level > 0) msg <- sprintf("%s [Aggr. level: %s]", msg, input$baseline_level)
          message(msg)
          if (!dir.exists(rv$ro$OutputCollect$plot_folder)) {
            message("Creating directory: ", rv$ro$OutputCollect$plot_folder)
            dir.create(rv$ro$OutputCollect$plot_folder)
          }
          onepager <- Rosa_onepagers(
            rv$ro$InputCollect, rv$ro$OutputCollect,
            select_model = isolate(rv$select_model),
            model_name = isolate(rv$select_model),
            baseline_level = input$baseline_level,
            report_start = input$op_date[1],
            report_end = input$op_date[2],
            export = TRUE
          )
          rv$baseline_level <- input$baseline_level
          tryCatch(
            {
              f <- fix_double_slash(paste0(rv$ro$Folder, "/", paste0(isolate(rv$select_model), ".png")))
              output$op_img <- renderImage(list(src = f, width = "100%"), deleteFile = FALSE)
              message("Rendering: ", f)
              return(imageOutput("op_img", height = "auto"))
            },
            error = function(e) message("No one-pagers available")
          )
        })
      }
    })
  })

  # BUDGET ALLOCATOR BUDGET, LEVERS AND CONSTRAINS
  observe({
    req(input$models)
    req(input$date_range)
    if (!is.null(rv$ro$InputCollect)) {
      rv$date_range_sce <- format(c(input$date_range[1], input$date_range[2]), "%Y-%m-%d")
      rv$sum_budget <- budget_calc(
        rv$ro$InputCollect$dt_input, rv$ro$InputCollect$paid_media_spends,
        rv$ro$InputCollect$date_var, rv$date_range_sce
      )
      # When date range changes, recalculate budget
      updateAutonumericInput(session, "budget", value = round(rv$sum_budget, 0))
      # Calculate relative selected values compared with averages
      lapply(rv$ro$InputCollect$paid_media_spends, function(x) {
        bc <- budget_calc(rv$ro$InputCollect$dt_input, x, rv$ro$InputCollect$date_var, rv$date_range_sce)
        output[[paste0(x, "_Constraints_N")]] <- renderText({
          sprintf(
            "%s - %s [%s]",
            round(input[[paste0(x, "_Constraints")]][1] / (bc + 1), 2),
            round(input[[paste0(x, "_Constraints")]][2] / (bc + 1), 2),
            formatNum(bc, abbr = TRUE)
          )
        })
      })
    }
  })
  output$ch_low_up <- renderUI({
    req(input$models)
    req(rv$ro$InputCollect)
    if (input$models != "Select a model" && isTRUE(input$budget > 0)) {
      rate <- input$budget / budget_calc(
        rv$ro$InputCollect$dt_input, rv$ro$InputCollect$paid_media_spends,
        rv$ro$InputCollect$date_var, rv$date_range_sce
      )
      updateAutonumericInput(
        session, "budget",
        label = sprintf(
          "Total budget (based on date range x%s):", signif(rate, 3)
        )
      )
      lapply(rv$ro$InputCollect$paid_media_spends, function(i) {
        temp <- budget_calc(rv$dataset, i, rv$ro$InputCollect$date_var, rv$date_range_sce)
        fluidRow(
          splitLayout(
            sliderInput(
              paste0(i, "_Constraints"),
              label = paste("Constraints:", i),
              min = 0,
              max = round(1 + 3 * temp * rate, 0),
              value = c(
                signif(0.75 * temp * rate, 4),
                signif(1.50 * temp * rate, 4)
              ),
              step = round(0.01 + 0.01 * temp * rate, 0),
              width = "100%"
            ),
            textOutput(paste0(i, "_Constraints_N")),
            cellWidths = c("85%", "15%")
          )
        )
      })
    }
  })
  # When setting target ROAS/CPA, calculate responses to give a notion
  observeEvent(c(input$target, input$target_upper, input$models, input$date_range), {
    req(input$models)
    req(input$target)
    if (!is.null(rv$ro$InputCollect) && !input$models %in% "Select a model") {
      if (rv$date_range_sce[1] < max(rv$ro$InputCollect$dt_mod$ds)) {
        rv$target_sales_df <- try(robyn_allocator(
          InputCollect = rv$ro$InputCollect,
          OutputCollect = rv$ro$OutputCollect,
          select_model = extract_content_within_parentheses(input$models),
          target_value = input$target,
          date_range = rv$date_range_sce,
          scenario = "target_efficiency",
          channel_constr_low = 0.1,
          channel_constr_up = isolate(input$target_upper),
          export = FALSE, quiet = TRUE
        )$dt_optimOut)
        if (length(rv$target_sales_df) > 1) {
          temp <- robyn_performance(
            InputCollect = rv$ro$InputCollect,
            OutputCollect = rv$ro$OutputCollect,
            solID = rv$select_model,
            start_date = rv$date_range_sce[1],
            end_date = rv$date_range_sce[2]
          )
          baseline <- temp$response[temp$channel == "GRAND TOTAL"] -
            sum(temp$response[temp$channel %in% rv$ro$InputCollect$paid_media_spends])
          incremental <- rv$target_sales_df$optmResponseTotal[1]
          output$target_boxes1 <- shinydashboard::renderValueBox(
            shinydashboard::valueBox(formatNum(baseline, signif = 3, abbr = TRUE), "Total Baseline (non-paid)", icon = icon("hashtag"))
          )
          output$target_boxes2 <- shinydashboard::renderValueBox(
            shinydashboard::valueBox(formatNum(incremental, signif = 3, abbr = TRUE), "Optimized Incrementality", icon = icon("plus"))
          )
          output$target_boxes3 <- shinydashboard::renderValueBox(
            shinydashboard::valueBox(
              formatNum(baseline + incremental, signif = 3, abbr = TRUE),
              paste("Total", cleanText(rv$ro$InputCollect$dep_var_type, title = TRUE)),
              icon = icon("equals")
            )
          )
        }
      }
    }
  })

  # BUILD BUDGET ALLOCATOR SCENARIOS
  observeEvent(input$sce_robyn, {
    if (!is.null(input$models) && input$models != "Select a model" && input$budget > 0) {
      nsteps <- 4
      withProgress(message = "Allocator", value = 0, {
        incProgress(1 / nsteps, detail = "Calculating both scenarios")
        shinyjs::show("sce_pen")
        tic("allocator")
        const_low <- NULL
        const_up <- NULL
        for (ch in rv$ro$InputCollect$paid_media_spends) {
          channel_low_up <- paste0(ch, "_Constraints")
          temp <- budget_calc(rv$ro$InputCollect$dt_input, ch, rv$ro$InputCollect$date_var, rv$date_range_sce)
          const_low <- append(const_low, round(input[[channel_low_up]][1] / (temp + 1), 2))
          const_up <- append(const_up, round(input[[channel_low_up]][2] / (temp + 1), 2))
        }
        if (length(const_low) > 0) {
          ba <- robyn_sce(
            rv$ro$InputCollect, rv$ro$OutputCollect, input$models,
            create_files, rv$date_range_sce, input$budget,
            const_low, const_up, input$ch_c,
            input$target, input$target_upper
          )

          # Log results
          logs <- list(
            solID = input$models,
            max_response = list(
              dt_optimOut = ba$AllocatorCollect1$dt_optimOut,
              mainPoints = ba$AllocatorCollect1$mainPoints
            ),
            target_efficiency = list(
              dt_optimOut = ba$AllocatorCollect2$dt_optimOut,
              mainPoints = ba$AllocatorCollect2$mainPoints
            )
          )
          incProgress(1 / nsteps, detail = "Logging allocator...")
          rv$last_log <- log_action("allocator", extra = logs, elapsed = toc_secs("allocator"), user = rv$user)

          # When any of the selected dates has changed to nearest existing date
          dr <- c(ba$AllocatorCollect1$dt_optimOut$date_min[1], ba$AllocatorCollect1$dt_optimOut$date_max[1])
          if (!all(input$date_range == dr)) {
            showNotification(
              sprintf(
                "NOTE: Date range provided was modified to match available dates in data (%s to %s)",
                paste(input$date_range, collapse = ":"), paste(dr, collapse = ":")
              ),
              type = "warning", duration = NULL
            )
          }
          # When the entire budget is not able to be allocated because of the constraints
          if (round(ba$AllocatorCollect1$dt_optimOut$optmSpendTotal[1] * 1.001) < input$budget |
            round(ba$AllocatorCollect1$dt_optimOut$optmSpendTotal[1] * 0.999) > input$budget) {
            showNotification(
              "NOTE: Given the upper/lower constrains, the total budget can't be fully allocated (^)",
              type = "warning", duration = NULL
            )
          }

          # Files to plot
          fbase <- gsub("//", "/", file.path(rv$ro$Folder, gsub("\\(|\\)", "", sub(".+\\s", "", input$models))))
          matricname <- ifelse(rv$ro$InputCollect$dep_var_type == "revenue", "roas", "cpa")
          incProgress(1 / nsteps, detail = "Painting Max Response")
          Sys.sleep(0.5)
          file1 <- paste0(fbase, "_reallocated_best_", matricname, ".png")
          output$mr_img <- renderImage(list(src = file1, width = "100%"), deleteFile = FALSE)
          output$mr <- renderUI({
            if (is.null(input$models) || input$models == "Select a model") {
              return(plotOutput("noplot_ba0"))
            } else {
              message("Rendering: ", file1)
              if (file.exists(file1)) {
                return(imageOutput("mr_img", height = "auto"))
              } else {
                return(plotOutput("noplot_ba1"))
              }
            }
          })

          incProgress(1 / nsteps, detail = "Painting Target ROAS")
          Sys.sleep(0.5)
          file2 <- paste0(fbase, "_reallocated_target_", matricname, ".png")
          output$te_img <- renderImage(list(src = file2, width = "100%"), deleteFile = FALSE)
          output$te <- renderUI({
            if (is.null(input$models) || input$models == "Select a model") {
              plotOutput("noplot_ba0")
            } else {
              message("Rendering: ", file2)
              if (file.exists(file2)) {
                return(imageOutput("te_img", height = "auto"))
              } else {
                return(plotOutput("noplot_ba2"))
              }
            }
          })

          shinyjs::hide("sce_pen")
          shinyjs::show("sce_ok_pen")
          incProgress(1 / nsteps, detail = "Ready!")
          Sys.sleep(0.5)
        }
      })
    } else {
      showNotification("Select a model first...", "", type = "warning")
    }
  })

  ######### OTHER PLOTS TAB
  output$otherplotsopts <- renderUI({
    images <- list.files(
      file.path(rv$ro$Folder),
      pattern = "^([a-zA-Z]+_?)+{1}\\d?\\.png$", full.names = TRUE
    )
    if (length(images) > 0) {
      names(images) <- file_name(images)
      return(list(
        helpText("Additional plots to help you understand the models:"),
        selectInput("select_other_plot", "Select Plot to Render:", choices = images, selected = 1),
        imageOutput("otherplotsrendered", height = "auto"),
        hr()
      ))
    } else {
      NULL
    }
  })
  output$otherplotsrendered <- renderImage(
    {
      req(input$select_other_plot)
      list(src = input$select_other_plot, width = "100%")
    },
    deleteFile = FALSE
  )

  observeEvent(input$res_back, {
    updateTabsetPanel(session, "tabs", selected = case_when(
      as.integer(input$exec_type) %in% 1:3 ~ "Execution",
      as.integer(input$exec_type) %in% 4 ~ "Dataset",
      TRUE ~ "Execution"
    ))
    shinyjs::runjs("window.scrollTo(0, 0)")
  })

  observeEvent(input$res_back4, {
    updateTabsetPanel(session, "tabs", selected = case_when(
      as.integer(input$exec_type) %in% 1:3 ~ "Execution",
      as.integer(input$exec_type) %in% 4 ~ "Dataset",
      TRUE ~ "Execution"
    ))
    shinyjs::runjs("window.scrollTo(0, 0)")
  })

  observeEvent(input$models, {
    if (!input$models == "Select a model") {
      output$selected_json_txt <- renderUI({
        temp <- isolate(robyn_write(
          rv$ro$InputCollect, rv$ro$OutputCollect,
          extract_content_within_parentheses(input$models),
          export = FALSE, quiet = TRUE,
          currency = input$currency,
          conversion_units = input$conversion_units,
          brand = input$brand,
          market = input$market,
          final_model = input$final_model,
          notes = input$add_notes,
          baseline_level = input$baseline_level,
          model_selection = mod_sel_snap(),
          raw_data = rv$ro$InputCollect$dt_input
        ))
        output$json_txt <- renderPrint(jsonlite::toJSON(temp, pretty = TRUE))
        shiny::tagList(
          h3(paste("JSON for model", isolate(input$models))),
          verbatimTextOutput("json_txt"),
          hr()
        )
      })
    } else {
      output$selected_json_txt <- NULL
    }
  })

  ####### DASHBOARD
  observeEvent(input$dat_next4, {
    if (length(rv$ro) > 0) {
      # shinyjs::enable(selector = '.navbar-nav a[data-value="Results"]')
      rv$nav_matrix[as.numeric(input$exec_type), 3] <- 1
      shinyjs::show(selector = '.navbar-nav a[data-value="Results"]')
      shinyjs::hide("res_back")
      shinyjs::show("res_back4")
      updateTabsetPanel(session, "tabs", selected = "Results")
    } else {
      showNotification("Load required files!", type = "error")
    }
  })

  # Pre-saved dashboard projects
  observe({
    if (input$exec_type == 4) {
      isolate({
        rv$projects <- gsub("_", " ", c(isolate(rv$projects), lares::file_name(
          list.files(system.file("Dashboards", package = "Rosa"))
        )))
        # Show projects to specific users (to show to all users, don't add or set "all")
        access_file <- system.file("www/user_access.json", package = "Rosa")
        if (file.exists(access_file)) {
          message("Limiting access of projects based on user_access.json to ", rv$user)
          permissions <- jsonlite::read_json(access_file, simplifyVector = TRUE)
          allow <- lapply(rv$projects, function(p) {
            if (p %in% names(permissions)) {
              any(c("all", rv$user) %in% permissions[[p]])
            } else {
              TRUE
            }
          })
          rv$projects <- unique(rv$projects[unlist(allow)])
        }
        updateSelectizeInput(session, "projects", choices = rv$projects)
      })
    }
  })

  # Process new dashboard from a group of JSON files
  observeEvent(input$dash, {
    output$dash_name <- renderUI(
      textInput("dash_name", "Name for the project:", "Current Project")
    )
    output$dash_files <- renderTable(dplyr::tibble(
      "File Uploaded" = input$dash$name,
      "File Size" = paste0(signif(input$dash$size / (1024 * 1024), 2), " MB")
      # "Files Created" = input$dash$datapath
    ))
    output$dash_process <- renderUI(
      actionButton(
        "dash_process", "The files uploaded and identified are correct. Import and process them...",
        width = "100%", icon = icon("check")
      )
    )
  })
  observeEvent(input$dash_process, {
    req(input$dash_process)
    tic("dashboard_build")
    rv$rec <- tryCatch(
      {
        process_bundle(
          input$dash$datapath,
          input$dash$name,
          path = tempdir(),
          # Only to be used by dev to create new saved project available for others:
          save = TRUE && is_rstudio(),
          project = input$dash_name
        )
      },
      error = function(e) {
        showNotification(paste("Error recreating models:", e), "", type = "error", duration = NULL)
        return(NULL)
      }
    )
    if (!is.null(rv$rec)) {
      withProgress(message = "Loading project", value = 0.8, {
        rv$dash_sols <- gsub("\\.png", "", basename(rv$rec$onepagers))
        updatePickerInput(session, "dash_models", choices = rv$dash_sols, selected = rv$dash_sols)
        brands <- sort(unique(unlist(lapply(rv$rec$recreated, function(x) x$Extras$brand))))
        updatePickerInput(session, "dash_brands", choices = brands, selected = brands)
        markets <- sort(unique(unlist(lapply(rv$rec$recreated, function(x) x$Extras$market))))
        updatePickerInput(session, "dash_markets", choices = markets, selected = markets)
        rv$ro$Folder <- paste0(tempdir(), "/Dashboard")
        updateTextInput(session, "robyn_path", value = rv$ro$Folder)
        updateSelectizeInput(session, "projects", choices = sort(c(input$dash_name, rv$projects)), selected = input$dash_name)
        updateSelectInput(session, "models", "Select a model:", append("Select a model", rv$dash_sols))
        incProgress(0.1, detail = "Logging")
        rv$last_log <- log_action(
          "dashboard_build",
          extra = list(project = input$dash_name, solID = rv$dash_sols),
          elapsed = toc_secs("dashboard_build"), user = rv$user
        )
        incProgress(0.1, detail = "Loaded project")
        Sys.sleep(2)
      })
    }
  })

  # When loading a saved project
  observeEvent(input$dash_go, {
    req(input$projects)
    withProgress(message = "Loading project", value = 0, {
      tic("dashboard_load")
      incProgress(1 / 3)
      p <- gsub(" ", "_", input$projects)
      filename <- system.file(paste0("Dashboards/", p, ".RDS"), package = "Rosa")
      message("Reading: ", filename)
      rv$rec <- readRDS(filename)
      new_dir <- paste0(tempdir(), "/Dashboard", collapse = "")
      dir.create(new_dir, showWarnings = FALSE)
      suppressWarnings(file.copy(
        normalizePath(dirname(filename)),
        normalizePath(new_dir),
        recursive = TRUE, overwrite = TRUE
      ))
      rv$dash_sols <- gsub("\\.png", "", basename(rv$rec$onepagers))
      updatePickerInput(session, "dash_models", choices = rv$dash_sols, selected = rv$dash_sols)
      brands <- sort(unique(unlist(lapply(rv$rec$recreated, function(x) x$Extras$brand))))
      updatePickerInput(session, "dash_brands", choices = brands, selected = brands)
      markets <- sort(unique(unlist(lapply(rv$rec$recreated, function(x) x$Extras$market))))
      updatePickerInput(session, "dash_markets", choices = markets, selected = markets)
      rv$ro$Folder <- paste0(new_dir, "/", p, "/")
      rv$rec$recreated <- lapply(rv$rec$recreated, function(x) {
        x$OutputCollect$plot_folder <- rv$ro$Folder
        return(x)
      })
      updateTextInput(session, "robyn_path", value = rv$ro$Folder)
      updateSelectInput(session, "models", "Select a model:", append("Select a model", rv$dash_sols))
      incProgress(1 / 3)
      rv$last_log <- log_action(
        "dashboard",
        extra = list(project = input$projects, n_models = length(rv$dash_sols)),
        elapsed = toc_secs("dashboard_load"), user = rv$user
      )
      incProgress(1 / 3, detail = input$projects)
      Sys.sleep(2)
    })
  })

  output$dash_txt <- renderText({
    req(input$models)
    if (input$models != "Select a model") {
      if ("brand" %in% names(rv$rec$recreated[[input$models]][["Extras"]])) {
        sprintf(
          "<b>Brand:</b> %s<br><br><b>Market:</b> %s<br><br><b>%s:</b> %s<br><br><b>Notes:</b> %s",
          rv$rec$recreated[[input$models]]$Extras$brand,
          rv$rec$recreated[[input$models]]$Extras$market,
          ifelse(isolate(input$dep_var_type) == "revenue", "Currency", "Units"),
          ifelse(is.null(rv$rec$recreated[[input$models]]$Extras$currency), "Not defined",
            rv$rec$recreated[[input$models]]$Extras$currency
          ),
          rv$rec$recreated[[input$models]]$Extras$notes
        )
      } else {
        if (input$models %in% names(rv$rec$notes)) {
          rv$rec$notes[[input$models]]
        }
      }
    }
  })

  # HELP / DOCUMENT / PDF / Markdown
  # output$pdfview <- renderUI({
  #   tags$iframe(src = "www/tutorial.pdf", width = "100%", height = 800, frameborder = 0, marginheight = 0, marginwidth = 0)
  # })

  output$rendered_docs <- renderUI({
    markd <- system.file("www/documentation.md", package = "Rosa")
    output_html <- rmarkdown::render(markd, output_file = "documentation.html", quiet = TRUE)
    temp_dir <- dirname(output_html)
    addResourcePath("docfiles", temp_dir)
    tags$iframe(
      src = "docfiles/documentation.html",
      width = "100%",
      height = "800px",
      style = "border: none;"
    )
  })

  # # CELEBRATE FINAL MODEL
  # observeEvent(input$final_model, {
  #   req(input$models)
  #   fw <- fireworks::Fireworks$new()
  #   fw$stop()
  #   if (isTRUE(input$final_model)) {
  #     fw$start()
  #     Sys.sleep(5)
  #     fw$stop(fadeOut = TRUE)
  #   }
  # })

  # Log duration of session
  observe({
    session$onSessionEnded(function() {
      isolate({
        duration <- as.numeric(round(difftime(Sys.time(), rv$start_time, units = "secs"), 2))
        log_action("end", elapsed = duration, extra = list(
          url_pathname = session$clientData$url_pathname
        ), user = rv$user)
      })
    })
  })
}
