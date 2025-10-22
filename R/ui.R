# Global variables
default_path <- getwd()
use_cache <- TRUE
create_files <- TRUE
enable_gpt <- TRUE
enable_dataset_ai <- TRUE
session_id <- as.integer(Sys.time())

### Dataset #########################################################################################
dat <- function() {
  fluidPage(
    fluidRow(
      hr(),
      column(4, align = "left", h2(id = "h_d", "Data Inputs"), tags$style(HTML("#h_d{color:#F36633}"))),
      column(8, align = "right", div(
        style = "display: flex; justify-content: flex-end;",
        uiOutput("dat_next_button")
      )),
      useShinyjs(),
      use_hover(),
      textOutput("debug")
    ),
    fluidRow(
      column(12, shiny::wellPanel(
        align = "center",
        shinyWidgets::radioGroupButtons(
          inputId = "exec_type",
          label = "What type of execution do you want to carry out?",
          choices = c("Build models" = 1, "Recreate single model" = 3, "Refresh model (new data)" = 2, "Compare multiple models" = 4),
          selected = 1,
          justified = TRUE,
          checkIcon = list(yes = icon("ok", lib = "glyphicon"))
        ),
        uiOutput("exec_type_txt")
      ))
    ),
    uiOutput("dat_json_upload"),
    uiOutput("dat_data_upload"),
    uiOutput("dat_dashboard")
  )
}

### Configuration #########################################################################################
con <- function() {
  fluidPage(
    hr(),
    fluidRow(
      column(align = "left", 1, h2(id = "h_c", "Configuration"), tags$style(HTML("#h_c{color:#F36633}"))),
      use_hover(),
      column(
        11,
        style = "display: flex; justify-content: flex-end;",
        create_hover_action_button("con_back", "left", "Dataset", "backward", "backward", style = "margin-top:21px"),
        create_hover_action_button("con_next", "right", "Execution", "forward", "forward", "sweep-to-right", style = "margin-top:20px;font-weight:bold;border-color:#F36633;text-padding:-2px;text-align:center;border-radius:5px;border-width:2px; margin-left: 5px;")
      )
    ),
    fluidRow(column(12, actionButton("glossary", " Read more about inputs and parameters", icon = icon("question"), width = "100%"))),
    tags$br(),
    fluidRow(
      conditionalPanel(
        condition = "input.exec_type == 1",
        column(
          3, shiny::wellPanel(
            h3(strong("Local Settings")),
            tags$hr(),
            useShinyjs(),
            use_hover(),
            tags$div(
              fileInput("CF2", "Load configuration file (.json)", multiple = FALSE, accept = c(".json")),
              id = "CF2_tag"
            ),
            helpText("OR paste JSON formatted configurations below..."),
            textInput("json_conf", label = NULL, width = "100%"),
            actionButton("json_conf_process", label = "Process", icon = icon("gear"), width = "100%"),
            tags$hr(),
            uiOutput("outputs_dir")
          )
        )
      ),
      conditionalPanel(
        condition = "input.exec_type == 1",
        column(4, shiny::wellPanel(
          h3(strong("Input Variables Mapping")),
          tags$hr(),
          selectInput("date", "Date column (yyyy-mm-dd):", ""),
          selectInput("sales", "Dependent variable:", ""),
          radioButtons("dep_var_type", "Dependent variable type:", c(
            "Revenue (currency)" = "revenue",
            "Conversion (units)" = "conversion"
          ),
          inline = TRUE
          ),
          uiOutput("conversion_units"),
          selectInput("currency", "Promotional Spends Currency:",
            choices = sort(c("USD", "EUR", "GBP", "BRL", "JPY", "")),
            selected = "EUR", width = "100%"
          ),
          plotOutput("date_sales_plot", height = "200px"),
          tags$hr(),
          tags$div(
            selectizeInput("paid_media_spends", h4(strong("Paid channels spends [required]:")), choices = NULL, selected = NULL, multiple = TRUE),
            id = "pm_s_s"
          ),
          tableOutput("paid_media_spends_summary"),
          helpText("Note: keep same order for paid and exposure metric variables below. Leave empty to skip this step. If no exposure metric related to a specific paid variables, input its paid channel spend variable."),
          tags$div(
            selectizeInput("paid_media_vars", "Paid channels exposure metrics:", choices = NULL, selected = NULL, multiple = TRUE),
            id = "pm_v_s"
          ),
          tags$hr(),
          tags$div(
            selectizeInput("organic_vars", h4(strong("Organic variables:")), choices = NULL, selected = NULL, multiple = TRUE),
            id = "organic_vars_s"
          ),
          helpText("Note: keep the same order between vars and signs"),
          tags$div(
            selectizeInput("organic_signs", "Organic signs:", choices = c("default" = "1d", "positive" = "1p", "negative" = "1n"), selected = NULL, multiple = TRUE, options = list(create = TRUE)),
            id = "organic_signs_s"
          ),
          tags$hr(),
          tags$div(
            selectizeInput("context_vars", h4(strong("Context variables:")), choices = NULL, selected = NULL, multiple = TRUE),
            id = "context_vars_s"
          ),
          helpText("Note: keep the same order between vars and signs"),
          tags$div(
            selectizeInput("context_signs", "Context signs:", choices = c("default" = "1d", "positive" = "1p", "negative" = "1n"), selected = NULL, multiple = TRUE, options = list(create = TRUE)),
            id = "context_signs_s"
          ),
          tags$hr(),
          h4(strong("Other optional mappings:")),
          selectizeInput("factor_vars", "Categorical variables:", choices = NULL, selected = NULL, multiple = TRUE),
          selectizeInput("offline_vars", "Offline variables:", choices = "", width = "100%", multiple = TRUE)
        ))
      ),
      column(5, shiny::wellPanel(
        conditionalPanel(
          condition = "['1', '2'].includes(input.exec_type)",
          h3(strong("Modeling Settings")),
          tags$hr(),
          tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))),
          splitLayout(
            numericInput("iters", "Iterations:", 4000, min = 500, max = 8000, step = 250),
            numericInput("trials", "Trials:", 3, min = 1, max = 10),
            numericInput("cores", "Cores:", min(c(3, parallel::detectCores() - 1)),
              min = 1, max = 10, step = 1, width = "100%"
            )
          ),
          conditionalPanel(
            condition = "input.exec_type == 2",
            numericInput("steps", "Refresh steps:", 4, min = 1, max = 100)
          )
        ),
        conditionalPanel(
          condition = "['1'].includes(input.exec_type)",
          h4(strong("Basic Settings:")),
          conditionalPanel(
            condition = "input.exec_type == 1",
            sliderInput("window", "Modeling Window*:",
              min = as.Date("2000-01-01", "%Y-%m-%d"),
              max = as.Date("2099-12-01", "%Y-%m-%d"),
              value = c(as.Date("2024-01-01"), as.Date("2024-12-31")),
              step = 1
            ),
            conditionalPanel(
              condition = "input.exec_type == 1",
              tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))),
              splitLayout(
                checkboxGroupInput(
                  "prophet_vars", "Time series decomposition:",
                  choices = c(
                    "Trend" = "trend",
                    "Season" = "season",
                    "Holiday" = "holiday",
                    "Monthly" = "monthly",
                    "Weekday" = "weekday"
                  ),
                  selected = c("trend", "season", "holiday", "monthly")
                ),
                column(
                  width = 12,
                  tags$div(
                    selectInput("holidays_country", "Holidays country:", "ES"),
                    id = "country_s"
                  ),
                  helpText(tags$a(
                    href = "https://raw.githubusercontent.com/facebook/prophet/refs/heads/main/R/data-raw/generated_holidays.csv",
                    "(Holidays source)",
                    target = "_blank"
                  )),
                  tags$div(
                    selectInput("prophet_signs", "Prophet Signs:", choices = c("default", "positive", "negative"), selected = "default"),
                    id = "prophet_signs_s"
                  )
                ),
                cellWidths = c("65%", "35%")
              )
            )
          ),
          tags$hr(),
          h4(strong("Advanced Settings:")),
          splitLayout(
            checkboxInput("ts_val", "Time Series Validation", FALSE),
            checkboxInput("add_pen", "Use Penalty Factor", FALSE),
            checkboxInput("int_use", "Add Intercept", TRUE)
          ),
          conditionalPanel(
            condition = "input.ts_val",
            sliderInput("train_size", "Train size:", min = 0.5, max = 1, value = c(0.5, 0.8), step = 0.05)
          ),
          conditionalPanel(
            condition = "input.int_use",
            tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))),
            selectInput("int_sign", "Intercept sign:", c("non_negative", "unconstrained"), "non_negative")
          ),
          numericInput("seed", "Randomness Seed", 123, min = 0, step = 1),
          tags$hr(),
          tags$div(
            selectInput("adstock", "Adstock:", c("geometric", "weibull_cdf", "weibull_pdf"), "weibull_pdf"),
            id = "adstock_s"
          ),
          helpText("Keep in mind values will be overwritten if defined channels change"),
          actionButton("curves_help", "More about hyperparameters & curves", width = "100%"),
          tags$hr(),
          splitLayout(
            uiOutput("hyper_ch_a"),
            uiOutput("hyper_ch_g")
          ),
          conditionalPanel(
            condition = "input.exec_type == 1 && input.adstock == 'geometric'",
            helpText("Channels hyperparameters for Geometric adstock:"),
            splitLayout(
              uiOutput("hyper_ch_t")
            )
          ),
          conditionalPanel(
            condition = "input.exec_type == 1 && input.adstock != 'geometric'",
            helpText("Channels hyperparameters for Weibull adstock:"),
            splitLayout(
              uiOutput("hyper_ch_sh"),
              uiOutput("hyper_ch_sc")
            )
          )
        )
      ))
    )
  )
}

### Execution #########################################################################################
run <- function() {
  fluidPage(
    fluidRow(
      hr(),
      column(align = "left", 1, h2(id = "h_e", "Execution"), tags$style(HTML("#h_e{color:#F36633}"))),
      use_hover(),
      column(
        11,
        style = "display: flex; justify-content: flex-end;",
        create_hover_action_button("run_back", "left", "Configuration", "backward", "backward", style = "margin-top:21px"),
        create_hover_action_button("run_back3", "left", "Dataset", "backward", "backward", style = "margin-top:21px"),
        create_hover_action_button("run_next", "right", "Results", "forward", "forward", "sweep-to-right", style = "margin-top:20px;border-color: #F36633; text-padding: -2px; text-align:center; border-radius: 5px; border-width: 2px; margin-left: 5px;")
      )
    ),
    column(
      width = 4,
      shiny::wellPanel(
        useShinyjs(),
        use_hover(),
        splitLayout(
          hover_action_button("run_robyn", "Run Rosa, Run!",
            icon = icon("play"), button_animation = "bounce-in", icon_animation = "grow",
            style = "color: white; font-weight: bold; background-color: #3BB143; border-color: #A9A9A9; position: relative; height: 36px;
                       text-padding: -2px; text-align:center; border-radius: 5px; border-width: 2px; width: 100%"
          )
        ),
        br(),
        downloadButton("save_inputs", "Download configurations file", icon = icon("save"), style = "width:100%;"),
        tags$hr(),
        helpText("Execution Summary (please double check before running Rosa):"),
        div(class = "sidebar-table", tableOutput("summary")),
        tags$hr(),
        if (use_cache) {
          list(
            splitLayout(
              checkboxInput("use_cache", "Use cache", value = FALSE, width = "100%"),
              checkboxInput("rewrite_cache", "Overwrite cache", value = FALSE, width = "100%")
            ),
            actionButton("reset_cache", "Reset cache", width = "100%")
          )
        } else {
          NULL
        },
      )
    ),
    column(
      width = 8,
      splitLayout(
        h3(strong("Console logs:")),
        list(
          br(),
          div(
            style = "float: right;",
            shinyWidgets::materialSwitch(
              inputId = "live_logs",
              label = "Live logs",
              value = TRUE,
              status = "info",
              inline = TRUE
            )
          )
        )
      ),
      shiny::wellPanel(htmlOutput("console")),
      plotOutput("moo_points", height = "600px")
    )
  )
}

### Results #########################################################################################
res <- function() {
  fluidPage(
    fluidRow(
      hr(),
      column(align = "left", 1, h2(id = "h_r", "Results"), tags$style(HTML("#h_r{color:#F36633}"))),
      use_hover(),
      column(
        11,
        style = "display: flex; justify-content: flex-end;",
        create_hover_action_button("res_back", "left", "Execution", "backward", "backward", style = "margin-top:21px; margin-right: 5px;"),
        create_hover_action_button("res_back4", "left", "Dataset", "backward", "backward", style = "margin-top:21px; margin-right: 5px;")
      )
    ),
    sidebarLayout(
      sidebarPanel(
        useShinyjs(),
        use_hover(),
        tags$div(
          selectInput("models", "Select a model:", NULL, width = "100%"),
          id = "modelss"
        ),
        selectInput("baseline_level", "Agreggate baseline variables", width = "100%", selected = 0, choices = c(
          "No aggregation" = 0,
          "L1: Intercept" = 1,
          "L2: & Trend" = 2,
          "L3: & Time Series Decomposition" = 3,
          "L4: & Contextual variables" = 4,
          "L5: & Organic variables" = 5
        )),
        tags$hr(),
        h4(strong("Save Model Results")),
        splitLayout(
          uiOutput("brand"),
          # selectInput("brand", "Brand", "", width = "100%"), # dynamic in case no brands.txt file
          selectInput("market", "Market", c("", unique(Robyn::dt_prophet_holidays$country)), width = "100%")
        ),
        helpText("Add information about your model before exporting it (footnotes, alerts, caveats, decisions, assumptions, spent excluded, weighted calcs, etc.)"),
        textInput("add_notes", "Include relevant comments", value = "", width = "100%"),
        uiOutput("download_options"),
        uiOutput("download_model"),
        uiOutput("build_report"),
        tags$hr(),
        htmlOutput("dash_txt")
      ),
      mainPanel(
        tabsetPanel(
          id = "results_tabs",
          tabPanel(
            "Select Models",
            helpText(HTML(paste(
              "Use the following metrics as criteria to find the best models to select and evaluate based on your needs.",
              "Read more about it",
              tags$a("here.", target = "_blank", href = "https://bit.ly/mmm-candidates")
            ))),
            fluidRow(
              column(
                width = 6,
                sliderInput("slider_1", "Model fit (R^2)", width = "100%", min = 0, max = 100, value = 75),
                uiOutput("slider_2"),
                splitLayout(
                  sliderInput("slider_6", div("Distance to Baseline ", icon("arrow-right")), width = "100%", min = 0, max = 100, value = 0),
                  sliderInput("slider_7", div("L4 Baseline [%]", style = "color: #808080;"), width = "100%", min = 0, max = 100, value = 0),
                  cellWidths = c("65%", "35%")
                ),
                sliderInput("slider_3", "Business Error (DECOMP.RSSD)", width = "100%", min = 0, max = 100, value = 0)
              ),
              column(
                width = 6,
                sliderInput("slider_4", "Non-Zero Promotional Betas", width = "100%", min = 0, max = 100, value = 30),
                sliderInput("slider_5", "Models in Cluster", width = "100%", min = 0, max = 100, value = 10),
                sliderInput("slider_8", "Certainty within Cluster", width = "100%", min = 0, max = 100, value = 60),
                sliderInput("slider_9", "Cluster Mean Std Dev (Experimental)", width = "100%", min = 0, max = 100, value = 0)
              ),
              column(width = 12, shiny::plotOutput("modelselector", height = "900px"))
            )
          ),
          tabPanel(
            "Dashboard",
            uiOutput("rep_boxes"),
            tags$br(),
            h4(strong("Models")),
            fluidRow(
              column(
                width = 4,
                pickerInput(
                  inputId = "dash_models",
                  label = "Selected models",
                  choices = "",
                  options = list(`actions-box` = TRUE),
                  multiple = TRUE,
                  width = "100%"
                )
              ),
              column(
                width = 4,
                pickerInput(
                  inputId = "dash_markets",
                  label = "Selected markets",
                  choices = "",
                  options = list(`actions-box` = TRUE),
                  multiple = TRUE,
                  width = "100%"
                )
              ),
              column(
                width = 4,
                pickerInput(
                  inputId = "dash_brands",
                  label = "Selected brands",
                  choices = "",
                  options = list(`actions-box` = TRUE),
                  multiple = TRUE,
                  width = "100%"
                )
              )
            ),
            tags$br(),
            h4(strong("Model Information")),
            gt::gt_output("report_summary"),
            tags$hr(),
            h4(strong("Channels Performance")),
            fluidRow(
              column(width = 5, dateRangeInput(
                "pw_date_range", "Data date range to report",
                format = "yyyy-mm-dd",
                startview = "year", weekstart = 1, language = "en",
                separator = " to ", width = "100%"
              )),
              column(width = 4, pickerInput(
                inputId = "pw_options",
                label = "Include/exclude columns",
                choices = c("Date Range", "Market", "Brand", "Variable Type", "Metric", "Carryover Response", "Marginal Performance", "Non-Promo"),
                selected = c("Date Range", "Variable Type"),
                options = list(`actions-box` = TRUE),
                multiple = TRUE,
                width = "100%"
              )),
              column(width = 3, br(), div(
                style = "display:inline-block; float:center",
                downloadButton("download_perf", "Download CSV")
              ))
            ),
            gt::gt_output("report_perfo"),
            tags$hr(),
            h4(strong("Model Errors")),
            gt::gt_output("report_errors")
          ),
          tabPanel(
            "One-Pagers",
            helpText("Carefully inspect each of these plots to understand how the selected model is interpreting the business. Do they make sense?"),
            dateRangeInput("op_date", "Reporting date range (aggregates non-promotional channels as baseline)", width = "100%"),
            shiny::uiOutput("one_pager")
          ),
          tabPanel(
            "Budget Allocator",
            helpText(HTML(paste(
              "Evaluate different scenarios using the budget allocator based on the selected model and date range used to calculate mean spends and carryovers.",
              "Read more about it",
              tags$a("here", target = "_blank", href = "https://bit.ly/robyn-ba1"),
              "and",
              tags$a("here.", target = "_blank", href = "https://bit.ly/robyn-ba1")
            ))),
            fluidRow(
              column(
                width = 6,
                dateRangeInput("date_range", "Date range to consider:", format = "yyyy-mm-dd", startview = "year", weekstart = 1, language = "en", separator = " to ", width = "100%")
              ),
              column(
                width = 6,
                tags$br(),
                hover_action_button(
                  "sce_robyn", "Calculate both scenarios",
                  icon = icon("file-text"), button_animation = "rectangle-out", icon_animation = "bounce",
                  style = "border-color: #F36633; text-padding: -2px; text-align:center; border-radius: 5px; border-width: 2px; width: 100%"
                )
              )
            ),
            tags$hr(),
            tabsetPanel(
              id = "budall_tabs",
              tabPanel(
                "Maximum Response",
                tags$br(),
                fluidRow(
                  column(
                    width = 6,
                    autonumericInput("budget", "Total budget (based on date range by default):", 0, minimumValue = 0, maximumValue = 100000000, step = 10000, width = "100%", digitGroupSeparator = ",", decimalPlaces = 0, align = "left")
                  ),
                  column(
                    width = 6,
                    numericInput("ch_c", "Constraint multiplier:", 3, min = 1, max = 5, width = "100%")
                  ),
                  column(
                    width = 12,
                    helpText("Set minimum and maximum total budget per channel we want the optimizer to be allowed for the date range defined."),
                    column(width = 12, uiOutput("ch_low_up")),
                    tags$hr(),
                    uiOutput("mr")
                  )
                )
              ),
              tabPanel(
                "Target Efficiency",
                tags$br(),
                column(
                  width = 12,
                  splitLayout(
                    cellWidths = c("48%", "4%", "48%"),
                    autonumericInput("target", "ROAS/CPA target:", minimumValue = 0, value = 1.7, width = "100%", digitGroupSeparator = ",", decimalPlaces = 1, align = "left"),
                    helpText(" "),
                    autonumericInput("target_upper", "Channels upper constraint:", 10, minimumValue = 1, width = "100%", align = "left", decimalPlaces = 0)
                  ),
                  splitLayout(
                    shinydashboard::valueBoxOutput("target_boxes1"),
                    shinydashboard::valueBoxOutput("target_boxes2"),
                    shinydashboard::valueBoxOutput("target_boxes3")
                  ),
                  tags$hr(),
                  uiOutput("te")
                )
              )
            )
          ),
          tabPanel(
            "Others",
            uiOutput("otherplotsopts"),
            uiOutput("selected_json_txt")
          )
        )
      )
    )
  )
}

### Documentation #########################################################################################

doc <- function() {
  fluidPage(
    fluidRow(
      hr(),
      column(6, align = "left", h2(id = "h_doc", "Documentation"), tags$style(HTML("#h_doc{color:#F36633}"))),
      column(6, br(), fluidRow(
        uiOutput("show_backlog", style = "display: inline-block; margin-left: 15px; float:right"),
        uiOutput("askme", style = "display: inline-block; float:right")
      )),
      column(
        12,
        helpText(paste("Last updated:", as.Date(file.info(system.file("www/documentation.md", package = "Rosa"))$atime))),
        uiOutput("rendered_docs")
      )
    )
  )
}

### TAB MENU #########################################################################################
company_logo <- ""
logo_Rosa_path <- "www/RosaLogo.png"
favicon <- "www/favicon.ico"
ui <- function() {
  bootstrapPage(
    navbarPage(
      title = div(
        id = "title",
        div(
          style = "display: flex; align-items: center; height: 20px;",
          tags$img(src = logo_Rosa_path, height = "35px", style = "margin-right: 10px;"),
          "Rosa"
        ),
        div(
          style = "position:fixed; right:15px; top:11px",
          uiOutput("chatme", style = "display:inline-block; margin-right:10px;"),
          shiny::img(src = company_logo, height = "30px")
        ),
      ),
      id = "tabs",
      fluid = TRUE,
      inverse = TRUE,
      position = "fixed-top",
      tabPanel("Dataset", dat()),
      tabPanel("Configuration", con()),
      tabPanel("Execution", run()),
      tabPanel("Results", res()),
      tabPanel("Documentation", doc()),
      selected = "Dataset"
    ),
    header = tags$head(
      tags$link(rel = "shortcut icon", href = favicon)
    ),
    shinyWidgets::useSweetAlert(),
    tags$script(HTML("
      $(document).on('click', '#gpt_ask, #gpt_ask2', function() {
        $(this).find('i').attr('class', 'fa fa-refresh fa-spin');
      });
    ")),
    footer = tags$footer(
      paste0(
        "Rosa v", as.character(packageVersion("Rosa")), " & Robyn v", as.character(packageVersion("Robyn")),
        ifelse(is_server(), paste0(" @ ", system("hostname", intern = TRUE)), ""),
        " | Session: ", session_id
      ),
      align = "center",
      style = "width:100%; color:black; position:fixed; bottom:0; right:0; z-index:100; background-color:#f2f2ed; padding: 5px;"
    ),
    tags$style(HTML("body {margin-bottom: 60px;}
      .navbar-default .navbar-brand:hover {color:#F36633}
      .navbar {background-color:#282828}
      .navbar-default .navbar-nav > li > a {color:#D9DDDC}
      .navbar-default .navbar-nav > .active > a,
      .navbar-default .navbar-nav > .active > a:focus,
      .navbar-default .navbar-nav > .active > a:hover {color:#282828;font-weight:bold;background-color:#F36633}
      .navbar-default .navbar-nav > li > a:hover {color:#282828;background-color:#D9DDDC;text-decoration}
      .shiny-notification { font-weight:bold }
      .datepicker {z-index:99999 !important;}
      .bootstrap-switch .bootstrap-switch-handle-off.bootstrap-switch-oni,
      .bootstrap-switch .bootstrap-switch-handle-on.bootstrap-switch-oni { color:#006400 }
      .bootstrap-switch .bootstrap-switch-handle-off.bootstrap-switch-ofi,
      .bootstrap-switch .bootstrap-switch-handle-on.bootstrap-switch-ofi { color:#D22B2B }
      .sidebar-table {max-width: 100%;overflow-x: auto;}
      .sidebar-table table {width: 100% !important;table-layout: fixed;word-wrap: break-word;}
    "))
  )
}
