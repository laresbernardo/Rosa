# Global values
store_filename <- "Rosa.ragnar.duckdb"
store_location <- paste0("inst/extdata/", store_filename)
system_prompt <- "
You are a multi-lingual friendly question and answer analyst and assistant, expert in Marketing Mix Models and Rosa tool.
Always prioritize the knowledge provided over your own to reply, but feel free to complement.
If the question is not related to your expertise topics, reply you cannot answer the question because it is not related to MMM or Rosa.
If no relevant results are found, inform the user and do not attempt to answer the question.
If the user request is ambiguous, perform at least one search first, then ask a clarifying question.
Avoid adding code or LaTeX content in your answers.
Always reply in fully self-contained markdown format.
Every response should cite sources with section and subtitles if available.
"

# Create / recreate RAG
ragnardb_client <- function() {
  if (!file.exists(store_location)) {
    if (!dir.exists("inst/extdata/")) {
      dir.create("inst/extdata/")
    }
    store_Rosa <- ragnar_store_create(
      location = paste0("inst/extdata/", store_filename),
      name = lares::cleanText(gsub("\\.duckdb", "", store_filename)),
      overwrite = TRUE,
      embed = function(x) {
        embed_openai(
          x,
          model = "text-embedding-3-small", api_key = Sys.getenv("OPENAI_API")
        )
      }
    )
    data <- read_as_markdown("inst/www/documentation.md")

    ## Already added to documentation, no need but keeping as reference:
    # glossary <- system.file("data/glossary.RData", package = "Rosa")
    # knitr::kable(glossary$inputs, format = "markdown")
    # knitr::kable(glossary$params, format = "markdown")

    ragnar_store_insert(store_Rosa, markdown_chunk(data))
    ragnar_store_build_index(store_Rosa)

    # To close connection and remove .wal file:
    # DBI::dbDisconnect(store_Rosa@con)
  } else {
    store_Rosa <- ragnar_store_connect(store_location)
  }
  # ragnar::ragnar_store_inspect(store_Rosa)

  # Create the client/chat
  client_ragnar <- ellmer::chat_openai(
    model = "gpt-4.1",
    api_key = Sys.getenv("OPENAI_API")
  )
  ragnar_register_tool_retrieve(
    client_ragnar, store_Rosa,
    top_k = 10,
    description = "Rosa documentation"
  )
  client_ragnar$set_system_prompt(system_prompt)
  # client_ragnar$chat("share additional resources")
  client_ragnar
}

# AI Chat Assistant with RAG
gpt_rag <- function(input, output, session, rv) {
  observeEvent(input$chatme, showModal(chatModal(
    question = "gpt_question2",
    answer = "gpt_answer2",
    button = "gpt_ask2",
    title = "Chat with Rosa AI Assistant"
  )))
  observeEvent(input$gpt_ask2, {
    req(rv$rag_client)
    if (nchar(input$gpt_question2) <= 2) {
      showNotification("Please, write a meaningful message...", type = "error")
    } else {
      disable("gpt_ask2")
      tic("gpt_chat")
      message("Prompt: ", input$gpt_question2)
      showNotification("Chatting with AI Assistant...", type = "message")
      rv$rag_client$chat(input$gpt_question2, echo = "none")
      msg <- rv$rag_client$last_turn()@json$choices[[1]]$message$content
      updateTextInput(session, "gpt_question2", value = "")

      rv$chat_msg <- rv$chat_msg + 1
      msg <- sprintf("**%s**<br/>%s<br />", trimws(input$gpt_question2), msg)

      # Concatenate conversation to display
      rv$chat_replies <- paste(msg, rv$chat_replies, collapse = "<hr/>")
      output$gpt_answer2 <- renderUI(
        gsub("<br />", "<br/><hr/>", gsub("\u3010.*?\u3011", "", HTML(
          markdown::markdownToHTML(text = rv$chat_replies, template = FALSE)
        )))
      )
      log_action(
        "gpt_rag_chat",
        extra = list(
          question = input$gpt_question2,
          answer_length = nchar(msg),
          nprompt = rv$chat_msg
        ),
        elapsed = toc_secs("gpt_chat"), user = rv$user
      )
      enable("gpt_ask2")
    }
    # Update the button icon back to "play" after processing
    updateActionButton(session, "gpt_ask2", icon = icon("play"))
  })
}

# AI Assistant in Docs (using context - no chat)
gpt_contexted <- function(input, output, session, rv) {
  observeEvent(input$askme, showModal(chatModal(
    question = "gpt_question",
    answer = "gpt_answer",
    button = "gpt_ask",
    title = "Ask Rosa AI Assistant"
  )))
  observeEvent(input$gpt_ask, {
    if (nchar(input$gpt_question) <= 10) {
      showNotification("Please, write a meaningful prompt...", type = "error")
    } else {
      disable("gpt_ask")
      tic("gpt_question")
      message("OpenAI Question: ", input$gpt_question)
      showNotification("Asking AI Assistant...", type = "message")
      # https://platform.openai.com/docs/models/
      reply <- try(openai_prompt(
        user_prompt = input$gpt_question,
        system_prompt = get_ai_prompt()
      ))
      if (is.null(names(reply))) {
        reply <- list(error = list(message = "Error 404: Page Not Found"))
      }
      if ("error" %in% names(reply)) {
        msg <- reply$error$message
      } else {
        if (isTRUE("message" %in% names(reply$choices[[1]]))) {
          msg <- paste(trimws(reply$choices[[1]]$message$content), "\n")
          updateTextInput(session, "gpt_question", value = "")

          # AGENT: Last Log
          if (grepl("Agent: Fetch last log", msg)) {
            showNotification("Agent: Last Log", type = "message")
            msg <- toJSON(rv$last_log, pretty = TRUE)
            rv$last_log <- log_action(
              "gpt_agent",
              extra = list(
                agent = "last_log",
                question = input$gpt_question,
                answer_length = nchar(msg)
              ),
              elapsed = toc_secs("gpt_question"),
              user = rv$user
            )
          }

          # AGENT: Model Analyzer
          if (grepl("Agent: Selected model", msg)) {
            showNotification("Agent: Model Analyzer", type = "message", duration = 12)
            if (input$models %in% c("", "Select model")) {
              msg <- paste(
                "You must first select a model.",
                "Go to Results tab and select a model from the dropdown list before re-asking."
              )
            } else {
              temp <- robyn_write(
                rv$ro$InputCollect, rv$ro$OutputCollect,
                extract_content_within_parentheses(input$models),
                export = FALSE, quiet = TRUE,
                raw_data = "Not included",
                currency = input$currency,
                conversion_units = input$conversion_units,
                brand = input$brand,
                market = input$market,
                final_model = input$final_model,
                notes = input$add_notes,
                baseline_level = input$baseline_level,
                model_selection = extract_content_within_parentheses(input$models)
              )
              reply <- try(openai_prompt(
                user_prompt = get_ai_prompt("agent_selmod", question = input$gpt_question, json = temp),
                system_prompt = get_ai_prompt("system", agent_selmod = FALSE),
                max_tokens = 1500
              ))
              msg <- paste(trimws(reply$choices[[1]]$message$content), "\n")
              rv$last_log <- log_action(
                "gpt_agent",
                extra = list(
                  agent = "model_analyzer",
                  question = input$gpt_question,
                  answer_length = nchar(msg)
                ),
                elapsed = toc_secs("gpt_question"), user = rv$user
              )
            }
          }
        }
      }
      output$gpt_answer <- renderUI(HTML(
        markdown::markdownToHTML(text = msg, template = FALSE)
      ))
      # Log gpt_question but not include it as last_log
      log_action(
        "gpt_question",
        extra = list(question = input$gpt_question, answer_length = nchar(msg)),
        elapsed = toc_secs("gpt_question"), user = rv$user
      )
      enable("gpt_ask")
    }
    # Update the button icon back to "play" after processing
    updateActionButton(session, "gpt_ask", icon = icon("play"))
  })
}

### AI Assistant Chat Button in Header (using assistant)
gpt_assistant <- function(input, output, session, rv) {
  observeEvent(input$chatme, showModal(chatModal(
    question = "gpt_question2",
    answer = "gpt_answer2",
    button = "gpt_ask2",
    title = "Chat with Rosa AI Assistant"
  )))
  observeEvent(input$gpt_ask2, {
    if (nchar(input$gpt_question2) <= 2) {
      showNotification("Please, write a meaningful message...", type = "error")
    } else {
      disable("gpt_ask2")
      tic("gpt_chat")
      message("OpenAI Chat: ", input$gpt_question2)
      showNotification("Chatting with AI Assistant...", type = "message")
      reply <- try(openai_assistant_chat(input$gpt_question2, thread_id = rv$thread_id))
      if ("error" %in% names(reply)) {
        msg <- reply$error$message
      } else {
        rv$thread_id <- reply$thread_id
        rv$chat_msg <- rv$chat_msg + 1
        msg <- sprintf("**%s**<br/>%s<br />", trimws(input$gpt_question2), reply$reply)
      }
      updateTextInput(session, "gpt_question2", value = "")

      # AGENT: Model Analyzer
      if (grepl("Agent: Selected model", reply$reply)) {
        tic("gpt_agent")
        showNotification("Agent: Model Analyzer", type = "message", duration = 12)
        if (input$models %in% c("", "Select model")) {
          reply$reply <- paste(
            "You must first select a model.",
            "Go to Results tab and select a model from the dropdown list before re-asking."
          )
        } else {
          temp <- robyn_write(
            rv$ro$InputCollect, rv$ro$OutputCollect,
            extract_content_within_parentheses(input$models),
            export = FALSE, quiet = TRUE,
            raw_data = "Not included",
            currency = input$currency,
            conversion_units = input$conversion_units,
            brand = input$brand,
            market = input$market,
            final_model = input$final_model,
            notes = input$add_notes,
            baseline_level = input$baseline_level,
            model_selection = extract_content_within_parentheses(input$models)
          )
          reply <- try(openai_assistant_chat(
            get_ai_prompt("agent_selmod", question = input$gpt_question2, json = temp),
            thread_id = rv$thread_id
          ))
        }
        msg <- sprintf("**%s**<br/>%s<br />", trimws(input$gpt_question2), reply$reply)
        rv$last_log <- log_action(
          "gpt_agent",
          extra = list(
            agent = "model_analyzer",
            question = input$gpt_question,
            solID = input$models,
            answer_length = nchar(msg),
            nprompt = rv$chat_msg,
            thread = rv$thread_id
          ),
          elapsed = toc_secs("gpt_agent"), user = rv$user
        )
      }

      # Concatenate conversation to display
      rv$chat_replies <- paste(msg, rv$chat_replies, collapse = "<hr/>")
      output$gpt_answer2 <- renderUI(
        gsub("<br />", "<br/><hr/>", gsub("\u3010.*?\u3011", "", HTML(
          markdown::markdownToHTML(text = rv$chat_replies, template = FALSE)
        )))
      )
      log_action(
        "gpt_chat",
        extra = list(
          question = input$gpt_question2,
          answer_length = nchar(msg),
          nprompt = rv$chat_msg,
          thread = rv$thread_id
        ),
        elapsed = toc_secs("gpt_chat"), user = rv$user
      )
      enable("gpt_ask2")
    }
    # Update the button icon back to "play" after processing
    updateActionButton(session, "gpt_ask2", icon = icon("play"))
  })
}
