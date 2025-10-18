openai_prompt <- function(
    user_prompt,
    system_prompt = "",
    temperature = 0.7,
    top_p = 0.95,
    max_tokens = 900,
    api_base = Sys.getenv("OPENAI_BASE"),
    deployment = Sys.getenv("OPENAI_MODEL"),
    api_version = Sys.getenv("OPENAI_VERSION"),
    api_key = Sys.getenv("OPENAI_API"),
    quiet = FALSE,
    ...) {
  # Define the API endpoint
  model <- ifelse(deployment == "", "gpt-4o-latest", Sys.getenv("OPENAI_MODEL"))
  api_version <- ifelse(deployment == "", "2024-02-15-preview", Sys.getenv("OPENAI_VERSION"))
  api_endpoint <- fix_double_slash(paste0(
    api_base,
    ifelse(grepl("azure.com", api_base), "/openai/deployments/", "/"),
    ifelse(grepl("azure.com", api_base), model, "/v1/"),
    "/chat/completions?api-version=",
    api_version
  ))
  # Define the payload structure
  payload <- list(
    messages = list(
      list(
        role = "system",
        content = list(
          list(
            type = "text",
            text = system_prompt
          )
        )
      ),
      list(
        role = "user",
        content = list(
          list(
            type = "text",
            text = user_prompt
          )
        )
      )
    ),
    model = model,
    temperature = temperature,
    top_p = top_p,
    max_tokens = max_tokens
  )
  payload_json <- toJSON(payload, auto_unbox = TRUE)

  # Make the API request using the API key (approach depends on api_base URL)
  if (grepl("azure.com", api_base)) {
    res <- POST(
      api_endpoint,
      add_headers(`Content-Type` = "application/json", `api-key` = api_key),
      body = payload_json
    )
  } else {
    res <- POST(
      api_endpoint,
      add_headers(Authorization = paste("Bearer", api_key)),
      httr::content_type_json(),
      encode = "json",
      body = payload_json
    )
  }

  return(content(res))
}

get_ai_context <- function(context_file = system.file("www/context.RDS", package = "Rosa")) {
  if (file.exists(context_file)) {
    paste("CONTEXT:", readRDS(context_file))
  } else {
    "CONTEXT: All MMM knowledge you may have. You don't know about Rosa tool."
  }
}

get_ai_prompt <- function(
    type = "system",
    context = get_ai_context(),
    agent_selmod = TRUE,
    question = NULL,
    json = NULL) {
  if (type == "system") {
    agent_selmod_txt <- paste(
      "If user asks you something about the selected current model, simply answer:",
      "'Agent: Selected model'."
    )
    prompt <- lares::glued("
    You are a multi-lingual question and answer analyst and assistant,
    expert in Finance, Pharma Industry, Marketing Mix Models and Rosa tool.
    Always prioritize the information given in CONTEXT over your own knowledge to reply.
    If the question is not related to your expertise topics, reply you cannot answer the question
    because it is not related to MMM or Rosa.
    No need to share any code in your answers.
    If user asks for the last log, simply answer: 'Agent: Fetch last log'.
    {ifelse(agent_selmod, agent_selmod_txt, '')}
    CONTEXT: {context}")
  }
  if (type == "agent_selmod") {
    prompt <- paste(
      "The following are my inputs and outputs of the selected model.",
      "Please, provide an overview of the inputs and results.",
      "Also provide insights and recommendations based on the model,",
      "highlighting and explaining most relevant MMM findings.",
      "Do not use coefficient values to infer any type of conclusion.",
      "Be sure to always provide the name of the model, brand, and market if available.",
      question,
      jsonlite::toJSON(json)
    )
  }
  if (type == "agent_pptx") {
    prompt <- paste(
      "The following are my inputs and outputs of a selected MMM for my brand.",
      "Please, provide the top 3 insights and recommendations based on the model's results,",
      "highlighting and explaining most relevant MMM findings, in order of relevance.",
      "Do not provide a title to the results and round large numbers to avoid decimals.",
      "Do not use coefficient values to infer any type of conclusion.",
      jsonlite::toJSON(json)
    )
  }
  return(prompt)
}

# Assistant: https://shorturl.at/SbHSJ
openai_assistant_chat <- function(
    prompt,
    thread_id = NULL,
    assistant_id = Sys.getenv("OPENAI_ASSISTANT"),
    endpoint = Sys.getenv("OPENAI_BASE"),
    api_version = Sys.getenv("OPENAI_VERSION"),
    api_key = Sys.getenv("OPENAI_API")) {
  api_version <- ifelse(api_version == "", "2024-05-13-preview", Sys.getenv("OPENAI_VERSION"))
  # Create a thread if not provided (run once per session)
  if (is.null(thread_id)) {
    response <- POST(
      url = paste0(endpoint, "/openai/threads?api-version=", api_version),
      add_headers(
        `api-key` = api_key,
        `Content-Type` = "application/json"
      ),
      body = ""
    )
    thread_id <- content(response)$id
    if (is.null(thread_id)) {
      stop("API error: wasn't able to create a thread. Check API Version.")
    } else {
      message("Open AI Thread Generated: ", thread_id)
    }
  }

  # Add user question to the thread
  response <- POST(
    url = paste0(endpoint, "/openai/threads/", thread_id, "/messages?api-version=", api_version),
    add_headers(
      `api-key` = api_key,
      `Content-Type` = "application/json"
    ),
    body = toJSON(list(role = "user", content = prompt), auto_unbox = TRUE)
  )

  # Run the thread
  response <- POST(
    url = paste0(endpoint, "/openai/threads/", thread_id, "/runs?api-version=", api_version),
    add_headers(
      `api-key` = api_key,
      `Content-Type` = "application/json"
    ),
    body = toJSON(list(assistant_id = assistant_id), auto_unbox = TRUE)
  )
  run_id <- content(response)$id

  # Get the status of the run
  st <- list(status = "starting", thread_id = thread_id)
  while (st$status %in% c("starting", "in_progress", "queued")) {
    Sys.sleep(1)
    response <- GET(
      url = paste0(endpoint, "/openai/threads/", thread_id, "/runs/", run_id, "?api-version=", api_version),
      add_headers(
        `api-key` = api_key
      )
    )
    st <- content(response)
  }

  if (st$status == "completed") {
    # See the Assistant response
    response <- GET(
      url = paste0(endpoint, "/openai/threads/", thread_id, "/messages?api-version=", api_version),
      add_headers(
        `api-key` = api_key,
        `Content-Type` = "application/json"
      )
    )
    assistant_response <- content(response)
    reply <- assistant_response$data[[1]]$content[[1]]$text$value
    invisible(return(list(
      thread_id = thread_id,
      reply = reply
    )))
  } else {
    invisible(return(st))
  }
}

# if (FALSE) {
#   this <- openai_assistant_chat("How can I interpret the baseline?")
#   cat(this$reply)
#   this <- openai_assistant_chat("Is it the same as carryover?", thread_id = this$thread_id)
#   cat(this$reply)
# }
