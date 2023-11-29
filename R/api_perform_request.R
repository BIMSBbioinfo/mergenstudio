#' Perform API Request
#'
#' This function provides a generic interface for calling different APIs
#' (e.g., OpenAI, PALM (MakerSuite)). It dispatches the actual API
#' calls to the relevant method based on the `class` of the `skeleton` argument.
#'
#' @param skeleton A `mergenstudio_request_skeleton` object
#' @param ... Extra arguments (e.g., `stream_handler`)
#'
#' @return A `mergenstudio_response_skeleton` object
#'
#' @examples
#' \dontrun{
#' mergenstudio_request_perform(mergenstudio_skeleton)
#' }
#' @export
mergenstudio_request_perform <- function(skeleton, ...) {
  if (!inherits(skeleton, "mergenstudio_request_skeleton")) {
    cli::cli_abort("Skeleton must be a 'mergenstudio_request_skeleton' or a child class")
  }
  UseMethod("mergenstudio_request_perform")
}

#' @export
mergenstudio_request_perform.mergenstudio_request_openai <- function(skeleton, shinySession = NULL, ...) {

  url        <- skeleton$url
  api_key    <- skeleton$api_key
  prompt     <- skeleton$prompt
  history    <- skeleton$history
  stream     <- skeleton$stream
  model      <- skeleton$model
  max_tokens <- skeleton$extras$max_tokens
  n          <- skeleton$extra$n

  # Translate request
  messages <- c(
    skeleton$history,
    list(
      list(role = "user", content = skeleton$prompt)
    )
  )

  body <- list(
    "model"      = model,
    "stream"     = stream,
    "messages"   = messages,
    "max_tokens" = max_tokens,
    "n"          = n
  )

  # Perform request
  response <- NULL

  if (isTRUE(skeleton$stream)) {
    if (is.null(shinySession)) stop("Stream requires a shiny session object")

    stream_handler <- StreamHandler$new(
      session = shinySession,
      user_prompt = skeleton$prompt
    )

    # stream_chat_completion(
    #   prompt = skeleton$prompt,
    #   history = skeleton$history,
    #   element_callback = stream_handler$handle_streamed_element
    # )
    # print(response)

    # response <- stream_handler$current_value
    # if(response == ""){
    #   response <- stream_handler$chunks[[1]]$error$message
    # }

    # merge query
    # agent <- mergen::setupAgent(name="openai", type="chat", model = "gpt-4", ai_api_key = Sys.getenv("OPENAI_API_KEY"))
    agent <- mergen::setupAgent(name="openai", type="chat", model = "gpt-4", ai_api_key = api_key)
    response <- mergen::sendPrompt(agent, prompt = skeleton$prompt, return.type = "text")

  } else {
    response <- httr2::request(url) %>%
      httr2::req_auth_bearer_token(api_key) %>%
      httr2::req_body_json(body) %>%
      httr2::req_perform() %>%
      httr2::resp_body_json()
  }
  # return value
  structure(
    list(
      skeleton = skeleton,
      response = response
    ),
    class = "mergenstudio_response_openai"
  )
}

#' @export
mergenstudio_request_perform.mergenstudio_request_replicate <- function(skeleton, shinySession = NULL, ...) {

  url        <- skeleton$url
  api_key    <- skeleton$api_key
  prompt     <- skeleton$prompt
  history    <- skeleton$history
  stream     <- skeleton$stream
  model      <- skeleton$model
  max_tokens <- skeleton$extras$max_tokens
  n          <- skeleton$extra$n

  # Translate request

  messages <- c(
    skeleton$history,
    list(
      list(role = "user", content = skeleton$prompt)
    )
  )

  body <- list(
    "model"      = model,
    "stream"     = stream,
    "messages"   = messages,
    "max_tokens" = max_tokens,
    "n"          = n
  )

  # Perform request
  response <- NULL

  if (isTRUE(skeleton$stream)) {
    if (is.null(shinySession)) stop("Stream requires a shiny session object")

    stream_handler <- StreamHandler$new(
      session = shinySession,
      user_prompt = skeleton$prompt
    )

    stream_chat_completion(
      prompt = skeleton$prompt,
      history = skeleton$history,
      element_callback = stream_handler$handle_streamed_element
    )

    response <- stream_handler$current_value
    if(response == ""){
      response <- stream_handler$chunks[[1]]$error$message
    }
  } else {
    response <- httr2::request(url) %>%
      httr2::req_auth_bearer_token(api_key) %>%
      httr2::req_body_json(body) %>%
      httr2::req_perform() %>%
      httr2::resp_body_json()
  }
  # return value
  structure(
    list(
      skeleton = skeleton,
      response = response
    ),
    class = "mergenstudio_response_replicate"
  )
}


#' #' @export
#' mergenstudio_request_perform.mergenstudio_request_palm <-
#'   function(skeleton, ...) {
#'     response <- create_completion_palm(prompt = skeleton$prompt)
#'     structure(
#'       list(
#'         skeleton = skeleton,
#'         response = response
#'       ),
#'       class = "mergenstudio_response_palm"
#'     )
#'   }
#'
#' #' @export
#' mergenstudio_request_perform.mergenstudio_request_anthropic <-
#'   function(skeleton, ...) {
#'     model   <- skeleton$model
#'     prompt  <- skeleton$prompt
#'     history <- skeleton$history
#'     cli_inform(c("i" = "Using Anthropic API with {model} model"))
#'     response <- create_completion_anthropic(prompt  = prompt,
#'                                             history = history,
#'                                             model   = model)
#'     structure(
#'       list(
#'         skeleton = skeleton,
#'         response = response
#'       ),
#'       class = "mergenstudio_response_anthropic"
#'     )
#'   }
#'
#' #' @export
#' mergenstudio_request_perform.mergenstudio_request_azure_openai <- function(skeleton, ...) {
#'
#'   messages <- c(
#'     skeleton$history,
#'     list(
#'       list(role = "user", content = skeleton$prompt)
#'     )
#'   )
#'
#'   body <- list("messages"   = messages)
#'
#'   cat_print(body)
#'
#'   response <- create_completion_azure_openai(prompt = body)
#'   structure(
#'     list(
#'       skeleton = skeleton,
#'       response = response
#'     ),
#'     class = "mergenstudio_response_azure_openai"
#'   )
#' }


#' @export
mergenstudio_request_perform.default <- function(skeleton, ...) {
  cli_abort(
    c("x" = "This API service is not implemented or is missing.",
      "i" = "Class attribute for `prompt`: {class(prompt)}")
  )
}
