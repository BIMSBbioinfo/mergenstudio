#' Base for a request to the OPENAI API
#'
#' This function sends a request to a specific OpenAI API \code{task} endpoint at the base URL \code{https://api.openai.com/v1}, and authenticates with an API key using a Bearer token.
#'
#' @param task character string specifying an OpenAI API endpoint task
#' @param token String containing an OpenAI API key. Defaults to the OPENAI_API_KEY environmental variable if not specified.
#' @return An httr2 request object
request_base <- function(task, token = Sys.getenv("OPENAI_API_KEY")) {
  if (!task %in% get_available_endpoints()) {
    cli::cli_abort(message = c(
      "{.var task} must be a supported endpoint",
      "i" = "Run {.run mergenstudio::get_available_endpoints()} to get a list of supported endpoints"
    ))
  }
  httr2::request(getOption("mergenstudio.openai_url")) %>%
    httr2::req_url_path_append(task) %>%
    httr2::req_auth_bearer_token(token = token)
}


#' List supported models
#'
#' Get a list of the models supported by the OpenAI API.
#'
#' @param service The API service
#'
#' @return A character vector
#' @export
#'
#' @examples
#' get_available_endpoints()
get_available_models <- function(service) {
  if (grepl("^openai", service)) {
    check_api()
    models <-
      request_base("models") %>%
      httr2::req_perform() %>%
      httr2::resp_body_json() %>%
      purrr::pluck("data") %>%
      purrr::map_chr("id")
    if(service == "openai-chat"){
      models <- models %>%
        stringr::str_subset("^gpt") %>%
        stringr::str_subset("instruct", negate = TRUE) %>%
        stringr::str_subset("vision", negate = TRUE) %>%
        sort()

      idx <- which(models == "gpt-3.5-turbo")
      models <- c(models[idx], models[-idx])
    } else if(service == "openai-completion"){
      models <- c("text-davinci-003", "text-davinci-002", "text-curie-001", "text-babbage-001", "text-ada-001")
    }
    return(models)
  } else if (service == "replicate"){
    models <- c("llama-2-70b-chat")
  }
  models
}

#' List supported endpoints
#'
#' Get a list of the endpoints supported by mergenstudio.
#'
#' @return A character vector
#' @export
#'
#' @examples
#' get_available_endpoints()
get_available_endpoints <- function() {
  c("completions", "chat/completions", "edits", "embeddings", "models")
}
