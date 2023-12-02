#' Mergen Chat in Source
#'
#' Call this function as a Rstudio addin to ask mergen to improve spelling and
#' grammar of selected text.
#'
#' @export
#'
#' @return This function has no return value.
#'
addin_mergenchat_in_source <- function() {
  cli_process_start("Sending query to ChatGPT")
  mergenchat_insource()
  cli_process_done()
}

#' Mergen Chat in Source
#'
#' Giving more useful output in a source (i.e., *.R) file.
#'
#' @return No return value. Replaces the selected text with new text provided by the request.
#'
mergenchat_insource <- function() {

  gptstudio_chat_in_source_file_ext <- character(1L)

  tryCatch(expr = {
    doc_path <- rstudioapi::documentPath()
    gptstudio_chat_in_source_file_ext <<- tools::file_ext(doc_path)
  }, error = function(e) {
    cli::cli_alert_info("Current document is not saved. Assuming .R file extension")
    gptstudio_chat_in_source_file_ext <<- "R"
  })

  # get selection
  selection_query <- get_selection()

  # response
  response <- mergen_chat_completion(selection_query$value)

  # stop
  cli::cli_progress_step("Sending query to mergen...", msg_done = "mergen responded")

  # insert text
  insert_text(response)
}

#' Generate text completions using OpenAI's API for Chat
#'
#' @param prompt The prompt for generating completions
#' @param openai_api_key The API key for accessing OpenAI's API. By default, the
#'   function will try to use the `OPENAI_API_KEY` environment variable.
#'
#' @return A list with the generated completions and other information returned
#'   by the API.
#'
mergen_chat_completion <-
  function(prompt,
           openai_api_key = Sys.getenv("OPENAI_API_KEY")) {

    myAgent <- mergen::setupAgent(name="openai", type="completion", ai_api_key = openai_api_key)
    response <- mergen::sendPrompt(myAgent, prompt = prompt, return.type = "text")
    response
  }