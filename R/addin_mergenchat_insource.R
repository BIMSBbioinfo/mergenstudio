#' Mergen Chat in Source
#'
#' Call this function as a Rstudio addin to ask mergen to improve spelling and
#' grammar of selected text.
#'
#' @export
#'
#' @import cli
#'
#' @return This function has no return value.
#'
#' @examples
#' \dontrun{
#' addin_mergenchat_in_source()
#' }
addin_mergenchat_in_source <- function() {
  cli::cli_process_start("Sending query to ChatGPT")
  mergenchat_insource()
  cli::cli_process_done()
}

#' Mergen Chat in Source
#'
#' Giving more useful output in a source (i.e., *.R) file.
#'
#' @import cli
#' @noRd
mergenchat_insource <- function() {

  mergenstudio_chat_in_source_file_ext <- character(1L)

  tryCatch(expr = {
    doc_path <- rstudioapi::documentPath()
    mergenstudio_chat_in_source_file_ext <<- tools::file_ext(doc_path)
  }, error = function(e) {
    cli::cli_alert_info("Current document is not saved. Assuming .R file extension")
    mergenstudio_chat_in_source_file_ext <<- "R"
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
#' @param ai_api_key The API key for accessing OpenAI's API. By default, the
#'   function will try to use the `AI_API_KEY` environment variable.
#'
#' @importFrom mergen setupAgent sendPrompt
#' @noRd
mergen_chat_completion <-
  function(prompt,
           ai_api_key = Sys.getenv("AI_API_KEY")) {

    myAgent <- mergen::setupAgent(name="openai", type="completion", ai_api_key = ai_api_key)
    response <- mergen::sendPrompt(myAgent, prompt = prompt, return.type = "text")
    response
  }
