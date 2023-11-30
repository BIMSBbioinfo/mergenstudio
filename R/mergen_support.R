mergenstudio_skeleton <- function(service = "openai",
                                  prompt = "Name the top 5 packages in R.",
                                  history = list(
                                    list(
                                      role = "system",
                                      content = "You are an R chat assistant"
                                    )
                                  ),
                                  model = "gpt-3.5-turbo",
                                  # stream = TRUE,
                                  selfcorrect = FALSE,
                                  ...) {

  # validate_skeleton(model, prompt, history, stream, selfcorrect)
  validate_skeleton(model, prompt, history, selfcorrect)
  structure(
    list(
      service = service,
      prompt = prompt,
      history = history,
      model = model,
      selfcorrect = selfcorrect
    )
  )
}

# validate_skeleton <- function(model, prompt, history, stream, selfcorrect) {
validate_skeleton <- function(model, prompt, history, selfcorrect) {
  assert_that(
    rlang::is_scalar_character(model),
    msg = "Model name is not a valid character scalar"
  )
  assert_that(
    rlang::is_scalar_character(prompt),
    msg = "Prompt is not a valid list"
  )
  assert_that(
    rlang::is_list(history),
    msg = "History is not a valid list"
  )
  # assert_that(
  #   rlang::is_bool(stream),
  #   msg = "Stream is not a valid boolean"
  # )
  assert_that(
    rlang::is_bool(selfcorrect),
    msg = "Selfcorrect is not a valid boolean"
  )
}

#' @importFrom mergen setupAgent selfcorrect sendPrompt
mergenstudio_request <- function(skeleton = NULL){

  # merge query
  myAgent <- mergen::setupAgent(name=skeleton$service, type="chat", model = skeleton$model, ai_api_key = Sys.getenv("OPENAI_API_KEY"))
  if(skeleton$selfcorrect){
    print("self correct is invoked!")
    response <- mergen::selfcorrect(myAgent, prompt = skeleton$prompt, attempts = 3)
    response <- response$final.response
  } else {
    response <- mergen::sendPrompt(myAgent, prompt = skeleton$prompt, return.type = "text")
  }

  new_history <- c(
    skeleton$history,
    list(
      # list(role = "user", content = skeleton$prompt),
      list(role = "assistant", content = response)
    )
  )

  skeleton$history <- new_history
  skeleton$prompt <- NULL # remove the last prompt
  skeleton$response <- response
  skeleton
}
