mergenstudio_skeleton <- function(api_key = Sys.getenv("AI_API_KEY"),
                                  service = "openai-chat",
                                  model = "gpt-3.5-turbo",
                                  url = "",
                                  prompt = "Name the top 5 packages in R.",
                                  history = list(
                                    list(
                                      role = "system",
                                      content = "You are an R chat assistant"
                                    )
                                  ),
                                  # stream = TRUE,
                                  selfcorrect = FALSE) {

  if(api_key == ""){
    if(grepl("^openai", service))
      api_key <- Sys.getenv("AI_API_KEY")
  }

  # validate skeleton
  validate_skeleton(api_key, model, prompt, history, selfcorrect)

  structure(
    list(
      api_key = api_key,
      service = service,
      url = url,
      prompt = prompt,
      history = history,
      model = model,
      selfcorrect = selfcorrect
    )
  )
}

#' @importFrom assertthat assert_that
#' @importFrom rlang is_scalar_character is_list is_bool
#' @noRd
validate_skeleton <- function(api_key, model, prompt, history, selfcorrect) {
  # assertthat::assert_that(
  #   Sys.getenv(api_key) != "",
  #   msg = paste0(api_key, " is not found in R environment")
  # )
  assertthat::assert_that(
    rlang::is_scalar_character(model),
    msg = "Model name is not a valid character scalar"
  )
  assertthat::assert_that(
    rlang::is_scalar_character(prompt),
    msg = "Prompt is not a valid list"
  )
  assertthat::assert_that(
    rlang::is_list(history),
    msg = "History is not a valid list"
  )
  # assertthat::assert_that(
  #   rlang::is_bool(stream),
  #   msg = "Stream is not a valid boolean"
  # )
  assertthat::assert_that(
    rlang::is_bool(selfcorrect),
    msg = "Selfcorrect is not a valid boolean"
  )
}

#' @importFrom mergen setupAgent selfcorrect sendPrompt
#' @noRd
mergenstudio_request <- function(skeleton = NULL){

  # check prompt
  if(skeleton$prompt == ""){
    response = "You have to make a request and send a non-empty prompt"
  } else {

    # set agent
    tryCatch({
      if(skeleton$service == "generic"){
        myAgent <- mergen::setupAgent(name=skeleton$service, model = skeleton$model, url = skeleton$url,
                                      ai_api_key = skeleton$api_key)
      } else {
        myAgent <- mergen::setupAgent(name=skeleton$service, model = skeleton$model, ai_api_key = skeleton$api_key)
      }
    },
    error = function(x){
      response = "Request Failed: check your API configurations"
    })

    # if service is not replicate, add history to the prompt in sendPrompt or selfcorrect
    if(skeleton$service == "replicate"){
      previous.msgs <- NULL
    } else {
      previous.msgs <- skeleton$history
    }

    # get response, if setup is failed, says that it failed
    if(exists("myAgent")){
      if(skeleton$selfcorrect){
        print("selfcorrect")
        response <- mergen::selfcorrect(myAgent, prompt = skeleton$prompt, previous.msgs = previous.msgs, attempts = 3)
        response <- response$final.response
        skeleton$history <- c(
          skeleton$history,
          list(
            list(role = "assistant", content = "Self Correct is activated: trying to correct potential errors...")
          )
        )
      } else {
        # send prompt to mergen
        response <- mergen::sendPrompt(myAgent, prompt = skeleton$prompt, previous.msgs = previous.msgs, return.type = "text")
      }
    } else {
      response <- "Request Failed: check your API configurations"
    }
  }

  # update history
  skeleton$history <- c(
    skeleton$history,
    list(
      list(role = "user", content = skeleton$prompt),
      list(role = "assistant", content = response)
    )
  )
  skeleton$prompt <- NULL # remove the last prompt
  skeleton$response <- response
  skeleton
}
