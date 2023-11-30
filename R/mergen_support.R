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
                                  selfcorrect = FALSE) {

  # api key
  api_key <- switch(service,
                    "openai" = "OPENAI_API_KEY",
                    "replicate" = "my_replicate_key")

  # validate skeleton
  validate_skeleton(api_key, model, prompt, history, selfcorrect)

  structure(
    list(
      api_key = api_key,
      service = service,
      prompt = prompt,
      history = history,
      model = model,
      selfcorrect = selfcorrect
    )
  )
}

# validate_skeleton <- function(model, prompt, history, stream, selfcorrect) {
validate_skeleton <- function(api_key, model, prompt, history, selfcorrect) {
  # assert_that(
  #   Sys.getenv(api_key) != "",
  #   msg = paste0(api_key, " is not found in R environment")
  # )
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

  # set agent
  tryCatch({
    # myAgent <-
    #   switch(skeleton$service,
    #          "openai" = mergen::setupAgent(name=skeleton$service, type="chat", model = skeleton$model, ai_api_key = Sys.getenv(skeleton$api_key)),
    #          "replicate" = mergen::setupAgent(name=skeleton$service, model = skeleton$model, ai_api_key = Sys.getenv(skeleton$api_key)))
    if(skeleton$service == "openai"){
      myAgent <- mergen::setupAgent(name=skeleton$service, type="chat", model = skeleton$model, ai_api_key = Sys.getenv(skeleton$api_key))
    } else if(skeleton$service == "replicate") {
      myAgent <- mergen::setupAgent(name=skeleton$service, model = skeleton$model, ai_api_key = Sys.getenv(skeleton$api_key))
    }
  },
    error = function(x){
      response = "Request Failed: check your API configurations"
  })


  # get response, if setup is failed, says that it failed
  if(exists("myAgent")){
    if(skeleton$selfcorrect){
      response <- mergen::selfcorrect(myAgent, prompt = skeleton$prompt, attempts = 3)
      response <- response$final.response
    } else {
      try
      response <- mergen::sendPrompt(myAgent, prompt = skeleton$prompt, return.type = "text")
    }
  } else {
    response <- "Request Failed: check your API configurations"
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
