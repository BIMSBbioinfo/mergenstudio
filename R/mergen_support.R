#' @importFrom mergen promptContext
#' @noRd
mergenstudio_skeleton <- function(api_key = Sys.getenv("AI_API_KEY"),
                                  service = "openai-chat",
                                  model = "gpt-3.5-turbo",
                                  url = "",
                                  prompt = "Name the top 5 packages in R.",
                                  custom_context = "",
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

  if(custom_context %in% get_available_context()){
    if(custom_context == "No Context"){
      custom_context <- ""
    } else {
      custom_context <- mergen::promptContext(type = custom_context)
    }
  }

  # validate skeleton
  validate_skeleton(api_key, model, prompt, history, selfcorrect)

  structure(
    list(
      api_key = api_key,
      service = service,
      url = url,
      prompt = prompt,
      custom_context = custom_context,
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

#' @importFrom mergen setupAgent selfcorrect sendPrompt promptContext
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
        response <- mergen::selfcorrect(myAgent, prompt = skeleton$prompt, previous.msgs = previous.msgs, attempts = 3, context = skeleton$custom_context)
        response <- response$final.response
      } else {
        # send prompt to mergen
        response <- mergen::sendPrompt(myAgent, prompt = skeleton$prompt, previous.msgs = previous.msgs, return.type = "text", context = skeleton$custom_context)
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

#' @importFrom mergen setupAgent selfcorrect sendPrompt promptContext
#' @noRd
mergenstudio_execute <- function(rv, history, settings, session){

  # cleaning and parsing the code from response
  if(is.null(rv$last_response)) {
    showNotification(ui = "You have to get a response with code first!", duration = 3, type = "message", session = session)
  } else {

    # check history
    if(grepl("^Here are the results once the code is executed", utils::tail(history$chat_history,1)[[1]]$content)){
      showNotification(ui = "You have to get another response to execute code again!", duration = 3, type = "message", session = session)
    } else {

      # get code
      code_cleaned <- mergen::clean_code_blocks(rv$last_response)
      final_code <- mergen::extractCode(code_cleaned,delimiter = "```")
      final_code <- final_code$code

      mergen::extractInstallPkg(final_code)
      # execute code
      setwd(settings$directory)
      code_result<-mergen::executeCode(final_code,output="html",output.file=paste0(getwd(),"/","output_mergen_studio.html"))

      # if html file is created (code did not return any error)
      if (grepl("html",code_result[1])){
        # add result to chat history
        if(length(rvest::read_html(code_result))>1){
          history$chat_history <- c(history$chat_history,
                                    list(list(role = "assistant",
                                              content = shiny::includeHTML(code_result))
                                    ))
        }else{
          history$chat_history <- c(history$chat_history,
                                    list(list(role = "assistant",
                                              content = "The code returns no output.")
                                    ))
        }

        # remove html file
        file.remove(code_result)

        # append code to already generated code in chat:
        mergenstudio_env$code <- paste(mergenstudio_env$code,final_code,sep="\n")

      } else{
        history$chat_history <- c(history$chat_history,
                                  list(list(role = "assistant",
                                            content = paste0("The code resulted in the following errors/warnings:\n```\n",
                                                             code_result,"\n```\n\n"))
                                  ))
      }
    }
  }
}
