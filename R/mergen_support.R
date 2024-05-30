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
mergenstudio_request <- function(skeleton = NULL,resp=NULL){

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
        myAgent <- mergen::setupAgent(name=skeleton$service, model = skeleton$model, ai_api_key = skeleton$api_key, type="chat")
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
    # if resp is already given it means we need to go into self-correction mode with runCodeInResponse.
    if(exists("myAgent")){
      if(skeleton$selfcorrect){
        if (is.null(resp)){
          response <- mergen::sendPrompt(myAgent, prompt = skeleton$prompt, previous.msgs = previous.msgs, return.type = "text", context = skeleton$custom_context)
        }else{
          response <- mergen::runCodeInResponse(response=resp, prompt = skeleton$prompt, agent = myAgent, context = skeleton$custom_context, attempts = 3, correction = 'selfcorrect')
          response <- response$final.response
        }

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



#' @noRd
evaluate_error <- function(codestring) {
  # Parse the code string into an expression
  parsed_code <- parse(text = codestring)

  # Deparse the parsed expression into individual elements
  code_elements <- sapply(parsed_code, deparse)

  for (i in 1:length(code_elements)) {
    result <- tryCatch(
      {
        eval(str2expression(code_elements[[i]]))
        NULL  # Return NULL if no error or warning occurs
      },
      error = function(e) {
        res <- paste(e, 'error occured on line:', code_elements[i], 'line number', i, sep = " ")
        return(res)  # Return the error message
      },
      warning = function(w) {
        res <- paste(w, 'warning occured on line:', code_elements[i], 'line number', i, sep = " ")
        return(res)  # Return the warning message
      }
    )

    if (!is.null(result)) {
      return(result)  # Return from the main function if an error or warning occurs
    }
  }
  return(invisible(NULL))  # Return NULL invisibly if no error or warning occurs
}



#' @importFrom mergen setupAgent selfcorrect sendPrompt promptContext
#' @noRd
mergenstudio_execute <- function(rv, history, settings, session,rep=NULL,code=NULL){
  if (is.null(rep)){
    # cleaning and parsing the code from response
    if(is.null(rv$last_response)) {
        showNotification(ui = "You have to get a response with code first!", duration = 3, type = "message", session = session)
      } else {
          # get code
          code_cleaned <- mergen::clean_code_blocks(rv$last_response)
          final_code <- mergen::extractCode(code_cleaned,delimiter = "```")
          final_code <- final_code$code
        }
  } else if(!is.null(rep) & !is.null(code)) {
      #subs needed for matching rep to history
      rep<-gsub("language-r\n","",rep)
      rep<-gsub(" Copy\n","",rep)
      rep<-gsub("\n\n\n","\n```\n\n",rep)
      rep<-gsub("`","",rep)
      rep<-gsub("\n","",rep)
      final_code <- code
  }else{
    code_cleaned <- mergen::clean_code_blocks(rv$last_response)
    final_code <- mergen::extractCode(code_cleaned,delimiter = "```")
    final_code <- final_code$code
  }


  #if code gets back in multiple blocks.
  if (length(final_code)>1){
    final_code <- paste(final_code,collapse="\n")
    print(final_code)
  }

    # extract install
    mergen::extractInstallPkg(final_code)


    # execute code
    setwd(settings$directory)

    #check what is going on
    code_result<-suppressWarnings(suppressMessages(mergen::executeCode(final_code,output="html",output.file=paste0(getwd(),"/","output_mergen_studio.html"))))

    # search for correct position with help of the whole response rep
    pos<-0
    if (is.null(code)){
      #then it comes from auto execution so should append to the bottom.
      pos<-length(history$chat_history)
    }else{
      for (i in 1:length(history$chat_history)){
        curr_mess<-history$chat_history[[i]]$content
        curr_mess<-gsub("```R","",curr_mess)
        curr_mess<-gsub("`","",curr_mess)
        curr_mess<-gsub("\n","",curr_mess)
        curr_mess<-gsub("\\{r\\}","",curr_mess)
        curr_mess<-gsub("\\{R\\}","",curr_mess)
        if (grepl(rep,curr_mess,fixed=TRUE)){
          pos<-i
          }
      }
    }

    if (pos==0){
      showNotification(ui = "cant find location for result. Will append result to the bottom!", duration = 4, type = "message", session = session)
      pos <- length(history$chat_history)
    }

    if (pos!=0){
      if (grepl("html",code_result[1])){
        # add result to chat history
        if(length(rvest::read_html(code_result))>1){
          message<-list(list(role = "assistant",
                             content = shiny::includeHTML(code_result)))

        }else{
          message<-list(list(role = "assistant",
                             content = "The code returns no output."))
        }
      } else{
        res<-evaluate_error(final_code)
        message<-list(list(role = "user",
                           content = paste0("The code resulted in the
                                            following errors/warnings:\n```\n##",
                                            code_result,"\n```\n\n","```\n##",res,"\n```\n\n")))

      }


      if (file.exists('output_mergen_studio.html')){
        file.remove('output_mergen_studio.html')
      }

      # append to history at the right position
      if (pos == length(history$chat_history)){
          history$chat_history <- c(history$chat_history[1:pos],message)
      }else{
        # <p><img or <pre><code>## or message indicates there was a previous run of that code
        # so we overwrite that position with our new run
        if (grepl("<p><img",history$chat_history[[pos+1]]$content) |
            grepl("<pre><code>##",history$chat_history[[pos+1]]$content) |
            grepl ("The code resulted in the following errors/warnings:",history$chat_history[[pos+1]]$content) |
            grepl("The code returns no output.",history$chat_history[[pos+1]]$content)){
          if ((pos+1)==length(history$chat_history)){
            history$chat_history <- c(history$chat_history[1:pos],message)
          }else{
            history$chat_history <- c(history$chat_history[1:pos],message,
                                      history$chat_history[(pos+2):length(history$chat_history)])
          }
        }else{
          history$chat_history <- c(history$chat_history[1:pos],message,
                                    history$chat_history[(pos+1):length(history$chat_history)])
        }
      }

    }
}



