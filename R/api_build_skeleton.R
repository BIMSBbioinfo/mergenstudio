#' Construct a GPT Studio request skeleton.
#'
#' @param skeleton A GPT Studio request skeleton object.
#' @param style The style of code to use. Applicable styles can be retrieved
#'   from the "mergenstudio.code_style" option. Default is the
#'   "mergenstudio.code_style" option. Options are "base", "tidyverse", or "no
#'   preference".
#' @param skill The skill level of the user for the chat conversation. This can
#'   be set through the "mergenstudio.skill" option. Default is the
#'   "mergenstudio.skill" option. Options are "beginner", "intermediate",
#'   "advanced", and "genius".
#' @param task Specifies the task that the assistant will help with. Default is
#'   "coding". Others are "general", "advanced developer", and "custom".
#' @param custom_prompt This is a custom prompt that may be used to guide the AI
#'   in its responses. Default is NULL. It will be the only content provided to
#'   the system prompt.
#' @param ... Additional arguments.
#'
#' @return An updated GPT Studio request skeleton.
#'
#' @export
mergenstudio_skeleton_build <- function(skeleton, skill, style, task, custom_prompt, ...) {
  UseMethod("mergenstudio_skeleton_build")
}

#' @export
mergenstudio_skeleton_build.mergenstudio_request_openai <-
  function(skeleton = mergenstudio_create_skeleton(),
           skill    = getOption("mergenstudio.skill") ,
           style    = getOption("mergenstudio.code_style"),
           task     = "coding",
           custom_prompt = NULL,
           ...) {
    prompt      <- skeleton$prompt
    history     <- skeleton$history
    model       <- skeleton$model
    stream      <- skeleton$stream
    new_history <- prepare_chat_history(history, style, skill, task, custom_prompt)

    new_mergenstudio_request_skeleton_openai(model   = model,
                                          prompt  = prompt,
                                          history = new_history,
                                          stream  = stream)
  }

#' @export
mergenstudio_skeleton_build.mergenstudio_request_replicate <-
  function(skeleton = mergenstudio_create_skeleton(),
           skill    = getOption("mergenstudio.skill") ,
           style    = getOption("mergenstudio.code_style"),
           task     = "coding",
           custom_prompt = NULL,
           ...) {
    prompt      <- skeleton$prompt
    history     <- skeleton$history
    model       <- skeleton$model
    stream      <- skeleton$stream
    new_history <- prepare_chat_history(history, style, skill, task, custom_prompt)

    new_mergenstudio_request_skeleton_replicate(model   = model,
                                             prompt  = prompt,
                                             history = new_history,
                                             stream  = stream)
  }

#' #' @export
#' mergenstudio_skeleton_build.mergenstudio_request_anthropic <-
#'   function(skeleton = mergenstudio_create_skeleton("anthropic"),
#'            skill    = getOption("mergenstudio.skill") ,
#'            style    = getOption("mergenstudio.code_style"),
#'            task     = "coding",
#'            custom_prompt = NULL,
#'            ...) {
#'     prompt         <- skeleton$prompt
#'     history        <- skeleton$history
#'     model          <- skeleton$model
#'     stream         <- skeleton$stream
#'     new_history <- prepare_chat_history(history, style, skill, task, custom_prompt)
#'
#'     new_mergenstudio_request_skeleton_anthropic(model   = model,
#'                                              prompt  = prompt,
#'                                              history = new_history,
#'                                              stream  = stream)
#'   }
#'
#' #' @export
#' mergenstudio_skeleton_build.mergenstudio_request_palm <-
#'   function(skeleton = mergenstudio_create_skeleton("palm"),
#'            skill    = getOption("mergenstudio.skill") ,
#'            style    = getOption("mergenstudio.code_style"),
#'            task     = "coding",
#'            custom_prompt = NULL,
#'            ...) {
#'     prompt         <- skeleton$prompt
#'     history        <- skeleton$history
#'     model          <- skeleton$model
#'     stream         <- skeleton$stream
#'     new_history <- prepare_chat_history(history, style, skill, task, custom_prompt)
#'
#'     new_mergenstudio_request_skeleton_palm(model   = model,
#'                                         prompt  = prompt,
#'                                         history = new_history,
#'                                         stream  = stream)
#'   }
#'
#' #' @export
#' mergenstudio_skeleton_build.mergenstudio_request_azure_openai <-
#'   function(skeleton = mergenstudio_create_skeleton(),
#'            skill    = getOption("mergenstudio.skill") ,
#'            style    = getOption("mergenstudio.code_style"),
#'            task     = "coding",
#'            custom_prompt = NULL,
#'            ...) {
#'     prompt      <- skeleton$prompt
#'     history     <- skeleton$history
#'     model       <- skeleton$model
#'     stream      <- skeleton$stream
#'     new_history <- prepare_chat_history(history, style, skill, task, custom_prompt)
#'
#'     new_mergenstudio_request_skeleton_azure_openai(model   = model,
#'                                                 prompt  = prompt,
#'                                                 history = new_history,
#'                                                 stream  = stream)
#'   }
