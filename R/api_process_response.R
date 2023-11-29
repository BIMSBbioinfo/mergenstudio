#' Call API
#'
#' This function provides a generic interface for calling different APIs
#' (e.g., OpenAI, PALM (MakerSuite)). It dispatches the actual API
#' calls to the relevant method based on the `class` of the `skeleton` argument.
#'
#' @param skeleton A `mergenstudio_response_skeleton` object
#' @param ... Extra arguments, not currently used
#'
#' @return A `mergenstudio_request_skeleton` with updated history and prompt removed
#'
#' @examples
#' \dontrun{
#' mergenstudio_response_process(mergenstudio_skeleton)
#' }
#' @export
mergenstudio_response_process <- function(skeleton, ...) {
  UseMethod("mergenstudio_response_process")
}

#' @export
mergenstudio_response_process.mergenstudio_response_openai <-
  function(skeleton, ...) {
    response <- skeleton$response
    skeleton <- skeleton$skeleton

    if (skeleton$stream == TRUE) {
      last_response = response
    } else {
      last_response <- response$choices[[1]]$message$content
    }

    new_history <- c(
      skeleton$history,
      list(
        list(role = "user", content = skeleton$prompt),
        list(role = "assistant", content = last_response)
      )
    )

    skeleton$history <- new_history
    skeleton$prompt <- NULL # remove the last prompt
    class(skeleton) <- c("mergenstudio_request_skeleton",
                         "mergenstudio_request_openai")
    skeleton
  }

#' @export
mergenstudio_response_process.mergenstudio_response_replicate <-
  function(skeleton, ...) {
    response <- skeleton$response
    skeleton <- skeleton$skeleton

    if (skeleton$stream == TRUE) {
      last_response = response
    } else {
      last_response <- response$choices[[1]]$message$content
    }

    new_history <- c(
      skeleton$history,
      list(
        list(role = "user", content = skeleton$prompt),
        list(role = "assistant", content = last_response)
      )
    )

    skeleton$history <- new_history
    skeleton$prompt <- NULL # remove the last prompt
    class(skeleton) <- c("mergenstudio_request_skeleton",
                         "mergenstudio_request_replicate")
    skeleton
  }

#' #' @export
#' mergenstudio_response_process.mergenstudio_response_anthropic <-
#'   function(skeleton, ...) {
#'     response <- skeleton$response
#'     skeleton <- skeleton$skeleton
#'
#'     new_history <- c(
#'       skeleton$history,
#'       list(
#'         list(role = "user", content = skeleton$prompt),
#'         list(role = "assistant", content = response)
#'       )
#'     )
#'
#'     skeleton$history <- new_history
#'     skeleton$prompt <- NULL # remove the last prompt
#'     class(skeleton) <- c("mergenstudio_request_skeleton",
#'                          "mergenstudio_request_anthropic")
#'     skeleton
#'   }
#'
#' #' @export
#' mergenstudio_response_process.mergenstudio_response_palm <-
#'   function(skeleton, ...) {
#'     response <- skeleton$response
#'     skeleton <- skeleton$skeleton
#'
#'     new_history <- c(
#'       skeleton$history,
#'       list(
#'         list(role = "user", content = skeleton$prompt),
#'         list(role = "assistant", content = response)
#'       )
#'     )
#'
#'     skeleton$history <- new_history
#'     skeleton$prompt <- NULL # remove the last prompt
#'     class(skeleton) <- c("mergenstudio_request_skeleton",
#'                          "mergenstudio_request_palm")
#'     skeleton
#'   }
#'
#' #' @export
#' mergenstudio_response_process.mergenstudio_response_azure_openai <-
#'   function(skeleton, ...) {
#'     response <- skeleton$response
#'     skeleton <- skeleton$skeleton
#'
#'     if (skeleton$stream == TRUE) {
#'       last_response = response
#'     } else {
#'       last_response <- response$choices[[1]]$message$content
#'     }
#'
#'     new_history <- c(
#'       skeleton$history,
#'       list(
#'         list(role = "user", content = skeleton$prompt),
#'         list(role = "assistant", content = last_response)
#'       )
#'     )
#'
#'     skeleton$history <- new_history
#'     skeleton$prompt <- NULL # remove the last prompt
#'     class(skeleton) <- c("mergenstudio_request_skeleton",
#'                          "mergenstudio_request_azure_openai")
#'     skeleton
#'   }
