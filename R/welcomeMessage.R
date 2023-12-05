#' Welcome message
#'
#' HTML widget for showing a welcome message in the chat app.
#' This has been created to be able to bind the message to a shiny event to trigger a new render.
#'
#' @import htmlwidgets
#' @inheritParams run_chat_app
#' @inheritParams welcomeMessage-shiny
#' @inheritParams chat_message_default
#' @param elementId The element's id
#'
#' @importFrom htmlwidgets createWidget
welcomeMessage <- function(ide_colors = get_ide_theme_info(),
                           translator = create_translator(),
                           width = NULL,
                           height = NULL,
                           elementId = NULL) {
  default_message <- chat_message_default(translator = translator)

  # forward options using x
  x <- list(
    message = style_chat_message(default_message, ide_colors = ide_colors) %>% as.character()
  )

  # create widget
  htmlwidgets::createWidget(
    name = "welcomeMessage",
    x,
    width = width,
    height = height,
    package = "mergenstudio",
    elementId = elementId
  )
}

#' Shiny bindings for welcomeMessage
#'
#' Output and render functions for using welcomeMessage within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a welcomeMessage
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name welcomeMessage-shiny
#'
#' @importFrom htmlwidgets shinyWidgetOutput
welcomeMessageOutput <- function(outputId, width = "100%", height = NULL) {
  htmlwidgets::shinyWidgetOutput(outputId, "welcomeMessage", width, height, package = "mergenstudio")
}

#' @rdname welcomeMessage-shiny
#' @importFrom htmlwidgets shinyRenderWidget
renderWelcomeMessage <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) {
    expr <- substitute(expr)
  } # force quoted
  htmlwidgets::shinyRenderWidget(expr, welcomeMessageOutput, env, quoted = TRUE)
}

#' Default chat message
#' @inheritParams mod_chat_ui
#' @importFrom purrr map_chr
#' @importFrom glue glue glue_collapse
chat_message_default <- function(translator = create_translator()) {

  welcome_title <- "Welcome to mergen!" %>%
    purrr::map_chr(~ translator$t(.x))

  welcome_messages <- c(
    "I'm here to guide you through every step, offering unique code execution and insightful prompt chains tailored to your coding needs, regardless of your experience level.",
    "Whether you're starting fresh or already a pro, my advanced prompt chains and code execution capabilities are designed to elevate your R programming journey.",
    "As your specialized R coding assistant, I bring the power of direct code execution and advanced prompt chains to your fingertips. Feel free to ask me anything. I'm here to streamline and enrich your R coding experience.",
    "As an advanced R virtual assistant, I'm equipped with unique features like code execution and interactive prompt chains to help you achieve your coding objectives, no matter how big or small.",
    "As your virtual assistant, I offer more than just guidance. I bring the capabilities of executing R code and engaging with dynamic prompt chains to make your coding journey smoother and more enjoyable."
  ) %>%
    purrr::map_chr(~ translator$t(.x))

  content <- c(
    "{welcome_title}\n\n{sample(welcome_messages, 1)}\n\n",
    translator$t("Type anything to start our conversation.")
  ) %>%
    glue::glue_collapse() %>%
    glue::glue()

  list(
    role = "assistant",
    content = content
  )
}
