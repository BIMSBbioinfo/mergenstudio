#' prism dependencies for code syntax highlighting
#' @noRd
prismDependencies <- tags$head(
  tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/prism/1.8.4/prism.min.js"),
  tags$link(rel = "stylesheet", type = "text/css",
            href = "https://cdnjs.cloudflare.com/ajax/libs/prism/1.8.4/themes/prism.min.css")
)

prismLanguageDependencies <- function(languages) {
  lapply(languages, function(x) {
    tags$head(
      tags$script(
        src = paste0("https://cdnjs.cloudflare.com/ajax/libs/prism/1.8.4/components/prism-",
                     x, ".min.js")
      )
    )
  })
}
