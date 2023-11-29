update_history <- function(response, prompt, history){
  c(history,
    list(
      list(role = "user", content = prompt),
      list(role = "assistant", content = response)
    ))
}
