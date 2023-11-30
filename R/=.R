
tryCatch({
  agent <- mergen::setupAgent(name="replicate", model = "llama-2-70b-chat", ai_api_key = Sys.getenv("my_replicate_key"))
}, error = function(x){
  response = "Request Failed: check your API configurations"
})
