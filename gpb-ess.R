.getCompletions <- function (string, end = nchar(string)) {
  utils:::.assignLinebuffer(string)
  utils:::.assignEnd(end)
  utils:::.guessTokenFromLine()
  utils:::.completeToken()
  candidates <- utils:::.retrieveCompletions()
  cat(sprintf("Token:\n%s\n\n", get("token", utils:::.CompletionEnv)))
  cat("Candidates:\n")
  for (name in candidates) {
    cat(sprintf("%s\n", name))
  }
  cat(".CompletionEnv:\n")
  print(as.list(utils:::.CompletionEnv))
}
