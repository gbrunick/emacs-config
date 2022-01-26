options(menu.graphics = FALSE, pager = "cat")

.gpb_r_mode_get_completions <- function (position, currentLine) {
    utils:::.assignLinebuffer(currentLine)
    utils:::.assignEnd(nchar(currentLine))
    utils:::.guessTokenFromLine()
    utils:::.completeToken()

    # Quote the strings..
    completions <- vapply(utils:::.retrieveCompletions(), deparse, "")
    completions <- paste(completions, collapse = " ")

    string <- position + utils:::.CompletionEnv$start
    end <- position + utils:::.CompletionEnv$end
    elisp <- sprintf("\n(%s %s (%s))\n\n", string, end, completions)
    cat(elisp)
}


traceback <- function (x = NULL, max.lines = getOption("deparse.max.lines")) {
  n <- length(x <- .traceback(x))
  if (n == 0L)
    cat(gettext("No traceback available"), "\n")
  else {
    for (i in 1L:n) {
      xi <- x[[i]]
      label <- paste0(n - i + 1L, ": ")
      m <- length(xi)
      srcloc <- if (!is.null(srcref <- attr(xi, "srcref"))) {
                  srcfile <- attr(srcref, "srcfile")
                  paste0(" at ", srcfile$filename, "#", srcref[1L])
                }
      if (is.numeric(max.lines) && max.lines > 0L && max.lines < m) {
        xi <- c(xi[seq_len(max.lines)], " ...")
        m <- length(xi)
      }
      if (!is.null(srcloc)) {
        xi[m] <- paste0(xi[m], srcloc)
      }
      if (m > 1)
        label <- c(label, rep(substr("          ", 1L,
                                     nchar(label, type = "w")), m - 1L))
      cat(paste0(label, xi), sep = "\n")
    }
  }
  invisible(x)
}
