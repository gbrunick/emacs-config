# Always use absolute paths when sourcing files.  This makes file handling
# much easier on the Emacs side.
source <- function(file, ...) {
  base::source(file = base::normalizePath(file), ...)
}

# Show the full path in the traceback
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


# We collect all our functions in single list to avoid poluting the R
# global namespace too much.
.gpb_r_mode <- local({

  # This should agree with `gpb-r-end-of-output-marker` in gpb-r-mode.el.
  start_marker <- 'gpb-r-callback:'
  end_marker <- 'END:75b30f72-85a0-483c-98ce-d24414394ff0'

  quote_string <- function(txt) deparse(txt)

  get_completions <- function (position, currentLine) {
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

  invoke_callback <- function(...) {
    cat(sprintf("%s %s %s\n", start_marker, sprintf(...), end_marker))
  }

  print_error_location <- function() {
    srcref <- getSrcref(tail(sys.calls(), 1)[[1]])
    if (is.null(srcref) && length(sys.calls()) > 1) {
      srcref <- getSrcref(tail(sys.calls(), 2)[[1]])
    }
    if (!is.null(srcref)) {
      file <- file.path(getSrcDirectory(srcref), getSrcFilename(srcref))
      line <- getSrcLocation(srcref)
      invoke_callback("(gpb-r--add-error-info %s %s)", deparse(file), line)
    }
  }

  # `name` is a string giving a function name.
  get_args <- function(name) {
    func <- tryCatch({
      f <- eval(parse(text = name))
      stopifnot(is.function(f))
      f
    }, error = function(e) NULL)

    if (is.null(func)) {
      cat("NULL\n")
      return(invisible(NULL))
    }

    lines <- deparse(args(func))
    lines <- trimws(lines, "left")
    # The last line is NULL
    lines <- paste(head(lines, -1), collapse = "")
    eldoc_info <- trimws(sub("^function +", name, lines))
    cat(sprintf("\n%s\n", eldoc_info))
    invisible(NULL)
  }

  # We pass this temp file to Emacs for region evaluation.
  region_file <- tempfile("region-", fileext = ".R")

  eval_region_file <- function(buffer_name, wd = NULL, namespace = NULL) {
    lines <- readLines(region_file)
    src <- srcfilecopy(sprintf("[%s]", buffer_name), lines,
                       timestamp = Sys.time(), isFile = FALSE)
    expr <- parse(text = lines, srcfile = src)

    if (!is.null(wd)) {
      save_dir <- setwd(wd)
      on.exit(setwd(save_dir))
    }

    if (is.null(namespace)) {
      eval(expr, globalenv())
    } else {
      eval(expr, asNamespace(namespace))
    }

    invisible(NULL)
  }

  shut_down <- function() {
    savehistory()
    quit(save = "no")
  }
  
  options(menu.graphics = FALSE,
          pager = "cat",
          error = print_error_location)

  # This is the API exposed to grp-r-mode.el.
  list(get_completions = get_completions,
       get_args = get_args,
       region_file = region_file,
       eval_region_file = eval_region_file
       shut_down = shut_down)
})
