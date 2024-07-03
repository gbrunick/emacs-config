# We collect all our functions in single list to avoid poluting the R
# global namespace too much.
.gpb_r_mode <- local({

  get_completions <- function (currentLine) {
    utils:::.assignLinebuffer(currentLine)
    utils:::.assignEnd(nchar(currentLine))
    utils:::.guessTokenFromLine()
    utils:::.completeToken()

    # Quote the strings..
    completions <- vapply(utils:::.retrieveCompletions(), deparse, "")
    completions <- paste(completions, collapse = " ")
    elisp <- sprintf("\n(%s %s (%s))\n\n",
                     utils:::.CompletionEnv$start,
                     utils:::.CompletionEnv$end,
                     completions)
    cat(elisp)
  }

  # Print a traceback on error.
  error_callback <- function() {
    calls <- .traceback(1)

    # We prefer srcref line info when it is available.
    for (i in seq_along(call)) {
      srcref <- getSrcref(calls[[i]])
      if (!is.null(srcref)) {
        calls[[i]] <- as.character(srcref)
        attr(calls[[i]], "srcref") <- srcref
      }
    }
    traceback_wrapper(rev(calls))
  }

  # Emacs writes to this file for region evaluation.
  region_file <- tempfile("region-", fileext = ".R")
  writeLines("init", region_file)

  sync_working_dir <- function() {
    dir <- getwd()
    emit_chdir(dir)
    cat(sprintf("Working dir: %s\n", dir))
  }

  #' Evaluate an region from Emacs
  #'
  #' @param buffer_name String
  #' @param wd Optional working directory
  #' @param namespace Optional R namespace. See `asNamespace`
  #'
  #' @return
  #'
  #' @export
  #'
  eval_region_file <- function(buffer_name, wd = NULL, namespace = NULL) {
    lines <- readLines(region_file)
    # Filenames that begin and end with braces are buffer names.
    src <- srcfilecopy(sprintf("[%s]", buffer_name), lines,
                       timestamp = Sys.time(), isFile = FALSE)
    expr <- parse(text = lines, srcfile = src)

    if (!is.null(wd)) {
      save_dir <- setwd(wd)
      emit_chdir(wd)
      on.exit(emit_chdir(save_dir))
    }

    if (is.null(namespace)) {
      eval(expr, globalenv())
    } else {
      eval(expr, asNamespace(namespace))
    }

    invisible(NULL)
  }

  # Should agree with `gpb-r-guid'."_75b30f72-85a0-483c-98ce-d24414394ff0"
  guid <- "7b530f72-85a0-483c-98ce-d24414394ff0"

  emit_chdir <- function(dir) {
    if (file.exists(dir)) {
      dir <- base::normalizePath(dir)
    }
    cat(sprintf("%s:CHDIR:%s\n", guid, dir))
  }

  # Wrappers around some standard functions

  source_wrapper <- function(file, ..., chdir = FALSE) {
    if (file.exists(file)) {
      file <- base::normalizePath(file)
    }
    if (chdir) {
      wd <- base::normalizePath(getwd())
      emit_chdir(dirname(file))
      on.exit(emit_chdir(wd))
    }
    base::source(file, ..., chdir = chdir)
  }

  traceback_wrapper <- function(x = NULL, ...) {
    basename <- function(path) {
      if (file.exists(path)) base::normalizePath(path)
      else base::basename(path)
    }
    f <- base::traceback
    environment(f) <- environment()
    f(x, ...)
  }

  render_wrapper <- function(input, ..., envir = parent.frame()) {
    if (file.exists(input)) {
      input <- base::normalizePath(input)
    }
    wd <- base::normalizePath(getwd())
    emit_chdir(dirname(input))
    on.exit(emit_chdir(wd))
    rmarkdown::render(input, ..., envir = envir)
  }

  # Emacs reads this output and sets `gpb-r-mode--region-file'.
  cat(sprintf("region-file: %s\n", base::normalizePath(region_file)))

  options(menu.graphics = FALSE,
          pager = "cat",
          error = error_callback,
          prompt = sprintf("%s:PROMPT", guid))

  list(
    # This is the API exposed to grp-r-mode.el.
    get_completions = get_completions,
    eval_region_file = eval_region_file,
    sync_working_dir = sync_working_dir,

    # And these are used below.
    source_wrapper = source_wrapper,
    traceback_wrapper = traceback_wrapper,
    render_wrapper = render_wrapper)
})

# We wrap some standard functions to make them output more more path
# information when they are called.
source    <- .gpb_r_mode$source_wrapper
traceback <- .gpb_r_mode$traceback_wrapper
render    <- .gpb_r_mode$render_wrapper
