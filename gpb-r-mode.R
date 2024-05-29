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

  print_error_location <- function() {
    srcref <- getSrcref(tail(sys.calls(), 1)[[1]])
    if (is.null(srcref) && length(sys.calls()) > 1) {
      srcref <- getSrcref(tail(sys.calls(), 2)[[1]])
    }
    if (!is.null(srcref)) {
      file <- file.path(getSrcDirectory(srcref), getSrcFilename(srcref))
      line <- getSrcLocation(srcref)
      cat(sprintf("Error at %s#%s\n", file, line))
    }
  }

  # Emacs writes to this file for region evaluation.
  region_file <- tempfile("region-", fileext = ".R")
  writeLines("init", region_file)

  sync_working_dir <- function() {
    wd <- normalizePath(getwd())
    cat(sprintf("chdir: %s\n", wd))
    cat(sprintf("Current Working Dir: %s\n", wd))
  }

  eval_region_file <- function(buffer_name, wd = NULL, namespace = NULL) {
    lines <- readLines(region_file)
    src <- srcfilecopy(sprintf("[%s]", buffer_name), lines,
                       timestamp = Sys.time(), isFile = FALSE)
    expr <- parse(text = lines, srcfile = src)

    if (!is.null(wd)) {
      save_dir <- setwd(wd)
      cat(sprintf("chdir: %s", wd))
      on.exit({
        setwd(save_dir)
        cat(sprintf("chdir: %s", save_dir))
      })
    }

    if (is.null(namespace)) {
      eval(expr, globalenv())
    } else {
      eval(expr, asNamespace(namespace))
    }

    invisible(NULL)
  }

  # Wrappers around functions that change the working directory.
  source <- function(file, ..., chdir = FALSE) {
    if (chdir) {
     wd1 <- normalizePath(dirname(file))
     cat(sprintf("chdir: %s\n", wd1))
     wd2 <- normalizePath(getwd())
     on.exit(cat(sprintf("chdir: %s\n", wd2)))
    }
    base::source(file, ..., chdir = chdir)
  }

  # Show full paths in `traceback'
  traceback <- function(...) {
    basename <- normalizePath
    f <- base::traceback
    environment(f) <- environment()
    f(...)
  }

  options(menu.graphics = FALSE,
          pager = "cat",
          error = print_error_location,
          prompt = "PROMPT:75b30f72-85a0-483c-98ce-d24414394ff0")

  # This is the API exposed to grp-r-mode.el.
  list(region_file = region_file,
       get_completions = get_completions,
       eval_region_file = eval_region_file,
       sync_working_dir = sync_working_dir,
       # Advised versions of core R functions.
       source = source,
       traceback = traceback)
})

# Emacs reads this output and sets `gpb-r-mode--region-file'.
cat(sprintf("region-file: %s\n", normalizePath(.gpb_r_mode$region_file)))
