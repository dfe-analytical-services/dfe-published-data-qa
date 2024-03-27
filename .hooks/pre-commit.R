#!/usr/bin/env Rscript
cat("\nRunning commit hooks...", fill = TRUE)

cat("\n")

cat("1. Checking code styling...\n")
style_output <- eval(styler::style_dir()$changed)
if (any(style_output)) {
  cat("Warning: Code failed styling checks.
  \n`styler::style_dir()` has been run for you.
  \nPlease check your files and dashboard still work.
  \nThen re-stage and try committing again.")
  quit(save = "no", status = 1, runLast = FALSE)
} else {
  cat("...code styling checks passed")
}

cat("\n")

# End of hooks
