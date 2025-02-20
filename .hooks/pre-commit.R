#!/usr/bin/env Rscript
message("\nRunning commit hooks...")

message("\n")

message("1. Checking code styling...\n")
style_output <- eval(styler::style_dir()$changed)
if (any(style_output)) {
  message("Warning: Code failed styling checks.
  \n`styler::style_dir()` has been run for you.
  \nPlease check your files and dashboard still work.
  \nThen re-stage and try committing again.")
  quit(save = "no", status = 1, runLast = FALSE)
} else {
  message("...code styling checks passed")
}

message("\n")
message("\n2. Rebuilding manifest.json...")
if (system.file(package = "git2r") == "") {
  renv::install("git2r")
}
if (system.file(package = "rsconnect") != "" & system.file(package = "git2r") != "") {
  if (!any(grepl("manifest.json", git2r::status()))) {
    rsconnect::writeManifest()
    git2r::add(path = "manifest.json")
  }
  message("...manifest.json rebuilt\n")
} else {
  if (system.file(package = "rsconnect") == "") {
    message("rsconnect is not installed")
  }
  if (system.file(package = "git2r") == "") {
    message("git2r is not installed")
  }
  message("...this step has been skipped")
}

message("\n")

# End of hooks
