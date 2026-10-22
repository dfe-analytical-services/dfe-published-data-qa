#!/usr/bin/env Rscript
message("\nRunning commit hooks...")

if (!renv::status()$synchronized) {
  renv::restore()
}

message("\n")

message("1. Rebuilding manifest.json...")
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
