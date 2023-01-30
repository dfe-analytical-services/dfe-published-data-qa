# ---------- Lines to use to profile a running app
# install.packages("profvis")
# library(profvis)
# profvis::profvis({ runApp() })

#
# profvis(prof_input = 'profiles.Rprof')

# ---------- Lines to use to profile specific R code (currently laid out for a function against a refactored version)

profvis({
  # data <- fread("c:/R_Projects/dfe-published-data-qa/tests/shinytest/test-data/passes_everything.csv", encoding = "UTF-8", colClasses = c("character"), na.strings = NULL)
  # meta <- fread("c:/R_Projects/dfe-published-data-qa/tests/shinytest/test-data/passes_everything.meta.csv", encoding = "UTF-8", colClasses = c("character"), na.strings = NULL)

  # ---- Old code

  old <- function() {
    "School" %in% unlist(distinct(data, geographic_level), use.names = FALSE)
  }

  old()

  # ---- Potential new code

  new <- function() {
    "School" %in% unique(data$geographic_level)
  }

  new()
})
