# Max file size ---------------------------------------------------------------------------------

options(shiny.maxRequestSize = 500 * 1024^2)

# Sanitising error messages (to avoid revealing anything untoward)

options(shiny.sanitize.errors = TRUE)

# Library calls ---------------------------------------------------------------------------------

library(shiny)
library(janitor)
library(dplyr)
library(stringr)
library(data.table)
library(shinyjs)
library(tools)
library(readr)
library(testthat)
library(shinytest)
library(styler)
library(tidyr)
library(ggplot2)
library(shinyFeedback)
library(DT)
library(shinyWidgets)
library(shinycssloaders)
library(sparkline)
library(config)
library(rsconnect)
library(shinyalert)
library(shinydisconnect)

# activeTestsInFile ---------------------------------------------------------------------------------
# Extracting the active tests that are run against files

activeTestsInFile <- function(file) {
  trimws(gsub("\\(.* # active test", "", grep("(# active test)$", read_lines(file), value = TRUE)))
}

activeTests <- sapply(c("R/fileValidation.r", "R/preCheck1.r", "R/preCheck2.r", "R/mainTests.r"), activeTestsInFile, simplify = FALSE)

numberActiveTests <- length(unlist(activeTests, use.names = FALSE))

# tidy_code_function -------------------------------------------------------------------------------

tidy_code_function <- function() {
  message("----------------------------------------")
  message("App scripts")
  message("----------------------------------------")
  app_scripts <- eval(styler::style_dir(recursive = FALSE)$changed)
  message("R scripts")
  message("----------------------------------------")
  r_scripts <- eval(styler::style_dir("R/")$changed)
  message("Test scripts")
  message("----------------------------------------")
  test_scripts <- eval(styler::style_dir("tests/", filetype = "r")$changed)
  script_changes <- c(app_scripts, r_scripts, test_scripts)
  return(script_changes)
}


# Function scripts ---------------------------------------------------------------------------------

source("R/knownVariables.r", encoding = "UTF-8")
source("R/readFile.r")
source("R/screenFiles.r")

# present_file_size ---------------------------------------------------------------------------------
# Function to show the file size

present_file_size <- function(filesize) {
  if (is.null(filesize)) {} else {
    if (round(filesize / 1024 / 1024 / 1024, 2) >= 1) {
      return(paste0(round(filesize / 1024 / 1024 / 1024, 2), " GB"))
    } else {
      if (round(filesize / 1024 / 1024, 2) < 1) {
        return(paste0(round(filesize / 1024, 2), " Bytes"))
      } else {
        return(paste0(round(filesize / 1024 / 1024, 2), " MB"))
      }
    }
  }
}

# Results boxes ----------------------------------------------------------------------------

pass_results_box <- function() {
  div(
    div(
      class = "panel panel-success",
      div(
        class = "panel-heading",
        style = "color: white;font-size: 18px;font-style: bold;", # background-color: #70ad47;
        "All the screener checks passed!"
      ),
      div(
        class = "panel-body",
        style = "padding-left:27px",
        "Your files can now be uploaded to Explore Education Statistics, see our  ",
        a(href = "https://rsconnect/rsc/stats-production-guidance/ees.html", "guidance on using EES", target = "_blank"),
        " for more information."
      )
    )
  )
}

fail_results_box <- function(message, table) {
  div(
    div(
      class = "panel panel-danger",
      div(
        class = "panel-heading",
        style = "color: white;font-size: 18px;font-style: bold;", # background-color: #d45859;
        textOutput(message)
      ),
      div(
        class = "panel-body",
        tableOutput(table)
      )
    )
  )
}

advisory_results_box <- function(message, table) {
  div(
    div(
      class = "panel panel-warning",
      div(
        class = "panel-heading",
        style = "color: white;font-size: 18px;font-style: bold; ", # background-color: #e87421;
        textOutput(message)
      ),
      div(
        class = "panel-body",
        tableOutput(table)
      )
    )
  )
}

results_box <- function(message, table) {
  div(
    div(
      class = "panel panel-secondary",
      div(
        class = "panel-heading",
        style = "background-color: #727477; color: white;font-size: 18px; height:47px; vertical-align: middle; line-height:47px; padding-top:0px",
        div(
          class = "row",
          div(
            class = "col-sm-9",
            textOutput(message)
          ),
          div(
            class = "col-sm-3",
            downloadButton("download_results", "Download results")
          )
        )
      ),
      div(
        class = "panel-body",
        tableOutput(table)
      )
    )
  )
}

ancillary_box <- function() {
  div(
    div(
      class = "panel panel-info",
      div(
        class = "panel-heading",
        style = "color: white;font-size: 18px;font-style: bold;", # background-color: #38a1c1;
        "This file should not be uploaded as a standard data file"
      ),
      div(
        class = "panel-body",
        style = "padding-left:27px",
        "The data file only includes geographic levels that are going to be ignored by EES, see our  ",
        a(href = "https://rsconnect/rsc/stats-production-guidance/ees.html#Ancillary_file_uploads", "guidance on uploading ancillary files", target = "_blank"),
        " for more information on how to upload as an ancillary file instead of via the normal data route."
      )
    )
  )
}

# cs_num ----------------------------------------------------------------------------
# Comma separating function

cs_num <- function(value) {
  format(value, big.mark = ",", trim = TRUE)
}

# summarise_stats ----------------------------------------------------------------------------
# Summarising the counts of the results

summarise_stats <- function(count, text) {
  if (count == 1) {
    paste0(count, " test ", text)
  } else {
    paste0(count, " tests ", text)
  }
}

# appLoadingCSS ----------------------------------------------------------------------------
# Set up loading screen

appLoadingCSS <- "
#loading-content {
  position: absolute;
  background: #000000;
  opacity: 0.9;
  z-index: 100;
  left: 0;
  right: 0;
  height: 100%;
  text-align: center;
  color: #FFFFFF;
}
"

# roundFiveUp ----------------------------------------------------------------------------
# This function is used in place of round() which rounds 5's down
roundFiveUp <- function(x, n) {
  positiveNegative <- sign(x)
  z <- abs(x) * 10^n
  z <- z + 0.5 + sqrt(.Machine$double.eps)
  z <- trunc(z)
  z <- z / 10^n
  return(z * positiveNegative)
}


# spinner options ---------------------------------------------------------
options(spinner.type = 5)
options(spinner.color = "#c8c8c8") # Grey '#C0C0C0') # Laura's blue #6294C6
options(spinner.size = .5)
