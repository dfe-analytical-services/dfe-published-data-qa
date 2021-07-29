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


# disconnect duck ---------------------------------------------------------

getLocalTags <- function() {
  htmltools::tagList(
    htmltools::tags$script(paste0(
      "$(function() {",
      "  $(document).on('shiny:disconnected', function(event) {",
      "    $('#ss-connect-dialogAA').show();",
      "    $('#ss-overlay').show();",
      "  })",
      "});"
    )),
    htmltools::tags$div(id="ss-connect-dialogAA", 
                        style="display: none !important;",
                        
                        htmltools::tags$div(id="ss-connect-refresh", 
                                            
                                            htmltools::tags$p("Something went wrong! Try refreshing the page."),
                                            
                                            htmltools::tags$a(id="ss-reload-link", href="#", onclick="window.location.reload(true);")
                        ),
                        
                        htmltools::tags$div(id="ss-connect-image", 
                                            style="display: none !important;",
                                            
                                            htmltools::tags$img(id="ss-reload-image", src = "builder-duck.PNG"),
                                            
                                            htmltools::tags$p("If this persists, please contact statistics.development@education.gov.uk with details of what you were trying to do.")
                        )
                        
                        
    ),

    
    
    htmltools::tags$div(id="ss-overlay", style="display: none;")
  )
}

customDisconnectMessage <- function(
  text = "An error occurred. Please refresh the page and try again.",
  refresh = "Refresh",
  width = 450,
  top = 50,
  size = 22,
  background = "white",
  colour = "#ffffff", #"#444444",
  overlayColour = "black",
  overlayOpacity = 0.6,
  refreshColour = "#337ab7",
  css = ""
) {
  
  checkmate::assert_string(text, min.chars = 1)
  checkmate::assert_string(refresh)
  checkmate::assert_numeric(size, lower = 0)
  checkmate::assert_string(background)
  checkmate::assert_string(colour)
  checkmate::assert_string(overlayColour)
  checkmate::assert_number(overlayOpacity, lower = 0, upper = 1)
  checkmate::assert_string(refreshColour)
  checkmate::assert_string(css)
  
  if (width == "full") {
    width <- "100%"
  } else if (is.numeric(width) && width >= 0) {
    width <- paste0(width, "px")
  } else {
    stop("disconnectMessage: 'width' must be either an integer, or the string \"full\".", call. = FALSE)
  }
  
  if (top == "center") {
    top <- "50%"
    ytransform <- "-50%"
  } else if (is.numeric(top) && top >= 0) {
    top <- paste0(top, "px")
    ytransform <- "0"
  } else {
    stop("disconnectMessage: 'top' must be either an integer, or the string \"center\".", call. = FALSE)
  }
  
  htmltools::tagList(
    getLocalTags(),
    htmltools::tags$head(
      htmltools::tags$style(
        glue::glue(
          .open = "{{", .close = "}}",
          
          ## This hides the old message
          "#ss-connect-dialog { display: none !important; }", #rsconnect
          "#shiny-disconnected-overlay { display: none !important; }", #local
          
          "#ss-overlay {
             background-color: {{overlayColour}} !important;
             opacity: {{overlayOpacity}} !important;
             position: fixed !important;
             top: 0 !important;
             left: 0 !important;
             bottom: 0 !important;
             right: 0 !important;
             z-index: 99998 !important;
             overflow: hidden !important;
             cursor: not-allowed !important;
          }",
          
          "#ss-connect-dialogAA {
             background: {{background}} !important;
             color: {{colour}} !important;
             width: {{width}} !important;
             transform: translateX(-50%) translateY({{ytransform}}) !important;
             font-size: {{size}}px !important;
             top: {{top}} !important;
             position: fixed !important;
             bottom: auto !important;
             left: 50% !important;
             padding: 0.8em 1.5em !important;
             text-align: center !important;
             height: auto !important;
             opacity: 1 !important;
             z-index: 99999 !important;
             border-radius: 3px !important;
             box-shadow: rgba(0, 0, 0, 0.3) 3px 3px 10px !important;
          }",
          
          # "#ss-connect-dialogAA::before {
          # content: '{{ text }}' ;
          # display: block ;
          # color: #ffffff ;
          # font-size: {{size}}px !important;
          # }",
          
          "#ss-connect-dialogAA a {
             display: {{ if (refresh == '') 'none' else 'block' }} !important;
             color: {{refreshColour}} !important;
             font-size: {{size}}px !important;
             margin-top: {{size}}px !important;
             font-weight: normal !important;
          }",
          
          "#ss-connect-dialogAA a::before {
            content: '{{refresh}}';
            font-size: {{size}}px;
          }",
          
          "#ss-connect-dialogAA { {{ htmltools::HTML(css) }} }"
        )
      )
    )
  )
}