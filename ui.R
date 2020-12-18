fluidPage(
  theme = "acalat_theme.css",

  useShinyjs(),
  # options(shiny.reactlog = TRUE),

  inlineCSS(appLoadingCSS), # set in global.r

  # Initial loading screen -------------------------------------------------------------------------------------------

  div(
    id = "loading-content",
    h2("Loading...")
  ),
  hidden(
    div(
      id = "app-content",

      # Application title -----------------------------------------------------------------------------------

      titlePanel(div(HTML("DfE published data QA <h4>QA your data files before uploading to Explore Education Statistics for publication</h4>")), windowTitle = "DfE data QA"),

      # Initial guidance text -----------------------------------------------------------------------------------

      verticalLayout(
        br(),

        shinyjs::hidden(div(
          id = "guidance",
          "This app allows you to screen your data files against the Department’s ",
          a(href = "https://rsconnect/rsc/stats-production-guidance/ud.html", "underlying data standards", target = "_blank", rel = "noopener noreferrer"),
          "for Official and National statistical publications. You should make sure that any data files you intend to publish pass all of the data checks before uploading to Explore Education Statistics.",
          br(),
          br(),
          "The code for this app can be found on ",
          a(href = "https://github.com/dfe-analytical-services/dfe-published-data-qa", "GitHub", target = "_blank", rel = "noopener noreferrer"),
          ".",
          br(),
          br(),
          strong("How to use the app"),
          br(),
          "1.  Upload your data and associated metadata (.csv) files.",
          br(),
          "2.  Click to screen the files, the screener will run and display the results below. It may take a little while for the results to appear depending on the size of your file.",
          br(),
          "3.  Look through results, amend files in line with any recommendations and re-screen files if necessary.",
          br(),
          "4.  Reset the page if you have another file to check.",
          br(),
          br(),
          strong("Notes"),
          br(),
          "Currently this app works with files up to 500mb. If you have a file that is bigger than this, please contact us - ",
          a(href = "mailto:statistics.development@education.gov.uk", "statistics.development@education.gov.uk.", target = "_blank"),
          br(),
          "This app is constantly being developed, please let us know if you have any suggestions to improve it. If you experience any issues, please take screenshots and email them to us with as much information as possible.",
          hr()
        )),

        # Top panel for data uploads -----------------------------------------------------------------------------------

        wellPanel(
          tags$style(".shiny-file-input-progress {max-width: 99.8%; padding-left: 1px}"),

          fluidRow(
            column(
              5,
              fileInput(
                "datafile",
                "Upload a data file",
                multiple = FALSE,
                accept = ".csv"
              )
            ),
            column(
              5,
              fileInput(
                "metafile",
                "Upload the associated metadata file",
                multiple = FALSE,
                accept = ".csv"
              )
            ),
            column(2,
              align = "center", style = "margin-top: 25px;",
              # Only show buttons once files are added and remove button once results appear
              conditionalPanel(
                condition = "output.file_exists == true && output.showresults == false",
                uiOutput("uiScreenButton")
              ),
              shinyjs::hidden(div(
                id = "reset_button",
                uiOutput("uiResetButton")
              ))
            )
          )
        ),

        # Main panel showing results -----------------------------------------------------------------------------------

        fluidRow(

          # Loading screen that appears while tests are running -----------------------------------------------------------------------------------

          shinyjs::hidden(div(
            id = "loading",
            h4("Tests are now running against the files, this may take a few minutes depending on the size of your data file.", align = "center"),
            h4("Refresh the page if you wish to cancel the tests running.", align = "center"),
            br(),
            HTML('<center><img src="duckWaddle.gif"></center>')
          )),

          # Summarised results -----------------------------------------------------------------------------------

          shinyjs::hidden(div(
            id = "results",

            column(
              5,
              style = "padding-left:20px;",
              h4("Screening progress"),
              tags$style("#progress_stage img {max-width: 100%; max-height: 100%}"),
              imageOutput("progress_stage", height = "100%"),
              hr(),
              textOutput("testtime"),
              br(),
              textOutput("summary_text"),
              br(),
              textOutput("sum_failed_tests"),
              textOutput("sum_combined_tests"),
              textOutput("sum_passed_tests"),
              textOutput("sum_ignored_tests"),
              hr(),
              div(
                style = "word-break: break-all;",
                textOutput("datafilename"),
                textOutput("metafilename")
              ),
              hr(),
              column(
                6,
                style = "padding:0px",
                "Data file",
                br(),
                br(),
                textOutput("data_size"),
                textOutput("data_rows"),
                textOutput("data_cols")
              ),
              column(
                6,
                style = "padding:0px",
                "Metadata file",
                br(),
                br(),
                textOutput("meta_size"),
                textOutput("meta_rows"),
                textOutput("meta_cols")
              ),
            ),

            # Individual check results tables -----------------------------------------------------------------------------------

            column(
              7,
              uiOutput("ancillary_box"),
              uiOutput("passed_box"),
              uiOutput("failed_box"),
              uiOutput("advisory_box"),
              results_box(
                message = "all_tests",
                table = "table_all_tests"
              )
            )
            # End of column
          ))
          # End of summarised results div
        )
        # End of fluidRow
      )
      # End of verticalLayout
    )
  )
  # End of app content (hidden during initial load)
)
