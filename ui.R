fluidPage(
  theme = "acalat_theme.css",

  useShinyjs(),
  shinyFeedback::useShinyFeedback(),
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

            tabsetPanel(
              id = "trendy_tabs",

              tabPanel(
                title = "Screener results",
                value = "tab1",

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
                  )
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
              ),

              # QA pages -----------------------------------------------------------------------------------

              tabPanel(
                title = "File previews",
                value = "previewTab",
                style = "padding-left:20px; padding-right:20px",

                br(),
                tags$b("View metadata"),
                DTOutput("meta_table", width = "100%") %>% withSpinner(),
                hr(),
                tags$b("Preview datafile"),
                DTOutput("data_preview", width = "100%") %>% withSpinner()
              ),

              tabPanel(
                title = "What's in this file",
                value = "obUnitTab",
                style = "padding-left:20px; padding-right:20px",

                br(),
                fluidRow(
                  column(
                    3,
                    tags$b("What combinations of geography and time are in the data"),
                    br(),
                    br(),
                    "This table lists all the permutations of geographic_level and time_period where at least one row of data exists",
                    br(),
                    br(),
                    "For any unexpected Ns, check the data",
                    br(),
                    br(),
                    "For any legitimate Ns, consider if publishing this data would be worth it"
                  ),
                  column(
                    9,
                    DTOutput("geog_time_perms2", width = "100%") %>% withSpinner()
                  )
                ),
                hr(),
                fluidRow(
                  column(
                    3,
                    tags$b("What filters are present in the data"),
                    br(),
                    br(),
                    "This table lists all the filters and filter levels where at least one row of data exists",
                    br(),
                    br(),
                    "Check all levels you expect to see are there",
                    br(),
                    br(),
                    "Check the names are how you want them to look in EES",
                  ),
                  column(
                    9,
                    uiOutput("tables") %>% withSpinner()
                    # "Filter combinations missing (on trello/github issue)",
                  )
                ),
                hr(),
                fluidRow(
                  column(
                    3,
                    tags$b("What indicators are present in the data"),
                    br(),
                    br(),
                    "This table lists all the indicators that exist within the data",
                    br(),
                    br(),
                    "Check all the indicators you expect to see are there",
                    br(),
                    br(),
                    "Check the names are how you want them to look in EES"
                  ),
                  column(
                    9,
                    DTOutput("indicators", width = "60%") %>% withSpinner()
                  )
                ),
                hr(),
                fluidRow(
                  column(
                    3,
                    tags$b("Cells missing data"),
                    br(),
                    br(),
                    "This table shows how many cells do not contain data",
                    br(),
                    br(),
                    "How much of your data is unavailable, rounded to 0, not applicable or suppressed?"
                  ),
                  column(
                    9,
                    uiOutput("suppressed_cell_count") %>% withSpinner()
                  )
                ),
                hr()
                #
                # fluidRow(
                #   column(
                #     3,
                #     tags$b("Filter combinations"),
                #     br(),
                #     "The following filter combinations have no relevant data",
                #     br(),
                #     "Are there any expected filter combinations that are missing data?"
                #   ),
                #   column(
                #     8,
                #
                #     tableOutput("filterperms") %>% withSpinner()
                #   )
                # ),
                #
                # hr(),
              ),

              tabPanel(
                title = "Explore data",
                value = "indicatorsTab",
                style = "padding-left:20px; padding-right:20px",

                br(),
                fluidRow(
                  column(
                    3,
                    tags$b("Indicator summary"),
                    br(),
                    br(),
                    "Review indicator-level summaries of your data",
                    br(),
                    br(),
                    "Check to make sure values are sensible - are there any unexpected results when comparing to past time periods?"
                  ),
                  column(
                    9,
                    fluidRow(
                      column(
                        4,
                        uiOutput("indicatorChoice") %>% withSpinner(),
                      ),
                      column(
                        4,
                        uiOutput("geogChoice") %>% withSpinner()
                      ),
                      column(
                        4,
                        align = "left", style = "margin-top: 25px;",
                        actionButton(
                          inputId = "submit",
                          label = "Generate tables"
                        )
                      )
                    )
                  ),


                  fluidRow(
                    column(
                      12,
                      style = "padding-left:20px; padding-right:20px",

                      hr(),
                      uiOutput("table_list")
                    )
                  )
                )
              ),

              tabPanel(
                title = "Year-on-year changes",
                value = "outliersTab",
                style = "padding-left:20px; padding-right:20px",

                br(),
                fluidRow(
                  column(
                    3,
                    tags$b("Year-on-year summary"),
                    br(),
                    br(),
                    "Check for any large differences in data from year-to-year",
                    br(),
                    br(),
                    "Empty tables will appear for cases where there are no changes above your set threshold"
                  ),
                  column(
                    9,
                    fluidRow(
                      column(
                        4,
                        uiOutput("outlier_indicator_choice") %>% withSpinner()
                      ),
                      column(
                        4,
                        numericInputIcon(
                          inputId = "threshold_setting",
                          label = "Set percentage threshold for outlier:",
                          min = 0,
                          value = 20,
                          icon = list(NULL, icon("percent"))
                        )
                      ),
                      column(
                        4,
                        align = "left", style = "margin-top: 25px;",
                        actionButton(
                          inputId = "submit_outlier",
                          label = "Generate tables"
                        )
                      )
                    ),
                    fluidRow(
                      column(
                        4,
                        uiOutput("current_time") %>% withSpinner()
                      ),
                      column(
                        4,
                        uiOutput("comparison_time") %>% withSpinner()
                      ),
                      column(
                        4,
                        ""
                      )
                    )
                  ),


                  fluidRow(
                    column(
                      12,
                      style = "padding-left:20px; padding-right:20px",

                      hr(),
                      uiOutput("table_outlier_list")
                    )
                  )
                )
              ),

              tabPanel(
                title = "Check geography subtotals",
                value = "geogTab",
                style = "padding-left:20px; padding-right:20px",

                br(),
                fluidRow(
                  column(
                    3,
                    tags$b("Geography summary"),
                    br(),
                    br(),
                    "Check subtotals for geographies add up",
                  ),
                  column(
                    9,
                    fluidRow(
                      column(
                        4,
                        uiOutput("geog_indicator_choice") %>% withSpinner()
                      ),
                      column(
                        4,
                        ""
                      ),
                      column(
                        4,
                        align = "left", style = "margin-top: 25px;",
                        actionButton(
                          inputId = "submit_geographies",
                          label = "Generate tables"
                        )
                      )
                    )
                    # fluidRow(
                    #   column(
                    #     4,
                    #     uiOutput("geog_level_choice") %>% withSpinner()
                    #   ),
                    #   column(
                    #     4,
                    #     uiOutput("geog_sublevel_choice") %>% withSpinner()
                    #   ),
                    #   column(
                    #     4,
                    #     ''
                    #   )
                    # ),
                  ),


                  fluidRow(
                    column(
                      12,
                      style = "padding-left:20px; padding-right:20px",

                      hr(),
                      DTOutput("geog_agg2", width = "100%")
                    )
                  )
                )
              )
            ) # End of tabsetpanel
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
