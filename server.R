server <- function(input, output, session) {

  # Loading screen ----------------------------------------------------------------------------
  # Call initial loading screen

  hide(id = "loading-content", anim = TRUE, animType = "fade")
  show("app-content")

  shinyjs::showElement(id = "guidance")

  # Reactive values ----------------------------------------------------------------------------
  # Buttons and reactive values used as triggers

  values <- reactiveValues(
    shouldShow = FALSE,
    dataUploaded = FALSE,
    metaUploaded = FALSE,
    screenfailed = FALSE,
    showadvisory = FALSE,
    correctDataType = TRUE,
    correctMetaType = TRUE
  )

  # File upload check ----------------------------------------------------------------------------

  observeEvent(input$datafile, {
    values$correctDataType <- if_else(file_ext(input$datafile) == "csv", TRUE, FALSE)
    if (tail(values$correctDataType, n = 1) == FALSE) {
      values$dataUploaded <- FALSE
      shinyFeedback::feedbackWarning("datafile", !values$correctDataType, "Data files must be in comma separated values format (.csv)")
    } else {
      hideFeedback("datafile")
      values$dataUploaded <- TRUE
    }
  })

  observeEvent(input$metafile, {
    values$correctMetaType <- if_else(file_ext(input$metafile) == "csv", TRUE, FALSE)
    if (tail(values$correctMetaType, n = 1) == FALSE) {
      shinyFeedback::feedbackWarning("metafile", !values$correctMetaType, "Metadata files must be in comma separated values format (.csv)")
      values$metaUploaded <- FALSE
    } else {
      hideFeedback("metafile")
      values$metaUploaded <- TRUE
    }
  })

  output$file_exists <- reactive({
    return(values$dataUploaded && values$metaUploaded)
  })

  outputOptions(output, "file_exists", suspendWhenHidden = FALSE)

  # Screening button ----------------------------------------------------------------------------

  output$uiScreenButton <- renderUI({
    if (values$dataUploaded && values$metaUploaded) {
      actionButton("screenbutton", "Screen files", width = "80%")
    }
  })

  observeEvent(input$screenbutton, {
    shinyjs::hideElement(id = "guidance")

    # Show loading screen ---------------------------------------------------------------------------------
    shinyjs::showElement(id = "loading", anim = TRUE, animType = "fade", time = 1)

    values$shouldShow <- TRUE

    # Disable buttons ---------------------------------------------------------------------------------

    disable("uiScreenButton")
    disable(selector = "span[class='btn btn-default btn-file']")

    # Read in files ---------------------------------------------------------------------------------

    inputData <- input$datafile
    inputMeta <- input$metafile

    data <- readFile(inputData$datapath)
    meta <- readFile(inputMeta$datapath)

    # File info ---------------------------------------------------------------------------------------------------------

    isolate({

      # Output the file names
      output$datafilename <- renderText({
        paste0("Data - ", inputData$name)
      })
      output$metafilename <- renderText({
        paste0("Metadata - ", inputMeta$name)
      })

      # Date and time
      # Doing this to force the time to come out in the right time zone as the server runs on UTC
      dateTime <- Sys.time() %>% as.POSIXct(., tz = "")
      attributes(dateTime)$tzone <- "Europe/London"

      output$testtime <- renderText({
        paste0("Time - ", format(dateTime, "%H:%M, %d %b %Y"), " (Europe/London)")
      })

      # Size, rows and cols for files
      # present_file_size() and cs_num() defined in global.r
      output$data_size <- renderText({
        paste0("Size - ", present_file_size(inputData$size))
      })

      output$data_rows <- renderText({
        paste0("Rows - ", cs_num(data$mainFile %>% nrow()))
      })

      output$data_cols <- renderText({
        paste0("Columns - ", cs_num(data$mainFile %>% ncol()))
      })

      output$meta_size <- renderText({
        paste0("Size - ", present_file_size(inputMeta$size))
      })

      output$meta_rows <- renderText({
        paste0("Rows - ", cs_num(meta$mainFile %>% nrow()))
      })

      output$meta_cols <- renderText({
        paste0("Columns - ", cs_num(meta$mainFile %>% ncol()))
      })

      # File validation ---------------------------------------------------------------------------------

      screeningOutput <- screenFiles(inputData$name, inputMeta$name, data$fileSeparator, meta$fileSeparator, data$fileCharacter, meta$fileCharacter, data$mainFile, meta$mainFile)

      all_results <- screeningOutput$results

      output$progress_stage <- renderImage(
        {
          screeningOutput$progress_stage
        },
        deleteFile = FALSE
      )

      output$progress_message <- renderText({
        screeningOutput$progress_message
      })
    })

      # Summary stats ---------------------------------------------------------------------------------
      # numberActiveTests created in global.r file

      isolate({
        run_tests <- all_results %>% nrow()

        ignored_tests <- all_results %>%
          filter(result == "IGNORE") %>%
          nrow()

        failed_tests <- all_results %>%
          filter(result == "FAIL") %>%
          nrow()

        advisory_tests <- all_results %>%
          filter(result == "ADVISORY") %>%
          nrow()

        ancillary_tests <- all_results %>%
          filter(result == "ANCILLARY") %>%
          nrow()

        combined_tests <- advisory_tests + ancillary_tests

        passed_tests <- all_results %>%
          filter(result == "PASS") %>%
          nrow()

        leftover_tests <- numberActiveTests - run_tests

        output$summary_text <- renderText({
          paste0("Of all ", numberActiveTests, " tests, ", run_tests, " were successfully ran against the files, the results of these were:")
        })

        output$sum_failed_tests <- renderText({
          summarise_stats(failed_tests, "failed")
        })

        output$sum_combined_tests <- renderText({
          summarise_stats(combined_tests, "with advisory notes")
        })

        output$sum_passed_tests <- renderText({
          summarise_stats(passed_tests, "passed")
        })

        output$sum_ignored_tests <- renderText({
          summarise_stats(ignored_tests, "not applicable to the data")
        })

        # Top lines for results ---------------------------------------------------------------------------------

        output$num_failed_tests <- renderText({
          summarise_stats(failed_tests, "failed")
        })

        output$num_advisory_tests <- renderText({
          summarise_stats(advisory_tests, "with recommendations")
        })

        output$all_tests <- renderText({
          paste0("Full breakdown of the ", run_tests, " tests that were ran against the files")
        })

        # Main outputs for results ---------------------------------------------------------------------------------

        output$table_failed_tests <- renderTable(
          {
            filter(all_results, result == "FAIL") %>% select(message)
          },
          sanitize.text.function = function(x) x,
          include.colnames = FALSE
        )

        output$table_advisory_tests <- renderTable(
          {
            filter(all_results, result == "ADVISORY") %>% select(message)
          },
          sanitize.text.function = function(x) x,
          include.colnames = FALSE
        )

        output$table_all_tests <- renderTable(
          {
            all_results %>% select(message, result)
          },
          sanitize.text.function = function(x) x,
          include.colnames = FALSE
        )

        # UI blocks (result dependent) ---------------------------------------------------------------------------------

        if (failed_tests != 0) {
          output$failed_box <- renderUI({
            fail_results_box(
              message = "num_failed_tests",
              table = "table_failed_tests"
            )
          })
        }

        if (failed_tests == 0) {
          output$passed_box <- renderUI({
            pass_results_box()
          })
        }
        
      if (failed_tests == 0) {
        output$passed_box <- renderUI({
          pass_results_box()
        })
      }
      
      # Fireworks go here
      if (failed_tests == 0) {
        show_alert(
          title = "Success !!",
          type = "success",
          tags$span(
            "Your files can now be uploaded to Explore Education Statistics, see our  ",
            a(href = "https://rsconnect/rsc/stats-production-guidance/ees.html", "guidance on using EES", target = "_blank"),
            " for more information."),
          html = TRUE
        )
      }
      
      # Dynamic trendy-tabs,
      if (failed_tests == 0){
        shinyjs::show(selector = c("#trendy_tabs li a[data-value=previewTab]",
                                   "#trendy_tabs li a[data-value=obUnitTab]",
                                   "#trendy_tabs li a[data-value=filtersTab]",
                                   "#trendy_tabs li a[data-value=indicatorsTab]"))}
      else {
        shinyjs::hide(selector = c("#trendy_tabs li a[data-value=previewTab]",
                                   "#trendy_tabs li a[data-value=obUnitTab]",
                                   "#trendy_tabs li a[data-value=filtersTab]",
                                   "#trendy_tabs li a[data-value=indicatorsTab]"))}

      
      if (advisory_tests != 0) {
        output$advisory_box <- renderUI({
          advisory_results_box(
            message = "num_advisory_tests",
            table = "table_advisory_tests"
          )
        })
      }

        if (advisory_tests != 0) {
          output$advisory_box <- renderUI({
            advisory_results_box(
              message = "num_advisory_tests",
              table = "table_advisory_tests"
            )
          })
        }

        if (ancillary_tests != 0) {
          output$ancillary_box <- renderUI({
            ancillary_box()
          })
        }

        # Download all results button ---------------------------------------------------------------------------------

        output$download_results <- downloadHandler(
          filename = function() {
            paste0(
              substr(basename(inputData$name), 1, nchar(inputData$name) - 4),
              "_",
              format(dateTime, "%H%M_%d%b%Y"),
              ".csv"
            )
          },
          content = function(fname) {
            write.csv(apply(all_results %>% select(message, result), 2, as.character),
              fname,
              row.names = FALSE
            )
          }
        )
      }) # isolate

      # Hide loading screen
      shinyjs::hide(id = "loading")

      # Show results
      shinyjs::showElement(id = "results", anim = TRUE, animType = "fade", time = 1.5)

      # Show reset button
      shinyjs::showElement(id = "reset_button", anim = TRUE, animType = "fade", time = 1.5)
    })

    # Reset button ----------------------------------------------------------------------------

    output$uiResetButton <- renderUI({
      actionButton("resetbutton", "Reset page", width = "80%")
    })

    observeEvent(input$resetbutton, {

      # Hide results
      shinyjs::hideElement(id = "results")
      shinyjs::showElement(id = "guidance")
      shinyjs::hideElement(id = "qaResults")

      # clear files from input selection (does not fully reset fileInput, grr)
      reset("datafile")
      reset("metafile")

      # clear uploaded flags
      values$dataUploaded <- FALSE
      values$metaUploaded <- FALSE


      # Clear values
      values$shouldShow <- FALSE

      # enable the file upload buttons
      enable(selector = "span[class='btn btn-default btn-file disabled']")

      # set all objects to NULL
      output$datafilename <- NULL
      output$metafilename <- NULL
      output$testtime <- NULL
      output$data_size <- NULL
      output$data_rows <- NULL
      output$data_cols <- NULL
      output$meta_size <- NULL
      output$meta_rows <- NULL
      output$meta_cols <- NULL
      output$progress_stage <- NULL
      output$progress_message <- NULL
      output$failed_box <- NULL
      output$passed_box <- NULL
      output$advisory_box <- NULL
      output$ancillary_box <- NULL
      output$summary_text <- NULL
      output$sum_failed_tests <- NULL
      output$sum_combined_tests <- NULL
      output$sum_passed_tests <- NULL
      output$sum_ignored_tests <- NULL
      output$num_failed_tests <- NULL
      output$num_advisory_tests <- NULL
      output$all_tests <- NULL
      output$table_failed_tests <- NULL
      output$table_advisory_tests <- NULL
      output$table_all_tests <- NULL

      shinyjs::hideElement(id = "reset_button")
    })

    ## QA code ------------------------------------------------------------------------------

    isolate({
    # File previews ------------------------------------------------------------------------

    # Metadata preview
    output$meta_table <- renderTable({
      meta$mainFile
    })

    # Data preview
    output$data_preview <- renderTable({
      head(data$mainFile)
    })

    # Observational units ------------------------------------------------------------------

    # Time covered in the file
    output$time_coverage <- renderTable({
      data$mainFile %>% 
        select(time_period, time_identifier) %>% 
        distinct()
    })

    # Feel like this isn't doing much - maybe could we pull out the geog cols relevant to each level too?
    output$geog_coverage <- renderTable({
      unique(data$mainFile$geographic_level)
    })

    # Geography overview table
    output$geog_time_perms <- renderDataTable({
      
      ### May need some time identifier ordering? - what happens if quarters or weeks?
      
      data$mainFile %>%
        select(time_period, time_identifier, geographic_level) %>% 
        group_by(time_period, time_identifier, geographic_level) %>%
        tally() %>%
        rename(unique_locations = n) %>%
        arrange(desc(time_period), match(geographic_level, c(geography_matrix[,1]))) %>%
        pivot_wider(names_from = geographic_level, values_from = unique_locations) %>% 
        replace(is.na(.),0) %>% 
        DT::datatable()
    })

    # Filters --------------------------------------------------------------------------------
    showFilterLevels <- function(data, meta) {
      filters <- meta %>%
        filter(col_type == "Filter") %>%
        pull(col_name)

      levelsTable <- function(filter) {
        return(eval(parse(text = paste0("data %>% select(", filter, ") %>% distinct()"))))
      }

      output <- lapply(filters, levelsTable)

      return(output)
    }

    output$filters <- renderTable({
      showFilterLevels(data$mainFile, meta$mainFile)
    })

    # Indicators -----------------------------------------------------------------------------
    output$indicators <- renderTable({
      meta$mainFile %>%
        filter(col_type == "Indicator") %>%
        select(col_name)
    })

    # Add summary stats
    
    
    output$indicators <- renderTable({
      meta$mainFile %>%
        filter(col_type == "Indicator") %>%
        select(col_name)
    })
    
    output$indicator_summary_stats <- renderTable({
      
     indicators <- meta$mainFile %>% filter(col_type == "Indicator") %>% pull(col_name)
     indicator_data <-  data$mainFile %>% select(all_of(indicators))
     indicator_data %>%
     summarise(across(everything(), list(min = min, max = max))) %>% 
     pivot_longer(everything(), names_to = c("indicator", "measure"), names_pattern = "(.*)_(.*)") %>%  
     pivot_wider(names_from = "measure")
     
    })

    
  }) # isolate

  # showresults (shouldShow) ---------------------------------------------------------------------------------
  # Checking if results should show for when to show the screen button

  output$showresults <- reactive({
    return(values$shouldShow)
  })

  outputOptions(output, "showresults", suspendWhenHidden = FALSE)

  # Stop app ---------------------------------------------------------------------------------

  session$onSessionEnded(function() {
    stopApp()
  })
}
