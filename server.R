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
    showadvisory = FALSE
  )

  # File upload check ----------------------------------------------------------------------------

  observeEvent(input$datafile, {
    values$dataUploaded <- TRUE
  })

  observeEvent(input$metafile, {
    values$metaUploaded <- TRUE
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
      
      
      
      
      
      # QA page stuff
      # referring to meta and data only seem to work if the code is places in this isolate()
      # should check if it ruins any of the fancy performance improvement stuff
      
      output$meta_table <- renderTable({
        meta$mainFile
      })
      
      output$data_preview <- renderTable({
        head(data$mainFile)
      })
      
      output$geog_coverage <- renderTable({
        unique(data$mainFile$geographic_level)
      })
      
      output$time_coverage <- renderTable({
        unique(data$mainFile$time_period)
      })
      
      output$filters <- renderTable({
        meta$mainFile %>%
        filter(col_type == "Filter") %>%
        select(col_name)
      })
      
      output$indicators <- renderTable({
        meta$mainFile %>%
          filter(col_type == "Indicator") %>%
          select(col_name)
      })
      

      output$geog_time_perms <- renderTable({
        
        data$mainFile  %>% 
          count(time_period, geographic_level) %>%
          mutate(n = replace(n, n >0, "Y")) %>%
          pivot_wider(names_from = time_period, values_from = n, values_fill = "N")
        
      }) 
      
      
      
      # Checking if numbers add up
      # This is just an example based on passes_everything and will need tons of work to make sure it's flexible enough for any data file
      output$agg_check <- renderTable({ 
        
        data <- data$mainFile %>% 
          group_by(geographic_level, time_period, school_phase, school_type) %>% 
          summarise (aggregate_number = sum(enrolments)) %>% # sum(get(enrolments))) %>% 
          filter(school_type == "State-funded primary" & school_phase == "Individual") %>%
          pivot_wider(names_from = geographic_level, values_from = aggregate_number) #%>%
          #spread(key = geographic_level, value = aggregate_number) %>%
          #select(-school_type)
        
        check = function(dataset, id = "time_period"){
          years = dataset[,id]
          dataset[,id] = NULL
          dataset$match = do.call(pmax,as.list(dataset)) == do.call(pmin,as.list(dataset))
          dataset[,id] = years

          dataset <- dataset %>%
            select(time_period, everything()) %>%
            mutate(match = ifelse(match == TRUE, "MATCH", "NO MATCH"))

          return(dataset)
        }

        output_data <- check(data)

        return(output_data)
        
      }) 
      
      
      ## Scatter plot
      
      
      output$testing <- renderTable({ 
        
        latest_year_data <- data$mainFile  %>% 
          filter(geographic_level == "Local authority" & school_type == "State-funded primary" & school_phase == "Individual") %>%  
          dplyr::select(time_period,la_name,enrolments) %>%
          filter(time_period == 201718)
        
        var_column_title_latest <- colnames(latest_year_data)[1] 
        
        previous_year_data <- data$mainFile  %>% 
          filter(geographic_level == "Local authority" & school_type == "State-funded primary" & school_phase == "Individual") %>%  
          dplyr::select(time_period,la_name,enrolments) %>%
          filter(time_period == 201617)
        
        
        var_column_title_previous <- colnames(previous_year_data)[1]
        
       merge(x = latest_year_data , y =  previous_year_data , by = c("la_name"), all = TRUE)
        
        
      })
      
      output$scatter <- renderPlot({ 
        

        latest_year_data <- data$mainFile  %>% 
          filter(geographic_level == "Local authority") %>%  # & school_type == "State-funded primary" & school_phase == "Individual") %>%  
          dplyr::select(time_period,la_name,enrolments, school_type) %>%
          filter(time_period == 201718)
        
        var_column_title_latest <- colnames(latest_year_data)[1] 
        
        previous_year_data <- data$mainFile  %>% 
          filter(geographic_level == "Local authority") %>%  # & school_type == "State-funded primary" & school_phase == "Individual") %>%  
          dplyr::select(time_period,la_name,enrolments, school_type) %>%
          filter(time_period == 201617)
        
        
        var_column_title_previous <- colnames(previous_year_data)[1]
        
        multiple_year_data <- merge(x = latest_year_data , y =  previous_year_data , by = c("la_name", "school_type"), all = TRUE)
        
      
        ggplot(multiple_year_data, aes(x= enrolments.x, #get(var_column_title_latest), 
                                         y=enrolments.y, #get(var_column_title_previous), 
                                         color=factor(school_type),
                                         text = paste("LA name:", la_name, "<br>",
                                                      "Latest year data:", enrolments.x, "<br>",
                                                      "Previous year data:", enrolments.y, "<br>"))) +
            # xlab(var_column_title_latest) +
            # ylab(var_column_title_previous) +
            geom_point()  +
            geom_abline(intercept = 0, slope = 1,color="black",linetype = "dashed") +
            scale_y_continuous(limits = c(0, max(multiple_year_data[, enrolments.y]))) +
            scale_x_continuous(limits = c(0, max(multiple_year_data[, enrolments.x]))) +
            theme_minimal() 
          
        
      })
      
      
      
      # Geog
      # Time
      # Filters
      # Indicators
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      output$data_cols3 <- renderText({
        paste0("Columns - ", cs_num(meta$mainFile %>% ncol()))
      })
      
      
    }) # isolate

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
  
  
  
  # QA further page ----------------------------------------------------------------------------
  
  observeEvent(input$qaFurther, {
    
    shinyjs::hideElement(id = "results")
    
    shinyjs::hideElement(id = "reset_button")
    
    shinyjs::showElement(id = "qaResults")
    
    shinyjs::showElement(id = "backToScreener")
    
  })
  
  observe({
    toggle(condition = input$runQA, 
           selector = c("#navbar li a[data-value=tab2]",
                        "#navbar li a[data-value=tab3]")
    )
  })
  
  observeEvent(input$goback, {
    
    shinyjs::hideElement(id = "backToScreener")
    
    shinyjs::showElement(id = "reset_button")
    
    shinyjs::showElement(id = "results")
    
  })
  
  # Coverage of data 
  # Need a better way of accessing the data than this, I tried just referring to meta/data and it couldn't find it
  
  # output$meta_table <- renderTable({
  #   
  #   req(input$metafile)
  #   read.csv(input$metafile$datapath)
  #   
  # })
  # 
  # 
  # output$data_preview <- renderTable({
  #   
  #   req(input$metafile)
  #   head(read.csv(input$datafile$datapath))
  #   
  # })
  # 
  # # output$geog_coverage <- renderTable({
  # #   
  # #   req(input$metafile)
  # #   data <- read.csv(input$datafile$datapath)
  # #   
  # # })
  #   
  # 
  # output$geog_coverage <- renderTable({
  #   distinct(meta$mainFile$indicator_dp)
  # })
  # 
  # output$testing <- renderTable({
  #   meta$mainFile %>% ncol()
  # })
  # 
  # 
  # output$data_cols2 <- renderText({
  #   paste0("Columns - ", cs_num(data$mainFile %>% ncol()))
  # })
  # 
  # 
  # # Geog
  # # Time
  # # Filters
  # # Indicators
  
  
  
  
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
