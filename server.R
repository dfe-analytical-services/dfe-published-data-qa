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


      # Summary stats ---------------------------------------------------------------------------------
      # numberActiveTests created in global.r file

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
            " for more information."
          ),
          html = TRUE
        )
      }

      # Dynamic trendy-tabs,
      if (failed_tests == 0) {
        shinyjs::show(selector = c(
          "#trendy_tabs li a[data-value=previewTab]",
          "#trendy_tabs li a[data-value=obUnitTab]",
          "#trendy_tabs li a[data-value=indicatorsTab]",
          "#trendy_tabs li a[data-value=outliersTab]",
          "#trendy_tabs li a[data-value=geogTab]"
        ))
      }
      else {
        shinyjs::hide(selector = c(
          "#trendy_tabs li a[data-value=previewTab]",
          "#trendy_tabs li a[data-value=obUnitTab]",
          "#trendy_tabs li a[data-value=indicatorsTab]",
          "#trendy_tabs li a[data-value=outliersTab]",
          "#trendy_tabs li a[data-value=geogTab]"
        ))
      }


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


      ## QA code -----------------------------------------------------------------------------

      # File previews ----------------------------------------------------------------


      # Set striping to be "off" for data tables
      rowCallback <- c(
        "function(row, data, num, index){",
        "  var $row = $(row);",
        "    $row.css('background-color', '#454b51');",
        "    $row.hover(function(){",
        "      $(this).css('background-color', '#6a737c');",
        "     }, function(){",
        "      $(this).css('background-color', '#454b51');",
        "     }",
        "    );",
        "}"
      )

      # Metadata preview
      output$meta_table <- DT::renderDT({
        datatable(meta$mainFile,
          rownames = FALSE,
          style = "bootstrap",
          options = list(
            dom = "pt",
            rowCallback = JS(rowCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#232628', 'color': '#c8c8c8'});",
              "}"
            )
          )
        )
      })

      # Data preview
      output$data_preview <- DT::renderDT({
        datatable(data$mainFile,
          rownames = FALSE,
          style = "bootstrap",
          options = list(
            dom = "pt",
            rowCallback = JS(rowCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#232628', 'color': '#c8c8c8'});",
              "}"
            ),
            scrollX = TRUE
          )
        )
      })


      # Geog / time permutations -----------------------------------------------------

      output$geog_time_perms2 <- renderTable({
        data$mainFile %>%
          count(time_period, geographic_level) %>%
          mutate(n = replace(n, n > 0, "Y")) %>%
          pivot_wider(names_from = time_period, values_from = n, values_fill = "N")
      })
      # Filter permutations -----------------------------------------------------

      # output$filterperms <- renderTable({
      #
      #   filters <- meta$mainFile %>%
      #     dplyr::filter(col_type == "Filter") %>%
      #     pull(col_name)
      #
      #   total_info<-data.frame(filters)
      #
      #   for (filter in all_of(filters)) {
      #     info <- data$mainFile %>%
      #       select(filter) %>%
      #       distinct()
      #
      #     total_info <- total_info %>%  merge(info)}
      #
      #
      #   total_info <- total_info %>%
      #     select(-filters) %>%
      #     distinct()
      #
      #   #Get list of publication-specific filters
      #   publication_filters <- meta$mainFile %>%
      #     filter(col_type == "Filter") %>%
      #     select(col_name) %>%
      #     pull(col_name)
      #
      #   #Get filter group combos for publication-specific filters
      #   distinct_filter_groups <- data$mainFile %>%
      #     select(all_of(publication_filters)) %>%
      #     distinct() %>% mutate(flag=1)
      #
      #   total_info %>% left_join(distinct_filter_groups) %>%
      #     filter(is.na(flag)) %>%
      #     select(-flag)
      #
      # })
      #


      # Show filters and associated levels from the data -----------------------------

      showFilterLevels <- function(meta) {
        filters <- meta %>%
          dplyr::filter(col_type == "Filter") %>%
          pull(col_name)

        filter_groups <- meta %>%
          dplyr::filter(col_type == "Filter") %>%
          dplyr::filter(filter_grouping_column != "") %>%
          dplyr::filter(!is.na(filter_grouping_column)) %>%
          select(col_name, filter_grouping_column)


        levelsTable <- function(filter) {
          filter_group <- filter_groups %>%
            dplyr::filter(col_name == filter) %>%
            pull(filter_grouping_column)

          if (!is.na(filter_group)) {
            return(eval(parse(text = paste0("data$mainFile %>% select(", filter_group, ", ", filter, ") %>% distinct() %>% arrange(", filter_group, ", ", filter, ")"))))
          }
          else {
            return(eval(parse(text = paste0("data$mainFile %>% select(", filter, ") %>% distinct() %>% arrange(", filter, ")"))))
          }
        }


        output <- lapply(filters, levelsTable)

        return(output)
      }

      output$tables <- renderUI({
        myList <- showFilterLevels(meta$mainFile)

        if(length(myList) == 0){
          "There are no filters in this file"
        } else {
        
        tableList <- purrr::imap(myList, ~ {
          tagList(
            h4(.y),
            tableOutput(outputId = paste0("table_", .y))
          )
        })

        purrr::iwalk(myList, ~ {
          output_name <- paste0("table_", .y)
          output[[output_name]] <- renderTable(.x)
        })

        tagList(tableList)
        }
      })


      # Indicator summaries-------------------------------------------------------------

      output$indicators <- renderTable({
        meta$mainFile %>%
          filter(col_type == "Indicator") %>%
          select(col_name, label)
      })


      # Create geographic level choice depending on what's available

      output$geogChoice <- renderUI({
        selectInput(
          inputId = "geog_parameter",
          label = "Choose geographic level(s):",
          choices = data$mainFile %>% pull(geographic_level) %>% unique(),
          multiple = TRUE
        )
      })

      # Create indicator choice depending on what's available

      output$indicatorChoice <- renderUI({
        selectInput(
          inputId = "ind_parameter",
          label = "Choose indicator(s):",
          choices = meta$mainFile %>% filter(col_type == "Indicator") %>% pull(col_name),
          multiple = TRUE
        )
      })


      # Show summary stats table for an indicator
      showsumstats <- function(parameter, geog_parameter) {
        args <- expand.grid(ind = parameter, geog = geog_parameter, stringsAsFactors = FALSE)
            
            sumtable <- function(args) {
              return(eval(parse(text = paste0("data$mainFile %>% filter(geographic_level =='", args[2], "') %>% 
          mutate(across(all_of('", args[1], "'), na_if, 'c')) %>%
          mutate(across(all_of('", args[1], "'), na_if, 'z')) %>%
          mutate(across(all_of('", args[1], "'), na_if, ':')) %>%
          mutate(across(all_of('", args[1], "'), na_if, '~')) %>%
          mutate(across(all_of('", args[1], "'), as.numeric)) %>%
          select(time_period,'", args[1], "') %>%
          group_by(time_period) %>%
          summarise(across(everything(), list(min = ~ min(.x, na.rm=TRUE), 
                                              max = ~ max(.x, na.rm=TRUE), 
                                              average = ~ mean(.x, na.rm=TRUE), 
                                              count = ~ n(), 
                                              suppressed = ~ sum(is.na(.x))))) %>%
          pivot_longer(!time_period, names_to = c('indicator', 'measure'), names_pattern = '(.*)_(.*)') %>%
          pivot_wider(names_from = 'time_period') %>%
          mutate(geographic_level ='", args[2], "', .before = indicator)"))))
            }
            
            output <- apply(args, 1, sumtable)
        
        return(output)
      }

      # create a list of tables - with one for each indicator summary
      theList <- eventReactive(input$submit, {
        validate(
          need(input$ind_parameter != "", "Please select at least one indicator"),
          need(input$geog_parameter != "", "Please select at least one geographic level")
        )
        
        return(showsumstats(input$ind_parameter, input$geog_parameter))
      })


      # Create and then output the tables
      observeEvent(input$submit, {
        req(theList())
        
        purrr::iwalk(theList(), ~ {
          names <- paste0("t_", .y)
          output[[names]] <- renderTable(.x)
        })
      })

      output$table_list <- renderUI({
        req(theList())

        t_list <- purrr::imap(theList(), ~ {
          tagList(
            h4(.y),
            tableOutput(paste0("t_", .y))
          )
        })
        tagList(t_list)
      })

      # Outliers ----------------------------------------------------------------------

      # Give users choice of indicators
      output$outlier_indicator_choice <- renderUI({
        selectInput(
          inputId = "outlier_indicator_parameter",
          label = "Choose indicator(s):",
          choices = meta$mainFile %>% filter(col_type == "Indicator") %>% select(col_name),
          multiple = TRUE
        )
      })

      # Select "current time"
      output$current_time <- renderUI({
        selectInput(
          inputId = "ctime_parameter",
          label = "Choose current time period:",
          choices = data$mainFile %>% arrange(desc(time_period)) %>% pull(time_period) %>% unique(),
          multiple = FALSE
        )
      })

      # Select "comparison time"
      output$comparison_time <- renderUI({
        selectInput(
          inputId = "comptime_parameter",
          label = "Choose comparison time period:",
          choices = data$mainFile %>% arrange(desc(time_period)) %>% pull(time_period) %>% unique(),
          multiple = FALSE
        )
      })


      # get outlier stats

      get_outliers <- function(outlier_indicator_parameter, threshold_setting, ctime_parameter, comptime_parameter) {
        args <- expand.grid(meas = outlier_indicator_parameter, thresh = threshold_setting, current = ctime_parameter, comparison = comptime_parameter, stringsAsFactors = FALSE)


        # Get list of indicators
        indicators <- meta$mainFile %>%
          filter(col_type == "Indicator") %>%
          pull(col_name)

        # Get list of filters
        filters <- data$mainFile %>%
          select(-indicators) %>%
          names()


        outliertable <- function(args) {
          return(eval(parse(text = paste0("data$mainFile %>% group_by(time_period,geographic_level) %>% 
          mutate(", args[1], "== as.numeric(", args[1], ")) %>% 
          filter(time_period %in% c(", args[4], ",", args[3], ")) %>% 
          select(all_of(filters),", args[1], ") %>% 
          spread(time_period,", args[1], ") %>% 
          mutate(thresh_indicator_big = as.numeric(`", args[4], "`) * (1+(", args[2], "/100)),
                 thresh_indicator_small = as.numeric(`", args[4], "`) * (1-(", args[2], "/100)),
                 outlier_large = as.numeric(`", args[3], "`) >= thresh_indicator_big,
                 outlier_small = as.numeric(`", args[3], "`) <= thresh_indicator_small) %>%
          select(-thresh_indicator_big,-thresh_indicator_small) %>%
          filter(as.numeric(`", args[3], "`) >= 5 | as.numeric(`", args[4], "`) >= 5) %>%  
          filter(outlier_large == TRUE | outlier_small ==TRUE)"))))
        }

        output <- apply(args, 1, outliertable)


        return(output)
      }

      # create a list of tables - with one for each indicator summary
      theOutlierList <- eventReactive(input$submit_outlier, {
        validate(
          need(input$outlier_indicator_parameter != "", "Please select at least one indicator"),
          need(input$ctime_parameter != input$comptime_parameter, "Please select two different time periods for comparison")
        )
        
        return(get_outliers(input$outlier_indicator_parameter, input$threshold_setting, input$ctime_parameter, input$comptime_parameter))
      })


      # Create and then output the tables
      observeEvent(input$submit_outlier, {
        req(theOutlierList())

        purrr::iwalk(theOutlierList(), ~ {
          names <- paste0("to_", .y)
          output[[names]] <- DT::renderDT(server = FALSE, {
            datatable(.x,
              rownames = FALSE,
              style = "bootstrap",
              extensions = "Buttons",
              options = list(
                dom = "Bptl",
                buttons = c("csv", "copy", "colvis"),
                rowCallback = JS(rowCallback),
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#232628', 'color': '#c8c8c8'});",
                  "}"
                ),
                scrollX = TRUE
              )
            )
          })
        })
      })

      output$table_outlier_list <- renderUI({
        req(theOutlierList())

        to_list <- purrr::imap(theOutlierList(), ~ {
          tagList(
            h4(.y),
            DTOutput(paste0("to_", .y), width = 1400) %>% withSpinner()
          )
        })
        tagList(to_list)
      })


      # check geog aggregations -------------------------------------------------------

      # Create lookup for geographic level and corresponding filters in data

      geog_level <- c(
        "National", "Regional", "Local authority", "Local authority district",
        "RSC region", "Parliamentary constituency", "Local enterprise partnership",
        "English devolved area", "Opportunity area", "Ward"
      )

      geog_level_label <- c(
        "country_name", "region_name", "la_name", "lad_name",
        "rsc_region_lead_name", "pcon_name", "local_enterprise_partnership_name",
        "english_devolved_area_name", "opportunity_area_name", "ward_name"
      )


      geog_lookup_labels <- data.frame(geog_level, geog_level_label)

      # Give users choice of indicators
      output$geog_indicator_choice <- renderUI({
        selectInput(
          inputId = "geog_indicator_parameter",
          label = "Choose indicator(s):",
          choices = meta$mainFile %>% filter(col_type == "Indicator") %>% select(col_name),
          multiple = TRUE
        )
      })

      # Give users choice of top level geog
      output$geog_level_choice <- renderUI({
        selectInput(
          inputId = "geog_level_parameter",
          label = "Choose geographic level (totals):",
          choices = data$mainFile %>% select(geographic_level) %>% distinct(),
          multiple = FALSE
        )
      })

      # Give users choice of sublevel geog
      output$geog_sublevel_choice <- renderUI({
        selectInput(
          inputId = "geog_sublevel_parameter",
          label = "Choose geographic level (subtotals):",
          choices = data$mainFile %>% filter(geographic_level != "National") %>% select(geographic_level) %>% distinct(),
          multiple = FALSE
        )
      })


      # get geog comparison

      get_geog_comparison <- function(geog_indicator_parameter, geog_level_parameter, geog_sublevel_parameter) {
        args <- expand.grid(meas = geog_indicator_parameter, geog_level = geog_level_parameter, geog_sublevel = geog_sublevel_parameter, stringsAsFactors = FALSE) %>%
          left_join(geog_lookup_labels)

        # Get list of publication - specific filters
        publication_filters <- meta$mainFile %>%
          filter(col_type == "Filter") %>%
          pull(col_name)

        geogtable <- function(args) {
          return(eval(parse(text = paste0("data$mainFile  %>% 
          filter(geographic_level == '", args[2], "') %>% 
          select(time_period,", args[4], ",all_of(publication_filters),", args[1], ") %>% 
          mutate(", args[1], " = as.numeric(", args[1], ")) %>% 
          arrange(time_period,get(publication_filters),", args[4], ")"))))
        }

        geogtable_sublevel <- function(args) {
          return(eval(parse(text = paste0("data$mainFile  %>% 
          filter(geographic_level == '", args[3], "') %>% 
          select(time_period,", args[4], ",all_of(publication_filters),", args[1], ") %>% 
          group_by(across(-c(", args[1], "))) %>% 
          summarise(", args[1], "= sum(as.numeric(", args[1], ",na.rm = TRUE))) %>% 
          ungroup() %>% 
          arrange(time_period,get(publication_filters),", args[4], ")"))))
        }

        output_total <- apply(args, 1, geogtable)

        output_subtotal <- apply(args, 1, geogtable_sublevel)

        output <- mapply(setdiff, output_total, output_subtotal, SIMPLIFY = FALSE)

        return(output)
      }

      # create a list of tables - with one for each indicator summary
      theGeographyList <- eventReactive(input$submit_geographies, {
        return(get_geog_comparison(input$geog_indicator_parameter, input$geog_level_parameter, input$geog_sublevel_parameter))
      })


      # Create and then output the tables
      observeEvent(input$submit_geographies, {
        req(theGeographyList())

        purrr::iwalk(theGeographyList(), ~ {
          names <- paste0("tg_", .y)
          output[[names]] <- DT::renderDT(server = FALSE, {
            datatable(.x,
              rownames = FALSE,
              style = "bootstrap",
              extensions = "Buttons",
              options = list(
                dom = "Bptl",
                buttons = c("csv", "copy", "colvis"),
                rowCallback = JS(rowCallback),
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#232628', 'color': '#c8c8c8'});",
                  "}"
                ),
                scrollX = TRUE
              )
            )
          })
        })
      })

      output$table_geography_list <- renderUI({
        req(theGeographyList())

        tg_list <- purrr::imap(theGeographyList(), ~ {
          tagList(
            h4(.y),
            DTOutput(paste0("tg_", .y), width = 1400) %>% withSpinner()
          )
        })
        tagList(tg_list)
      })


      # supressed cells ---------------------------------------------------------------

      observe({
        indicators <- meta$mainFile %>%
          dplyr::filter(col_type == "Indicator") %>%
          pull(col_name)

        total_indicator_count <- data$mainFile %>%
          select(all_of(indicators)) %>%
          unlist() %>%
          table() %>%
          as.data.frame() %>%
          summarise(sum(Freq)) %>%
          as.numeric()


        suppress_count <- data$mainFile %>%
          select(all_of(indicators)) %>%
          unlist() %>%
          table() %>%
          as.data.frame() %>%
          filter(. %in% c("z", "c", ":", "~")) %>%
          mutate(Perc = roundFiveUp(Freq / total_indicator_count * 100, 1))

        names(suppress_count) <- c("Symbol", "Frequency", "% of total cell count")


        output$suppressed_cell_count <- renderUI({
          if (nrow(suppress_count) == 0) {
            return(strong("No cells are suppressed"))
          }

          tableOutput("suppressed_cell_count_table")
        })

        output$suppressed_cell_count_table <- renderTable({
          suppress_count
        })
      })
    }) # end of isolate



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

    # Hide loading screen
    shinyjs::hide(id = "loading")

    # Show results
    shinyjs::showElement(id = "results", anim = TRUE, animType = "fade", time = 1.5)

    # Show reset button
    shinyjs::showElement(id = "reset_button", anim = TRUE, animType = "fade", time = 1.5)

    # Select the screening report tab panel

    updateTabsetPanel(session, "trendy_tabs", selected = "tab1")
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
