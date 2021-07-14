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
    correctMetaType = TRUE,
    datafile = NULL,
    metafile = NULL,
    clear = FALSE,
    environment = config::get("environment") # use "shinyapps" to test alternative behaviour locally
  )

  # File upload check ----------------------------------------------------------------------------

  observe({
    req(input$datafile)
    req(input$metafile)
    req(!values$clear)

    values$datafile <- input$datafile
    values$metafile <- input$metafile
  })

  # observeEvent(input$datafile, {
  #   values$clear <- FALSE
  # }, priority = 1000)
  #
  # observeEvent(input$metafile, {
  #   values$clear <- FALSE
  # }, priority = 1000)
  #

  observeEvent(input$datafile,
    {
      values$clear <- FALSE
      values$correctDataType <- if_else(file_ext(input$datafile) == "csv", TRUE, FALSE)
      if (tail(values$correctDataType, n = 1) == FALSE) {
        values$dataUploaded <- FALSE
        shinyFeedback::feedbackWarning("datafile", !values$correctDataType, "Data files must be in comma separated values format (.csv)")
      } else {
        hideFeedback("datafile")
        values$dataUploaded <- TRUE
      }
    },
    priority = 1000
  )

  observeEvent(input$metafile,
    {
      values$clear <- FALSE
      values$correctMetaType <- if_else(file_ext(input$metafile) == "csv", TRUE, FALSE)
      if (tail(values$correctMetaType, n = 1) == FALSE) {
        shinyFeedback::feedbackWarning("metafile", !values$correctMetaType, "Metadata files must be in comma separated values format (.csv)")
        values$metaUploaded <- FALSE
      } else {
        hideFeedback("metafile")
        values$metaUploaded <- TRUE
      }
    },
    priority = 1000
  )

  output$file_exists <- reactive({
    return(values$dataUploaded && values$metaUploaded)
  })

  outputOptions(output, "file_exists", suspendWhenHidden = FALSE)

  # Screening button ----------------------------------------------------------------------------

  output$uiScreenButton <- renderUI({
    if (values$dataUploaded && values$metaUploaded) {
      if (values$environment == "shinyapps") {
        actionButton("external_screenbutton", "Screen files", width = "80%")
      } else {
        actionButton("screenbutton", "Screen files", width = "80%")
      }
    }
  })

  # Confirmation modal for external use ----------------------------------------------------------

  observeEvent(input$external_screenbutton, {
    shinyalert::shinyalert(
      title = "",
      text = "This is the external version of the DfE data screening app. We do not control the servers that this app runs on. <br><br><strong> You should only use this to screen published or dummy data. </strong><br><br> For sensitive files, you should clone the repository and screen files locally instead.",
      type = "warning",
      closeOnEsc = FALSE,
      closeOnClickOutside = FALSE,
      html = TRUE,
      showConfirmButton = TRUE,
      showCancelButton = TRUE,
      confirmButtonText = "I understand, proceed with screening files",
      confirmButtonCol = "#AEDEF4",
      cancelButtonText = "Cancel screening",
      animation = TRUE,
      size = "s",
      callbackR = function(x) {
        if (x == TRUE) {
          values$proceed_with_screening <- x
        } else {
          if (x == FALSE) {
            # Clear uploaded files (well, partly)
            shinyjs::reset("datafile")
            shinyjs::reset("metafile")

            # clear uploaded flags
            values$dataUploaded <- FALSE
            values$metaUploaded <- FALSE

            # Clearing something that looks like the right thing?
            values$datafile <- NULL
            values$metafile <- NULL
          }
        }
      },
      immediate = FALSE
    )
  })

  # Main screening button ------------------------------------------------------------------------

  observeEvent(input$screenbutton | values$proceed_with_screening,
    {
      if (input$screenbutton == 0 && is.null(values$proceed_with_screening)) {

        # This ends it early
        return()
      }

      shinyjs::hideElement(id = "guidance")

      # Show loading screen ---------------------------------------------------------------------------------
      shinyjs::showElement(id = "loading", anim = TRUE, animType = "fade", time = 1)

      values$shouldShow <- TRUE

      # Disable buttons ---------------------------------------------------------------------------------

      disable("uiScreenButton")
      disable(selector = "span[class='btn btn-default btn-file']")

      # Read in files ---------------------------------------------------------------------------------

      inputData <- values$datafile
      inputMeta <- values$metafile

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
      if (failed_tests == 0 & data$mainFile %>%
        select(geographic_level) %>%
        distinct() %>%
        nrow() > 1) {
        shinyjs::show(selector = c(
          "#trendy_tabs li a[data-value=previewTab]",
          "#trendy_tabs li a[data-value=obUnitTab]",
          "#trendy_tabs li a[data-value=indicatorsTab]",
          "#trendy_tabs li a[data-value=outliersTab]",
          "#trendy_tabs li a[data-value=geogTab]"
        ))
      }
      else if (failed_tests == 0 & data$mainFile %>%
        select(geographic_level) %>%
        distinct() %>%
        nrow() == 1) {
        shinyjs::show(selector = c(
          "#trendy_tabs li a[data-value=previewTab]",
          "#trendy_tabs li a[data-value=obUnitTab]",
          "#trendy_tabs li a[data-value=indicatorsTab]",
          "#trendy_tabs li a[data-value=outliersTab]"
        ))

        shinyjs::hide(selector = c(
          "#trendy_tabs li a[data-value=geogTab]"
        ))
      } else {
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

        if (failed_tests == 0) {

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
              class = "table-bordered",
              options = list(
                dom = "pt",
                ordering = F,
                pageLength = 12,
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
              class = "table-bordered",
              options = list(
                dom = "pt",
                pageLength = 12,
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

          # output$geog_time_perms2 <- renderTable({
          #   data$mainFile %>%
          #     count(time_period, geographic_level) %>%
          #     mutate(n = replace(n, n > 0, "Y")) %>%
          #     pivot_wider(names_from = time_period, values_from = n, values_fill = "N")
          # })

          output$geog_time_perms2 <- DT::renderDT({
            table <- data$mainFile %>%
              count(time_period, geographic_level) %>%
              mutate(n = replace(n, n > 0, "Y")) %>%
              pivot_wider(names_from = time_period, values_from = n, values_fill = "N")

            datatable(table,
              rownames = FALSE,
              style = "bootstrap",
              class = "table-bordered",
              options = list(
                dom = "t",
                ordering = F,
                # rowCallback = JS(rowCallback),
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#232628', 'color': '#c8c8c8'});",
                  "}"
                )
              )
            )
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
              if (nrow(filter_groups) > 0) {
                filter_group <- filter_groups %>%
                  dplyr::filter(col_name == filter) %>%
                  pull(filter_grouping_column)

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

            if (length(myList) == 0) {
              "There are no filters in this file"
            } else {
              tableList <- purrr::imap(myList, ~ {
                tagList(
                  h4(paste("Filter ", .y)),
                  # tableOutput(outputId = paste0("table_", .y))
                  DTOutput(outputId = paste0("table_", .y), width = "60%") %>% withSpinner()
                )
              })

              purrr::iwalk(myList, ~ {
                output_name <- paste0("table_", .y)
                # output[[output_name]] <- renderTable(.x)

                output[[output_name]] <- DT::renderDT(datatable(.x,
                  rownames = FALSE,
                  style = "bootstrap",
                  class = "table-bordered",
                  options = list(
                    dom = "t",
                    ordering = F,
                    # rowCallback = JS(rowCallback),
                    initComplete = JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({'background-color': '#232628', 'color': '#c8c8c8'});",
                      "}"
                    )
                  )
                ))
              })

              tagList(tableList)
            }
          })




          # Indicator summaries-------------------------------------------------------------

          output$indicators <- DT::renderDT({
            table <- meta$mainFile %>%
              filter(col_type == "Indicator") %>%
              select(col_name, label)

            datatable(table,
              rownames = FALSE,
              style = "bootstrap",
              class = "table-bordered",
              options = list(
                dom = "t",
                ordering = F,
                # rowCallback = JS(rowCallback),
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#232628', 'color': '#c8c8c8'});",
                  "}"
                )
              )
            )
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
              y <- eval(parse(text = paste0("data$mainFile %>% filter(geographic_level =='", args[2], "') %>% 
          mutate(across(all_of('", args[1], "'), na_if, 'c')) %>%
          mutate(across(all_of('", args[1], "'), na_if, 'z')) %>%
          mutate(across(all_of('", args[1], "'), na_if, ':')) %>%
          mutate(across(all_of('", args[1], "'), na_if, '~')) %>%
          mutate(across(all_of('", args[1], "'), as.numeric)) %>%
          select(time_period,'", args[1], "') %>%
          group_by(time_period) %>%
          summarise(across(everything(), list(min = ~ min(.x, na.rm=TRUE), 
                                              max = ~ max(.x, na.rm=TRUE), 
                                              average = ~ round(mean(.x, na.rm=TRUE),1), 
                                              count = ~ n(), 
                                              suppressed = ~ sum(is.na(.x))))) %>%
          pivot_longer(!time_period, names_to = c('indicator', 'measure'), names_pattern = '(.*)_(.*)') %>%
          pivot_wider(names_from = 'time_period') %>%
          mutate(geographic_level ='", args[2], "', .before = indicator) %>% 
          mutate(Change = as.character(0))")))

              for (i in 1:nrow(y)) {
                y[i, ncol(y)] <- (paste(y[i, 4:(ncol(y) - 1)], sep = "", collapse = ","))
              }

              y$Change <- str_replace_all(y$Change, "x", "0")

              return(y)
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
            output$table_list <- renderUI({
              req(theList())

              t_list <- purrr::imap(theList(), ~ {
                tagList(
                  h4(.y),
                  DTOutput(outputId = paste0("t_", .y), width = "100%") %>% withSpinner()
                )
              })

              purrr::iwalk(theList(), ~ {
                output_name <- paste0("t_", .y)

                output[[output_name]] <- DT::renderDT(server = FALSE, {
                  cd <- list(list(targets = ncol(.x) - 1, render = JS("function(data, type, full){ return '<span class=sparkSamples>' + data + '</span>' }")))

                  cb <- JS(paste0("function (oSettings, json) {\n  $('.sparkSamples:not(:has(canvas))').sparkline('html', { type: 'line', lineColor: '#c8c8c8', fillColor: '#e87421', width: '200px', height: '40px'});\n}"), collapse = "")

                  dt <- datatable(.x,
                    rownames = FALSE,
                    style = "bootstrap",
                    class = "table-bordered",
                    options = list(
                      columnDefs = cd,
                      fnDrawCallback = cb,
                      rowCallback = JS(rowCallback),
                      dom = "t",
                      ordering = F,
                      initComplete = JS(
                        "function(settings, json) {",
                        "$(this.api().table().header()).css({'background-color': '#232628', 'color': '#c8c8c8'});",
                        "}"
                      )
                    )
                  )

                  dt$dependencies <- append(dt$dependencies, htmlwidgets:::getDependency("sparkline"))

                  dt
                })
              })
              tagList(t_list)
            })
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
            selected = data$mainFile %>% select(time_period) %>% arrange(desc(time_period)) %>% distinct() %>% slice(2) %>% pull(time_period),
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
          filter(as.numeric(`", args[3], "`) >= 5 | as.numeric(`", args[4], "`) >= 5) %>%  
          filter(outlier_large == TRUE | outlier_small ==TRUE) %>% 
          select(-thresh_indicator_big,-thresh_indicator_small,-time_identifier,-outlier_large,-outlier_small)"))))
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
            output$table_outlier_list <- renderUI({
              req(theOutlierList())

              to_list <- purrr::imap(theOutlierList(), ~ {
                tagList(
                  h4(.y),
                  DTOutput(outputId = paste0("to_", .y), width = "100%") %>% withSpinner()
                )
              })

              purrr::iwalk(theOutlierList(), ~ {
                names <- paste0("to_", .y)
                output[[names]] <- DT::renderDT(server = FALSE, {
                  datatable(.x,
                    rownames = FALSE,
                    style = "bootstrap",
                    extensions = "Buttons",
                    class = "table-bordered",
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
              tagList(to_list)
            })
          })


          # check geog aggregations -------------------------------------------------------

          #       # Give users choice of indicators
          output$geog_indicator_choice <- renderUI({
            selectizeInput(
              inputId = "geog_indicator_parameter",
              label = "Choose indicator(s):",
              choices = meta$mainFile %>% filter(col_type == "Indicator") %>% select(col_name),
              options = list(
                placeholder = "Please select an indicator",
                onInitialize = I('function() { this.setValue(""); }')
              )
            )
          })

          data_geog <- eventReactive(input$submit_geographies, {
            validate(
              need(input$geog_indicator_parameter != "", "Please select an indicator")
            )

            pf <- meta$mainFile %>%
              filter(col_type == "Filter") %>%
              pull(col_name)

            cf <- paste(pf, collapse = ", ")

            ii <- input$geog_indicator_parameter # "number_of_pupils"

            eval(parse(text = paste0("data$mainFile %>%
                            mutate(across(all_of('", ii, "'), na_if, 'c')) %>%
                            mutate(across(all_of('", ii, "'), na_if, 'z')) %>%
                            mutate(across(all_of('", ii, "'), na_if, ':')) %>%
                            mutate(across(all_of('", ii, "'), na_if, '~')) %>%
                            mutate(across(all_of('", ii, "'), as.numeric)) %>%
                            group_by(time_period,geographic_level,", cf, ") %>%
                            summarise(aggregate_number = sum(", ii, ")) %>%
                            spread(key = geographic_level, value = aggregate_number)")))
          })




          observeEvent(input$submit_geographies, {
            pf <- meta$mainFile %>%
              filter(col_type == "Filter") %>%
              pull(col_name)

          check_geog <- function(dataset, id = c("time_period", all_of(pf))) {
            years <- dataset[, id]
            dataset[, id] <- NULL
            dataset$match <- do.call(pmax, as.list(dataset)) == do.call(pmin, as.list(dataset))
            dataset[, id] <- years
            dataset <- dataset %>%
              select(time_period, all_of(pf), everything()) %>%
              mutate(match = case_when(
                match == TRUE ~ "MATCH",
                match == FALSE ~ "NO MATCH",
                TRUE ~ "MISSING TOTAL"
              )) %>%
              arrange(match(match, c("NO MATCH", "MISSING TOTAL", "MATCH")))

            return(dataset)
          }

            output$geog_agg2 <- DT::renderDT(server = FALSE, {
              req(data_geog())

              ttt <- check_geog(data_geog()) %>% as.data.frame()


              datatable(ttt,
                rownames = FALSE,
                style = "bootstrap",
                extensions = "Buttons",
                class = "table-bordered",
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
            ) %>% formatStyle(
              "match",
              backgroundColor = styleEqual(c("NO MATCH", "MISSING TOTAL", "MATCH"), c("#910000", "#e87421", "#30A104"))
            )
          })







            #
            # # ttt <- check(data_geog)
            #
            # output$geog_agg2 <- DT::renderDT(server = FALSE, {
            #   datatable(check(data_geog),
            #             rownames = FALSE,
            #             style = "bootstrap",
            #             extensions = "Buttons",
            #             class = "table-bordered",
            #             options = list(
            #               dom = "Bptl",
            #               buttons = c("csv", "copy", "colvis"),
            #               rowCallback = JS(rowCallback),
            #               initComplete = JS(
            #                 "function(settings, json) {",
            #                 "$(this.api().table().header()).css({'background-color': '#232628', 'color': '#c8c8c8'});",
            #                 "}"
            #               ),
            #               scrollX = TRUE
            #             )
            #   )
            #
            # })
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

            Symbol <- (c("z", "c", ":", "~"))

            symbol_expected <- as.data.frame(Symbol)

            suppress_count <- symbol_expected %>%
              left_join(suppress_count) %>%
              mutate_all(~ replace(., is.na(.), 0))


            output$suppressed_cell_count <- renderUI({
              DTOutput("suppressed_cell_count_table", width = "60%") %>% withSpinner()
            })

            output$suppressed_cell_count_table <- DT::renderDT({
              datatable(suppress_count,
                rownames = FALSE,
                style = "bootstrap",
                class = "table-bordered",
                options = list(
                  dom = "t",
                  ordering = F,
                  # rowCallback = JS(rowCallback),
                  initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': '#232628', 'color': '#c8c8c8'});",
                    "}"
                  )
                )
              )
            })
          })
        } # end of if for QA stuff
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
    },
    ignoreInit = TRUE
  )

  # Reset button ----------------------------------------------------------------------------

  output$uiResetButton <- renderUI({
    actionButton("resetbutton", "Reset page", width = "80%")
  })

  observeEvent(input$resetbutton,
    {
      if (values$environment == "shinyapps") {

        # Completely reset the session, reload the app. Locally this will just end the app
        session$reload()

        # Could add a holding message if we stick with restarting the session for this?
      } else {
        # Hide results
        shinyjs::hideElement(id = "results")
        shinyjs::showElement(id = "guidance")
        shinyjs::hideElement(id = "qaResults")

        # clear files from input selection (does not fully reset fileInput, grr)
        shinyjs::reset("datafile")
        shinyjs::reset("metafile")

        # attempt to clear some of the QA objects
        shinyjs::reset("meta_table")
        shinyjs::reset("data_preview")
        shinyjs::reset("geog_time_perms2")
        shinyjs::reset("tables")
        shinyjs::reset("indicators")
        shinyjs::reset("submit")
        shinyjs::reset("suppressed_cell_count")
        shinyjs::reset("indicator_choice")
        shinyjs::reset("geogChoice")
        shinyjs::reset("table_list")
        shinyjs::reset("outlier_indicator_choice")
        shinyjs::reset("current_time")
        shinyjs::reset("comparison_time")
        shinyjs::reset("threshold_setting")
        shinyjs::reset("submit_outlier")
        shinyjs::reset("table_outlier_list")
        shinyjs::reset("geog_indicator_choice")
        # shinyjs::reset("geog_level_choice")
        # shinyjs::reset("geog_sublevel_choice")
        shinyjs::reset("submit_geographies")
        shinyjs::reset("table_geography_list")


        shinyjs::reset("screenbutton")

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

        # set QA list objects to NULL
        output$geog_agg2 <- NULL
        output$table_outlier_list <- NULL
        output$table_list <- NULL

        values$datafile <- NULL
        values$metafile <- NULL
        values$clear <- TRUE
        reset("datafile")
        reset("metafile")

        shinyjs::hideElement(id = "reset_button")
      }
    },
    priority = 1000
  )


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
