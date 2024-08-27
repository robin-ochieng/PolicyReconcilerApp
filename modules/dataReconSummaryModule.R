# UI Function for the module
dataReconSummaryUI <- function(id) {
  ns <- NS(id)
  tagList(
    bs4Card(
      width = 12,
      title = "Summary of Previous Valuation Data",
      status = "primary",
      solidHeader = TRUE,
      collapsible = TRUE,
      DTOutput(ns("summaryPrevVal"))
    ),
    bs4Card(
      width = 12,
      title = "Summary of Current Valuation Data",
      status = "primary",
      solidHeader = TRUE,
      collapsible = TRUE,
      DTOutput(ns("summaryCurVal"))
    ),
    bs4Card(
      width = 12,
      title = "Ordinary Life Data Reconciliation as at Current Valuation Date",
      status = "primary",
      solidHeader = TRUE,
      collapsible = TRUE,
      downloadButton(ns("downloaddataReconSummary"), "Download Data Reconciliation Summary", class = "btn btn-primary"),
      br(),
      br(),
      DTOutput(ns("dataReconSummary"))
    )
  )
}

# Server Function for the module
dataReconSummaryServer <- function(id, prevData, curData) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Create the summary table for previous valuation data
    output$summaryPrevVal <- renderDT({
      req(prevData())
      prevData() %>%
        group_by(`Plan Name`, `Policy Status`) %>%
        summarise(Count = n(), .groups = 'drop') %>%
        pivot_wider(names_from = `Policy Status`, values_from = Count, values_fill = list(Count = 0)) %>%
        rowwise() %>%
        mutate(`Grand Total` = sum(c_across(c(Lapse, Maturity, `Present in Both`)), na.rm = TRUE))
    }, options = list(scrollX = TRUE, pageLength = 50, autoWidth = FALSE, paging = TRUE, searching = FALSE, info = FALSE,
                        initComplete = JS("function(settings, json) {",
                                          "  $(this.api().table().header()).css({",
                                          "    'background-color': '#007BFF',", 
                                          "    'color': '#FFFFFF'",  
                                          "  });",
                                          "}")))

    # Create the summary table for current valuation data
    output$summaryCurVal <- renderDT({
      req(curData())
      
      # Create a summary table from the Status_2 column
      summaryData <- curData() %>%
        group_by(`Plan Name`, `Status_2`) %>%
        summarise(Count = n(), .groups = 'drop') %>%
        pivot_wider(names_from = `Status_2`, values_from = Count, values_fill = list(Count = 0)) 
      
      # Calculate the count of "YES" in the New Policy column
      newPoliciesData <- curData() %>%
        group_by(`Plan Name`) %>%
        summarise(`New Policies` = sum(`New Policy` == "YES"))
      
      # Join the newPoliciesData with the summaryData
      finalSummary <- summaryData %>%
        left_join(newPoliciesData, by = "Plan Name") %>%
        rowwise() %>%
        mutate(`Grand Total` = sum(c_across(c(MISSING, Present, `New Policies`)), na.rm = TRUE)) %>%
        relocate(`New Policies`, .before = `Grand Total`)
      
      datatable(finalSummary, 
                options = list(scrollX = TRUE, pageLength = 50, autoWidth = FALSE, paging = TRUE, searching = FALSE, info = FALSE,
                              initComplete = JS("function(settings, json) {",
                                                "  $(this.api().table().header()).css({",
                                                "    'background-color': '#007BFF',", 
                                                "    'color': '#FFFFFF'",  
                                                "  });",
                                                "}")))
    })


    # Reactive expression to create the data reconciliation summary table
    dataReconSummaryData <- reactive({
      req(prevData(), curData())

      # Summarize the previous valuation data
      prevSummary <- prevData() %>%
        group_by(`Plan Name`, `Policy Status`) %>%
        summarise(Count = n(), .groups = 'drop') %>%
        pivot_wider(names_from = `Policy Status`, values_from = Count, values_fill = list(Count = 0)) %>%
        rowwise() %>%
        mutate(`Grand Total` = sum(c_across(c(Lapse, Maturity, `Present in Both`)), na.rm = TRUE)) %>%
        select(`Plan Name`, Lapse, Maturity, `Grand Total`)

      # Summarize the current valuation data
      curSummary <- curData() %>%
        group_by(`Plan Name`, `Status_2`) %>%
        summarise(Count = n(), .groups = 'drop') %>%
        pivot_wider(names_from = `Status_2`, values_from = Count, values_fill = list(Count = 0)) %>%
        left_join(
          curData() %>%
            group_by(`Plan Name`) %>%
            summarise(`New Policies` = sum(`New Policy` == "YES")),
          by = "Plan Name"
        ) %>%
        select(`Plan Name`, MISSING, `New Policies`)

      # Combine the summaries to create the data reconciliation summary
      full_join(prevSummary, curSummary, by = "Plan Name") %>%
        mutate(
          `Lapsed Policies` = -Lapse,
          `Matured Policies` = -Maturity,
          `Active Policies as at Previous Valuation Date` = `Grand Total`,
          `Active as at Current Valuation Date` = `Active Policies as at Previous Valuation Date` + 
                                                 `Lapsed Policies` + 
                                                 `Matured Policies` + 
                                                 MISSING + 
                                                 `New Policies`
        ) %>%
        select(
          `Plan Name`, 
          `Active Policies as at Previous Valuation Date`, 
          `Lapsed Policies`, 
          `Matured Policies`, 
          `Policies Not Valued in Previous Valuation` = MISSING, 
          `New Policies`, 
          `Active as at Current Valuation Date`
        )
    })

    # Render the data reconciliation summary table
    output$dataReconSummary <- renderDT({
      datatable(dataReconSummaryData(), 
                options = list(scrollX = TRUE, pageLength = 50, autoWidth = FALSE, paging = TRUE, searching = FALSE, info = FALSE,
                              initComplete = JS("function(settings, json) {",
                                                "  $(this.api().table().header()).css({",
                                                "    'background-color': '#007BFF',", 
                                                "    'color': '#FFFFFF'",  
                                                "  });",
                                                "}")))
    }) 

    # Download handler for the data reconciliation summary
    output$downloaddataReconSummary <- downloadHandler(
      filename = function() {
        paste("data_reconciliation_summary", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(dataReconSummaryData(), file, row.names = FALSE)
      }) 
  })
}
