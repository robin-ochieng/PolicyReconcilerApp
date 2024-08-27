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
      curData() %>%
        group_by(`Plan Name`, `Status_2`) %>%
        summarise(Count = n(), .groups = 'drop') %>%
        pivot_wider(names_from = `Status_2`, values_from = Count, values_fill = list(Count = 0)) %>%
        rowwise() %>%
        mutate(`Grand Total` = sum(c_across(c(MISSING, Present)), na.rm = TRUE))
    }, options = list(scrollX = TRUE, pageLength = 50, autoWidth = FALSE, paging = TRUE, searching = FALSE, info = FALSE,
                        initComplete = JS("function(settings, json) {",
                                          "  $(this.api().table().header()).css({",
                                          "    'background-color': '#007BFF',", 
                                          "    'color': '#FFFFFF'",  
                                          "  });",
                                          "}")))
  })
}
