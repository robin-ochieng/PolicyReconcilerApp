# UI Function for the module
currentValuationSummaryUI <- function(id) {
  ns <- NS(id)
  tagList(
    bs4Card(
      width = 12,
      title = "Current Valuation Data Summary",
      status = "primary",
      solidHeader = TRUE,
      collapsible = TRUE,
      DTOutput(ns("currentValSummary"))
    )
  )
}

# Server Function for the module
currentValuationSummaryServer <- function(id, curData) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Create and store summary data reactively
    summaryData <- reactive({
      req(curData())
      curData() %>%
        group_by(`Plan Name`, `Maturity Year`) %>%
        summarise(
          `Count of Policy No` = n(),
          `Sum of Revised Sum Assured` = sum(`Revised Sum Assured`, na.rm = TRUE),
          `Sum of Payable Premium` = sum(`Payable Premium`, na.rm = TRUE),
          .groups = 'drop'
        )
    })

    # Create the summary table for current valuation data
    output$currentValSummary <- renderDT({
      req(summaryData())
      datatable(summaryData(), options = list(scrollX = TRUE, pageLength = 30, autoWidth = FALSE, paging = TRUE, searching = FALSE,
                                             info = FALSE,
                                             columnDefs = list(
                                                list(targets = c(4, 5), render = JS(
                                                  "function(data, type, row, meta) {
                                                    if(type === 'display') {
                                                      data = parseFloat(data);
                                                      return data.toLocaleString(undefined, {
                                                        minimumFractionDigits: 0, 
                                                        maximumFractionDigits: 0
                                                      });
                                                    }
                                                    return data; // keep data as is for sorting or type detection
                                                  }"
                                                ))
                                              ),
                                              initComplete = JS("function(settings, json) {",
                                                                "  $(this.api().table().header()).css({",
                                                                "    'background-color': '#007BFF',", 
                                                                "    'color': '#FFFFFF'",  
                                                                "  });",
                                                                "}")))
  })
  # Return the summary data for use in other modules
    return(summaryData)
  })
}


