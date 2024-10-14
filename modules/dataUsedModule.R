# Description: Module for displaying data used for valuation
dataUsedUI <- function(id) {
  ns <- NS(id)
  tagList(
    bs4Card(
      width = 12,
      title = "Data Used for Valuation",
      status = "white",
      solidHeader = TRUE,
      collapsible = TRUE,
      downloadButton(ns("downloadDataUsed"), "Download Data Used for Valuation", class = "btn btn-primary"),
      DTOutput(ns("dataUsedTable"))
    )
  )
}

# Server Function for the module
dataUsedServer <- function(id, curData) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive expression to create the specific columns for the data used table
    dataUsed <- reactive({
      req(curData())
      curData() %>%
        transmute(
          `Policy Number` = `Policy No`,
          `Policy Type` = `Plan Name`,
          `Date of Birth` = DOB,
          `Gender` = Sex,
          `Basic Sum Assured` = `Revised Sum Assured`,
          `BONUS` = 0,
          `Annual Premium / Single Premium` = `Annual Premium`,
          `Payment Frequency` = `Installment Count`,
          `Effective Date` = `Start Date`,
          `Final Maturity Date` = `Maturity Date`,
          `Policy Term` = `Policy Term`,
          `Data Category` = `New Status`
        )
    })

    # Render the data used table
    output$dataUsedTable <- renderDT({
      datatable(dataUsed(), 
                options = list(scrollX = TRUE, pageLength = 50, autoWidth = FALSE, paging = TRUE, searching = FALSE, info = FALSE,
                              initComplete = JS("function(settings, json) {",
                                                "  $(this.api().table().header()).css({",
                                                "    'background-color': 'FFFFFF',", 
                                                "    'color': '#00000'",  
                                                "  });",
                                                "}")))
    })

    # Download handler for the data used dataset
    output$downloadDataUsed <- downloadHandler(
      filename = function() {
        paste("data_used_for_valuation", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(dataUsed(), file, row.names = FALSE)
      })
  })
}
