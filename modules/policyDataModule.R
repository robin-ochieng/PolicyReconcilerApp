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
      br(),
      br(),
      DTOutput(ns("dataUsedTable"))
    )
  )
}

# Server Function for the module
dataUsedServer <- function(id, curData, curValDate) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive expression to create the specific columns for the data used table
dataUsed <- reactive({
  req(curData(), curValDate())
  DOB <- as.Date(curData()$DOB)
  `Start Date` <- as.Date(curData()$`Start Date`)
  `Maturity Date` <- as.Date(curData()$`Maturity Date`)
  currentValDate <- as.Date(curValDate())

  curData() %>%
    transmute(
      `Policy Number` = `Policy No`,
      `Policy Type` = `Plan Name`,
      `Date of Birth` = DOB,
      Sex = Sex,
      `Basic Sum Assured` = `Revised Sum Assured`,
      `BONUS` = 0,
      `Annual Premium / Single Premium` = `Annual Premium`,
      `Payment Frequency` = `Installment Count`,
      `Effective Date` = `Start Date`,
      `Final Maturity Date` = `Maturity Date`,
      `Policy Term` = `Policy Term`,
      `Data Category` = `New Status`,
      Age = pmax(18, ifelse(is.na(interval(`Date of Birth`, currentValDate) %/% years(1)), 45, interval(`Date of Birth`, currentValDate) %/% years(1))),
      Gender = if_else(Sex == "M", "Male", "Female"),
      Nominal_Term = `Policy Term`,
      Calculated_Term = floor(as.numeric(as.Date(`Final Maturity Date`) - as.Date(`Effective Date`)) / 365.25),
      Annual_Premium = `Annual Premium / Single Premium`,
      Death_Benefit = if_else(`Policy Type` == "Annuity", 0, `Basic Sum Assured`),
      Survival_Benefit = if_else(`Policy Type` == "Annuity", `Basic Sum Assured` * 12, `Basic Sum Assured`),
      Product_Type = `Policy Type`,
      Start_Year = year(`Effective Date`),
      Current_Year_No = year(currentValDate) - Start_Year + 1,
      Guarantee_Period = 0,
      Benefit_Escalation = 0,
      PolicyNo = `Policy Number`,
      Data_Category = `Data Category`
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


