# FileInputModule.R

fileInputUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    hr(),
    div(
      class = "card-custom",
      fileInput(ns("file"), 
                label = tags$span("Upload Data as Excel File"),
                accept = c(".xlsx", ".xls")
      )
    ),
    hr(),
    div(
      class = "card-custom",
      dateInput(
        ns("valDate"),
        label = "Select Valuation Date:",
        value = as.Date("12/31/2022", format = "%m/%d/%Y"),
        max = Sys.Date()
      )
    ),
    hr(),
    br()
  )
}

fileInputServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive({
      req(input$file)
      inFile <- input$file
      list(data = read_excel(inFile$datapath), date = input$valDate)
    })
  })
}
