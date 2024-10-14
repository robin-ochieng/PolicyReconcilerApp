planSummaryUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "card-custom plan-selector-container",  # Combine classes for custom styling and layout
      div(
        class = "plan-selector",  # Maintain the existing class for width and alignment
        selectInput(ns("planSelector"), "Select Plan:", choices = NULL)
      )
    ),
    bs4Card(
      width = 12,
      title = "Plan Specific Summaries",
      status = "white",
      solidHeader = TRUE,
      collapsible = TRUE,
      downloadButton(ns("downloadPlanSummary"), "Download Plan Summaries", class = "btn btn-primary"),
      br(),
      br(),
      DTOutput(ns("planSummary"))
    )
  )
}

planSummaryServer <- function(id, summaryData) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Prepare choices for the select input
    planChoices <- reactive({
      req(summaryData())
      choices <- c("All Plans", unique(summaryData()$`Plan Name`))
      choices
    })

    # Update select input with available plans
    observeEvent(planChoices(), {
      choices <- planChoices()
      updateSelectInput(session, "planSelector", choices = choices, selected = "All Plans")
    }, ignoreNULL = FALSE)

    # Process data and render the data table
    filteredData <- reactive({
      req(summaryData())
      selected_plan <- input$planSelector
      selected_plan <- ifelse(is.null(selected_plan) || selected_plan == "", "All Plans", selected_plan)
      
      # Filter data based on selected plan
      data <- if (selected_plan != "All Plans") {
        summaryData() %>% filter(`Plan Name` == selected_plan)
      } else {
        summaryData()
      }

      # Calculate summaries
      plan_summaries <- data %>%
        group_by(`Plan Name`, `Maturity Year`) %>%
        summarise(
          Number = sum(`Count of Policy No`),  
          `Sum Assured` = sum(`Sum of Revised Sum Assured`),
          `Office Premium` = sum(`Sum of Payable Premium`),
          .groups = 'drop'
        ) %>%
        mutate(
          `Sum Assured` = number(`Sum Assured`, scale = 1, accuracy = 1, big.mark = ","),
          `Office Premium` = number(`Office Premium`, scale = 1, accuracy = 1, big.mark = ",")
        ) %>%
        arrange(`Plan Name`, `Maturity Year`)
      
      plan_summaries
    })

    output$planSummary <- renderDT({
      datatable(filteredData(), options = list(scrollX = TRUE, pageLength = 30, autoWidth = FALSE, paging = TRUE, searching = TRUE, info = FALSE))
    })

    # Download handler for the plan summaries
    output$downloadPlanSummary <- downloadHandler(
      filename = function() {
        paste0("Plan_Summary_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(filteredData(), file, row.names = FALSE)
      }
    )
  })
}
