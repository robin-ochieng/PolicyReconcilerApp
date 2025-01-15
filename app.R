library(shiny)
library(tidyverse)
library(readxl)
library(scales)
library(plotly)
library(ggrepel)
library(bs4Dash)
library(bslib)
library(DT)

# Source the modules
source("modules/dataReconSummaryModule.R")
source("modules/currentValuationSummaryModule.R")
source("modules/planSummaryModule.R")
source("modules/policyDataModule.R")

# Define the User Interface for the Application
# Increase max file size to 100 MB
options(shiny.maxRequestSize = 2000 * 1024^2)

# Define a custom theme using bslib
my_theme <- bs_theme(
  bg = "#202123", 
  fg = "#E1E1E1", 
  primary = "#EA80FC", 
  info = "#17a2b8",
  secondary = "#00BFA5",
  base_font = font_google("Mulish"),
  heading_font = font_google("Mulish"),
  code_font = font_google("Mulish"),
  navbar_bg = "#333333",  # Darker background for the navbar for contrast
  navbar_fg = "#ffffff"  # White text color for readability
)


# Define the User Interface for the Application
ui <- bs4DashPage(
  dark = NULL,
  help = NULL,
  fullscreen = FALSE,
  scrollToTop = TRUE,
  freshTheme = my_theme,
  header = bs4DashNavbar(
    status = "white",
    skin = "dark",
    controlbarIcon = NULL,
    sidebarIcon = NULL,
    tags$li(
      class = "text-center header-title-container",  # Added a new class for more specific styling
      tags$h4("Life Application Dashboard", class = "header-title")
    ),
    tags$div(class = "control-bar", actionButton("toggleControlbar", "Input Controls", class = "btn btn-primary control-button"))
  ),
  sidebar = bs4DashSidebar(
    skin = NULL,
    collapsed = FALSE,
    minified = FALSE,
  tags$div(
    class = "menu-container",
    tags$h3("Valuation Control Panel", class = "menu-title"),
    bs4SidebarMenu(
      bs4SidebarMenuItem("Previous Val Data", tabName = "viewPrevValData", icon = icon("file-invoice-dollar")),
      bs4SidebarMenuItem("Current Val Data", tabName = "viewCurValData", icon = icon("calendar-check")),
      bs4SidebarMenuItem("Data Reconciliation", tabName = "dataRecon", icon = icon("sync-alt")),
      bs4SidebarMenuItem("Valuation Summary", tabName = "valSummary", icon = icon("chart-bar")),
      bs4SidebarMenuItem("Plan Summaries", tabName = "planSummary", icon = icon("book")),
      bs4SidebarMenuItem("Policy Data", tabName = "dataUsedTab", icon = icon("database"))
    )),
    div(class = "sidebar-logo",
        img(src = "images/kenbright.png")
    )
  ),
  controlbar = bs4DashControlbar(
    id = "controlbar",
    skin = "info",
    bs4Card(
      title = "Input Controls",
      background = "white",
      width = 12,
      style = "max-width: 100%;",
      status = "primary",
      solidHeader = TRUE,
      collapsible = FALSE,
      br(),
      actionButton("loadAllData", "RUN", class = "btn btn-primary")
    )
  ),
  body = bs4DashBody(
    tags$head(
      includeCSS("www/css/custom_styles.css"),
      tags$link(href = "https://fonts.googleapis.com/css?family=Mulish", rel = "stylesheet"),
      tags$script(src = "www/javascript/sidebarToggle.js")
    ),
    bs4TabItems(
      bs4TabItem(tabName = "viewPrevValData",
                 fluidRow(
                   hr(),
                   div(
                     class = "card-custom",
                     fileInput("file1", 
                               label = tags$span("Upload Previous Valuation Data as a Excel File"),
                               accept = c(".xlsx", ".xls")
                     )
                   ),
                   hr(),
                   div(
                     class = "card-custom",
                     dateInput(
                       "prevValDate",
                       label = "Select Previous Valuation Date:",
                       value = as.Date("12/31/2022", format = "%m/%d/%Y"),
                       max = Sys.Date()
                     )
                   ),
                   hr(),
                   br(),
                   bs4Card(
                     title = "Previous Valuation Data Overview",
                     status = "white",
                     solidHeader = TRUE,
                     width = 12,
                     DTOutput("viewPrevValData")
                   )
                 )
      ),
      bs4TabItem(tabName = "viewCurValData",
                 fluidRow(
                   hr(),
                   div(
                     class = "card-custom",
                     fileInput("file2", 
                               label = tags$span("Upload Current Valuation Data as a Excel File"),
                               accept = c(".xlsx", ".xls")
                     )
                   ),
                   hr(),
                   div(
                     class = "card-custom",
                     dateInput(
                       "curValDate",
                       label = "Select Current Valuation Date:",
                       value = as.Date("12/31/2023", format = "%m/%d/%Y"),
                       max = Sys.Date()
                     )
                   ),
                   hr(),
                   br(),
                   bs4Card(
                     title = "Current Valuation Data Overview",
                     status = "white",
                     solidHeader = TRUE,
                     width = 12,
                     DTOutput("viewCurValData")
                   )
        )
      ),
      bs4TabItem(
        tabName = "dataRecon",
        dataReconSummaryUI("dataReconSummary")
      ),
      bs4TabItem(
        tabName = "valSummary",
        currentValuationSummaryUI("currentValSummaryModule")
      ),
      bs4TabItem(
        tabName = "planSummary",
        planSummaryUI("planSummaryModule")
      ),
      bs4TabItem(
        tabName = "dataUsedTab",
        dataUsedUI("dataUsed")
      )
    ),
    div(class = "body-footer", "© 2024 Life Dashboard")
  )
)

# Define the server logic required to read the input and calculate outputs
server <- function(input, output, session) {
  
  observeEvent(input$toggleControlbar, {
    updateBoxSidebar("controlbar")
  })
  
  # Reactive containers for the datasets
  reactivePrevValData <- reactiveVal()
  reactiveCurValData <- reactiveVal()
  
  # Extract policy numbers when the Previous Valuation Data is loaded
  observeEvent(input$file1, {
    req(input$file1)
    inFile <- input$file1
    data <- read_excel(inFile$datapath)
    reactivePrevValData(data)  # Store the loaded data
    policy_numbers_prev_val <<- data$`Policy No`  # Global assignment to use in mutation
  })
  
  # Extract policy numbers when the Current Valuation Data is loaded
  observeEvent(input$file2, {
    req(input$file2)
    inFile <- input$file2
    data <- read_excel(inFile$datapath)
    reactiveCurValData(data)  # Store the loaded data
    policy_numbers_cur_val <<- data$`Policy No`  # Global assignment to use in mutation
  })
  
  # Reactive value for storing the Current Valuation data
  processedPrevValData <- eventReactive(input$loadAllData, {
    req(reactivePrevValData())
    withProgress(message = 'Loading Previous Valuation Data...', {
      incProgress(0.1)  # initial progress
      data <- reactivePrevValData() %>%
        mutate(   
          `Maturity Date` = as.Date(`Maturity Date`, format = "%m/%d/%Y"),
          `Start Date` = as.Date(`Start Date`, format = "%m/%d/%Y"),
          index = match(`Policy No`, policy_numbers_cur_val),
          `Present in Current Valuation` = ifelse(is.na(index), "NO", policy_numbers_cur_val[index]),
          `Expected Maturity` = ifelse(`Maturity Date` <= input$curValDate, "YES", "NO"),
          `Policy Status` = case_when(
            `Present in Current Valuation` == "NO" & `Expected Maturity` == "NO" ~ "Lapse",
            `Present in Current Valuation` == "NO" & `Expected Maturity` == "YES" ~ "Maturity",
            TRUE ~ "Present in Both"),
          Active = `Maturity Date` > input$prevValDate)
      incProgress(0.9, detail = "Almost done...")  # incremental progress
      data
    })
  })
  
  # Reactive value for storing the Current Valuation data
  processedCurValData <- eventReactive(input$loadAllData, {
    req(reactiveCurValData())
    withProgress(message = 'Loading Current Valuation Data...', {
      incProgress(0.1)  # initial progress
      data <- reactiveCurValData()%>%
        mutate(   
          index_2022 = match(`Policy No`, policy_numbers_prev_val),
          `Present in Prev_Val` = ifelse(is.na(index_2022), "NO", policy_numbers_prev_val[index_2022]),
          `New Policy` = ifelse(`Start Date` > input$prevValDate, "YES", "NO"),
          Status_2 = case_when(
            `Present in Prev_Val` == "NO" & `New Policy` == "YES" ~ "NEW",
            `Present in Prev_Val` == "NO" & `New Policy` != "YES" ~ "MISSING", TRUE ~ "Present"),
          `New Status` = ifelse(trimws(Status) == "PAID UP", "Paidup", "Inforce"),
          `Installment Count` = case_when(
            `Payment Mode` == "M" ~ 12,
            `Payment Mode` == "Q" ~ 4,
            `Payment Mode` == "H" ~ 2,
            TRUE ~ 1),
          `Installment Premium` = `Annual Premium` / `Installment Count`,
          `With Profit` = "With Profit",
          `Revised Sum Assured` = ifelse(Status == "PAID UP", `Paid Up Value`, `Sum Insured`),
          `Payable Premium` = ifelse(Status == "PAID UP", 0, `Annual Premium`),
          Checks = ifelse(`Policy No` %in% policy_numbers_prev_val, as.character(`Policy No`), "New Policy"),
          `New Entries` = ifelse(Checks != "New Policy", "Ongoing", "New Policy"),
          `Revised Maturity Date` = `Start Date` %m+% months(12 * `Policy Term`),
          `Maturity Year` = year(`Revised Maturity Date`)
          )
      incProgress(0.9, detail = "Almost done...")  # incremental progress
      data
    })
  })
  
  # Display processed Previous Valuation data
  output$viewPrevValData <- renderDT({
    req(processedPrevValData())
    datatable(processedPrevValData(), options = list(scrollX = TRUE, 
                                                     pageLength = 30,
                                                     autoWidth = FALSE,
                                                     paging = TRUE,
                                                     searching = FALSE,
                                                     info = FALSE,
                                                     columnDefs = list(
                                                       list(visible = FALSE, targets = c(7,23))),
                                                     initComplete = JS(
                                                       "function(settings, json) {",
                                                       "  $(this.api().table().header()).css({",
                                                       "    'background-color': '#FFFFFF',", 
                                                       "    'color': 'black'",  
                                                       "  });",
                                                       "}"
                                                     )))
  })
  
  
  # Display processed Current Valuation data
  output$viewCurValData <- renderDT({
    req(processedCurValData())
    datatable(processedCurValData(), options = list(scrollX = TRUE, 
                                                    pageLength = 30,
                                                    autoWidth = FALSE,
                                                    paging = TRUE,
                                                    searching = FALSE,
                                                    info = FALSE,
                                                    initComplete = JS(
                                                      "function(settings, json) {",
                                                      "  $(this.api().table().header()).css({",
                                                      "    'background-color': '#FFFFFF',", 
                                                      "    'color': 'black'",  
                                                      "  });",
                                                      "}"
                                                    )))
  })
  
  # Call the module server function for data reconciliation summary
  dataReconSummaryServer("dataReconSummary", prevData = processedPrevValData, curData = processedCurValData)

  # Call the module server function for current valuation summary
  summaryData <- currentValuationSummaryServer("currentValSummaryModule", curData = processedCurValData)

  # Call the module server function for valuation summaries
  planSummaryServer("planSummaryModule", summaryData = summaryData)
 
  # Call the module server function for data used
  dataUsedServer("dataUsed", curData = processedCurValData, curValDate = reactive(input$curValDate))
}



# Run the application
shinyApp(ui = ui, server = server)
