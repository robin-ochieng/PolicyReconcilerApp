# Data Reconciliation and Summaries in R

## Introduction
This process is automated in R Shiny.

## Procedure
The procedure for generating these summaries in R Shiny is as follows:

1. **Install Required Packages**:
   ```R
   install.packages(c("shiny", "tidyverse", "readxl", "scales", "plotly", "ggrepel", "bs4Dash", "bslib", "DT"))

2. **Launch the Shiny App**:
Run app.R in R-Studio or use:
   ```R
   shiny::runApp("path/to/your/app.R")

3. **Upload Data**:
- Navigate to the **"Previous Val Data"** tab and upload the previous valuation data.
- Navigate to the **"Current Val Data"** tab and upload the current valuation data.
- Update the respective valuation dates for each dataset.

4. **Run the App**:
- Go to the **"Input Controls"** tab.
- Click the **"Run"** button to process and view the data for each valuation period.

5. **View Data Reconciliation**:
- Access the **"Data Reconciliation"** tab to view the movement between the valuation periods. This includes lapses, maturities, new policies, and missing policies.

6. **View Valuation Summary**:
- The **"Valuation Summary"** tab provides summaries of the current valuation, broken down by Product name and Maturity year.
- Detailed breakdowns per product can be generated and downloaded in the **"Plan Summaries"** tab.

