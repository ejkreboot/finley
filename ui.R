library(shiny)
library(shinythemes)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cyborg"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  fluidRow(
    h4(class="masthead", "finley."),
    p(class="submasthead", "showing you the money")
  ),
  tabsetPanel(
    tabPanel("Dashboard",
      fluidRow(
        column(8,
          plotOutput("accountsPlot")
        ),
        column(4,
          plotOutput("totalExpensesPlot")
        ),
      ),
      fluidRow(
        column(12,
          dataTableOutput('categoriesTable')
        )
      )
    ),
    tabPanel("Transactions",
      fluidRow(
        column(12,
          div(
            tags$table(class="transactions",
              tags$thead(id="transactions_table",
                tags$tr(
                  tags$td("Date"),
                  tags$td("Payee"),
                  tags$td("Amount"),
                  tags$td("Category")
                )
              ) 
            )
          )
        )
      )
    )
  )
)