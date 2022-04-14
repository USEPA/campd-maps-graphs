### UI
source('./allowance-app/allowances-banked.R')
source('./allowance-app/program-budgets.R')

programAppUI <- function(id){
  ns <- NS(id)
  tagList(
    navbarPage(strong("Program Insights"),
               tabPanel("Allowance Trends",
                        allowancesBankedUI(ns("allowancesBankedTab"))
               ),
               tabPanel("Program Budgets",
                        programBudgetsUI(ns("programBudgetsTab"))
               ),
    )
  )
}

programAppServer <- function(input, output, session) {
  callModule(allowancesBankedServer, "allowancesBankedTab")
  callModule(programBudgetsServer, "programBudgetsTab")
}


