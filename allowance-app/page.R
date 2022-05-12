### UI
source('./allowance-app/allowances-banked.R')
source('./allowance-app/program-budgets.R')
source('./allowance-app/resources.R')

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
               tabPanel("Resources",
                        resourcesUI(ns("resourcesTab"))
               ),
    )
  )
}

programAppServer <- function(input, output, session) {
  callModule(allowancesBankedServer, "allowancesBankedTab")
  callModule(programBudgetsServer, "programBudgetsTab")
  callModule(resourcesServer, "resourcesTab")
}


