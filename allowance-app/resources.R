## Resources Page

resourcesUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      tags$head(HTML("<title>Resouces</title>")), 
      h1("Program Insights Resources"),
      p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, 
           sed do eiusmod tempor incididunt ut labore et dolore 
           magna aliqua. Ut enim ad minim veniam, quis nostrud 
           exercitation ullamco laboris nisi ut aliquip ex ea commodo 
           consequat. Duis aute irure dolor in reprehenderit in 
           voluptate velit esse cillum dolore eu fugiat nulla 
           pariatur. Excepteur sint occaecat cupidatat non proident, 
           sunt in culpa qui officia deserunt mollit anim id est 
           laborum."),
      h2("Program Code Descriptions"),
      p("Below is a table of information for CAMD's allowance based programs."),
      div(class="resource-table",HTML(getHTML(programInfo$descriptionTable))),
      h2("CSAPR State Budgets, Variability Limits, and Assurance Provisions"),
      p("Please visit ",
        tags$a(href="https://www.epa.gov/csapr/cross-state-air-pollution-rule-csapr-state-budgets-variability-limits-and-assurance", 
               "CSAPR state budgets, variability limits and assurance provisions",
               target="_blank"),
        "to find state budgets, variability limits and assurance provisions information for the following programs:",
        tags$ul(
          tags$li("The CSAPR SO2 Group 1 trading program;"),
          tags$li("The CSAPR SO2 Group 2 trading program;"),
          tags$li("The CSAPR NOX annual trading program;"),
          tags$li("The CSAPR NOX ozone season Group 1 trading program;"),
          tags$li("The CSAPR NOX ozone season Group 2 trading program.")
        )),
      p("Please visit ",
        tags$a(href="https://www.epa.gov/csapr/revised-cross-state-air-pollution-rule-update", 
               "Revised Cross-State Air Pollution Rule Update",
               target="_blank"),
        "to find state budgets, variability limits, supplemental information and 
        assurance provisions information for the following programs:",
        tags$ul(
          tags$li("CSAPR NOX ozone season Group 3 trading program.")
        ))
    )
  )
}


resourcesServer <- function(input, output, session) {
  
}