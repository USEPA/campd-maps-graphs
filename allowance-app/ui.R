### UI

navbarPage(strong("Allowance Tools"),
           tabPanel("Allowance Bank",
                    titlePanel("Allowance Bank"),
                    textOutput("allowanceBankText"),
                    sidebarLayout(
                      sidebarPanel(h3("Filters"), 
                                   radioButtons("levelOfAnalysis", "Level of Analysis",
                                                c("Program"="p", 
                                                  "State"="s")
                                   )
                      ),
                      mainPanel(
                        #plotOutput("plot")
                      )
                    )
           ),
           tabPanel("Program Budgets",
                    titlePanel("Program Budgets"),
                    textOutput("programBugetsText"),
                    sidebarLayout(
                      sidebarPanel(h3("Filters"),
                                   radioButtons("levelOfAnalysis", "Level of Analysis",
                                                c("Program"="p", 
                                                  "State"="s")
                                   )
                      ),
                      mainPanel(
                        #plotOutput("plot")
                      )
                    )
           ),
)