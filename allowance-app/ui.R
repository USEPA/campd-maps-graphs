### UI

navbarPage(strong("Allowance Tools"),
           tabPanel("Allowance Bank",
                    fluidPage(
                      add_busy_gif(
                        src = "https://jeroen.github.io/images/banana.gif",
                        height = 70, width = 70,
                        position = 'full-page'
                      ),
                      titlePanel("Allowance Bank"),
                      textOutput("allowanceBankText"),
                      sidebarLayout(
                        sidebarPanel(h3("Filters"), 
                                   radioButtons("levelOfAnalysis", "Level of Analysis",
                                                c("Program"="p", 
                                                  "State"="s"),
                                                selected = "p"
                                   ),
                                   conditionalPanel(
                                     condition = "input.levelOfAnalysis == 'p'",
                                     multiSelectUI("programInput", 
                                                   label= "Select one or more programs",
                                                   choices= uniquePrograms)
                                   ),
                                   conditionalPanel(
                                     condition = "input.levelOfAnalysis == 's'",
                                     tags$div(style = "padding-left: 15px;",
                                              columnFilterSetUI("filterset", 
                                                                allowanceBankFilterIndicesState
                                              )
                                     )
                                   ),
                                   actionButton("previewButton", "Preview Data")
                        ),
                        mainPanel(
                          lineGraphUI("allocatedPlot"),
                          lineGraphUI("deductedPlot"),
                          lineGraphUI("carriedPlot")
                          
                        )
                      ),
                      h2("Summary Data Table"),
                      dataTableOutput("bankTable")
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