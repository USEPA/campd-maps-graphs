### UI

navbarPage(strong("Emission Tools"),
           tabPanel("Emission Trends",
                    titlePanel("Emission Trends"),
                    textOutput("emissionTrendsText"),
                    sidebarLayout(
                      sidebarPanel(h3("Filters"), 
                                   radioButtons("levelOfAnalysis", "Level of Analysis",
                                                c("National"="n", 
                                                  "State"="s",
                                                  "Facility"="f")
                                   )
                      ),
                      mainPanel(
                        #plotOutput("plot")
                      )
                    )
           ),
           tabPanel("Unit Landscape",
                    titlePanel("Unit Landscape"),
                    textOutput("unitLandscapeText"),
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