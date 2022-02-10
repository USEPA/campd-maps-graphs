
# user interface with map and emissions graph
ui <- fluidPage(
  add_busy_gif(
    src = "https://jeroen.github.io/images/banana.gif",
    height = 70, width = 70
  ),
  titlePanel("Facility Map"),
  p("Description..."),
  fluidRow(
    column(4,
           selectizeInput("program", label = "Chose a Regulatory Program",
                 choices = c(),
                 options = list(placeholder = '--Select Program--',
                         onInitialize = I('function() { this.setValue(""); }')))),
    column(4, actionButton("clearbutton", "Clear Program"), style="margin-top: 24px; margin-bottom: 10px;")
    ),
  fluidRow(tags$style(type = "text/css", "#map {!important;}"),
           style = "padding-bottom: 10px;",
    absolutePanel(id = "elements", class = "panel panel-default",
      #draggable = FALSE,
      right = 0,
      style = "overflow-y:scroll; max-height: 380px;
          background-color: white; z-index: 10; margin: 10px;
          padding: 10px;",
      tabsetPanel(
        tabPanel("Facility Summary", style = "margin: 10px; height:100%;
                      width:300px;",
                 htmlOutput("fac_summary_text"),
                 ),
        tabPanel("Account Summary", style = "margin: 10px; height:100%;
                      width:300px;",
                 htmlOutput("acct_summary_text"),
                 ),
        )
    ),
    leafletOutput("map",width="100%")
  ),
  h3("Emission Graphs"),
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        selectInput(inputId = "parameter_type",
                    label = "Parameter",
                    choices = c("so2Mass",
                                "noxMass",
                                "co2Mass",
                                "grossLoad",
                                "heatInput")),
        downloadButton('downloademissData', 'Download Emissions Data')
    )),
    mainPanel(
      plotlyOutput("emiss_plot")
    )
  ),
  div(style="margin:20px;")
)