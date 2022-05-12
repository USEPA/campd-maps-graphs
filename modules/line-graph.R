lineGraphUI <- function(id) {
  ns <- NS(id)
  tagList(
    textOutput(ns("title")),
    plotlyOutput(ns("linePlot"))
  )
}

lineGraphSever <- function(input, output, session, 
                           df, xVals, yVals, group, graphTitle, 
                           xtitle, ytitle, hoverText, xAxisIsYears=FALSE){
  
  m <- list(
    l = 20,
    r = 20,
    b = 20,
    t = 50,
    pad = 20
  )
  
  colorPalette = c("black",
                   "orange",
                   "plum",
                   "darkgreen",
                   "blue")
  
  output$linePlot <- renderPlotly({
    
    fig <- plot_ly(df, x = ~df[[xVals]], y = ~df[[yVals]], 
                   color = ~df[[group]], colors = colorPalette, 
                   text = hoverText,
                   hoverinfo = "text", 
                   symbol = ~df[[group]],
                   marker = list(size = 8),
                   type = "scatter",
                   mode = "lines+markers") %>%
      layout(title=paste(strwrap(graphTitle, width = 50),collapse = "\n"),
             xaxis = list(title = xtitle, 
                          tickangle=-45),
             yaxis = list(title = ytitle, 
                          range = list(0, (max(df[[yVals]])*1.1)),
                          tickformat=",d", 
                          tickangle=-45),
             margin = m,
             showlegend=TRUE
             )
    
    if (xAxisIsYears){
      fig <- fig %>% layout(xaxis = list(title = xtitle,
                                         tickvals=~df[[xVals]],
                                         tickformat='d'))
    }
    
    #fig <- config(fig, displayModeBar = TRUE)
    
    fig
  })
}

