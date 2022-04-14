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
  
  colorPalette = c("black",
                   "orange",
                   "plum",
                   "yellow",
                   "blue")
  
  output$linePlot <- renderPlotly({
    
    fig <- plot_ly(df, x = ~df[[xVals]], y = ~df[[yVals]], 
                   color = ~df[[group]], colors = colorPalette, 
                   
                   mode = 'lines+markers',hovertemplate=hoverText)%>%#, linetype = ~df[[group]])%>%
      add_markers(symbol=~df[[group]],showlegend = FALSE,size = 10) %>% 
      add_lines() %>%
      layout(title=paste(strwrap(graphTitle,width = 50),collapse = "\n"),
             xaxis = list(title = xtitle, 
                          tickangle=-45),
             yaxis = list(title = ytitle, 
                          range = list(0, (max(df[[yVals]])*1.1)),
                          tickformat=",d", 
                          tickangle=-45)
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

