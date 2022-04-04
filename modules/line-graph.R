lineGraphUI <- function(id) {
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("linePlot"))
  )
}

lineGraphSever <- function(input, output, session, 
                           df, xVals, yVals, group, graphTitle, 
                           xtitle, ytitle, xAxisIsYears=FALSE){
  
  output$linePlot <- renderPlotly({
    
    "p <- ggplot(data=df, aes(x=.data[[xVals]], y=.data[[yVals]], 
                             group=.data[[group]])) +
      geom_line(aes(color=.data[[group]]))+
      geom_point(aes(color=.data[[group]]))+
      labs(title = graphTitle,
           y = ytitle,
           x = xtitle,
           colour = group)+
      theme(legend.key.width = unit(1, 'cm'),)+
      theme(legend.title = element_text(size=14))
    
    if(!is.null(facetWrapVal)){
      p <- p + facet_wrap( ~ df[[facetWrapVal]], ncol=2)
    }
    
    if (xAxisIsYears){
      p <- p + scale_x_continuous(breaks = seq(min(df[[xVals]]),max(df[[xVals]]),by=1))+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }"
    
    #fig <- ggplotly(p)
    
    fig <- plot_ly(df, x = ~df[[xVals]], y = ~df[[yVals]], 
                   color = ~df[[group]], colors = "Set2", 
                   mode = "markers")%>%
      add_lines() %>% 
      layout(title=graphTitle,
             xaxis = list(title = xtitle),
             yaxis = list(title = ytitle)
             )
    
    if (xAxisIsYears){
      fig <- fig %>% layout(xaxis = list(title = xtitle, tickformat='d'))
    }
    
    #fig <- config(fig, displayModeBar = TRUE)
    
    fig
  })
}

