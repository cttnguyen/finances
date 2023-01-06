linePlotUI <- function(id) {
  plotlyOutput(NS(id, "fig"))
}

linePlotServer <- function(id, data, date_range, addl_traces = list()) {
  stopifnot(is.reactive(date_range))
  moduleServer(
    id,
    function(input, output, session) {
      
      
      date_var <- data %>% 
        select_if(is.Date) %>% 
        names()
      
      output$fig <- renderPlotly({
        validate(need(date_range(), ""))
        validate(
          need(
            length(date_var) == 1, 
            "Too many date variables in dataset"
          ),
          need(
            !is.na(date_var),
            "No date variable in dataset"
          )
        )
        format_time_axis(
          linePlot(
            data, 
            date_var,
            addl_traces
          ), 
          date_range()
        )
        
      })
      
    }
  )
}

linePlot <- function(data, date_var, addl_traces = list()){
  plotlyObj <- data %>% 
    {if(!"notes" %in% names(data)){
      mutate(., notes = "")
    } else .} %>% 
    plot_ly(
      type = "scatter",
      mode = "lines",
      x = as.formula(paste("~", date_var)),
      y = ~value,
      color = ~name,
      customdata = ~notes,
      hovertemplate = line_hovertemplate()
    ) %>% 
    layout(
      hovermode = "x",
      yaxis = list(
        title = list(
          text = "Amount ($)"
        ),
        fixedrange = TRUE
      )
    ) %>% 
    custom_config()
  
  if(length(addl_traces) == 0){
    return(plotlyObj)
  } else{
    reduce2(
      names(addl_traces),
      addl_traces,
        function(plotlyObj, trace_name, args, ...){
          list(p = plotlyObj) %>% 
            append(args) %>% 
            do.call(what = trace_name)
        },
      .init = plotlyObj
    ) %>% 
      return()
  }
}

