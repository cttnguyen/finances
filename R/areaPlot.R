areaPlotUI <- function(id) {
  plotlyOutput(NS(id, "fig"))
}

areaPlotServer <- function(id, date_range) {
  stopifnot(is.reactive(date_range))
  moduleServer(
    id,
    function(input, output, session) {
      
      output$fig <- renderPlotly({
        validate(need(date_range(), ""))
        
        format_time_axis(
          areaPlot(),
          date_range()
        )
      })
      
    }
  )
}

areaPlot <- function() {
  cc <- credit_cards %>% 
    select(-contains("total"))
  
  cc_levels <- cc %>%
    select_if(is.numeric) %>% 
    pivot_longer(everything()) %>% 
    group_by(name) %>% 
    summarise(total = sum(value, na.rm = TRUE), .groups = "drop") %>% 
    arrange(total) %>% 
    transmute(name = fct_inorder(name)) %>% 
    pull()
  
  
  
  fig_base <- plot_ly(
    data = cc,
    type = "scatter",
    mode = "none",
    stackgroup = "one"
  )  
  
  cc_levels %>% 
    reduce(
      function(plotlyobj, varname, ...) {
        add_trace(
          plotlyobj,
          x = ~payment_date,
          y = as.formula(paste0("~", varname)),
          name = str_to_title(varname),
          text = ~notes,
          customdata = ~notes,
          hovertemplate = line_hovertemplate()
        )
      },
      .init = fig_base
    ) %>% 
    layout(
      yaxis = list(
        title = list(
          text = "Amount ($)"
        ),
        fixedrange = TRUE
      )
    ) %>% 
    custom_config()
}
