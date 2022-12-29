filter_cc <- function(month_range) {
  credit_cards %>% 
    filter(
      payment_date >= min(month_range),
      payment_date <= max(month_range)
    )
}

cc_area_plot <- function(date_range) {
  cc <- credit_cards %>% 
    select(-contains("total"))
  
  fig_base <- plot_ly(
    data = cc,
    type = "scatter",
    mode = "none",
    stackgroup = "one"
  )  
  
  fig <- cc %>% 
    select_if(is.numeric) %>% 
    names() %>% 
    reduce(
      function(plotlyobj, varname, ...) {
        add_trace(
          plotlyobj,
          x = ~payment_date,
          y = as.formula(paste0("~", varname)),
          name = str_to_title(varname),
          text = ~notes,
          customdata = ~notes,
          hovertemplate = paste0("<b>", varname, "</b><br>%{x}<br>%{y:$,.2f}<br>%{customdata}")
        )
      },
      .init = fig_base
    ) %>% 
    layout(
      xaxis = list(
        title = list(
          text = "Payment Date"
        ),
        range = date_range
      ),
      yaxis = list(
        title = list(
          text = "Amount ($)"
        ),
        range = c(0, 15000)),
      title = list(
        text = "Monthly Credit Card Expenditures by Account"
      )
    )
  fig
  
}


cc_totals_plot <- function(date_range){
  credit_cards %>%
    select(payment_date, total, adjusted_total, notes) %>%
    pivot_longer(contains("total")) %>%
    plot_ly(
      type = "scatter",
      mode = "lines",
      x = ~payment_date,
      y = ~value,
      color = ~name,
      customdata = ~paste0(notes, "<br>", name),
      hovertemplate = "%{x}<br>%{y:$,.2f}<br>%{customdata}"
    ) %>% 
    layout(
      xaxis = list(
        title = list(
          text = "Payment Date"
        ),
        range = date_range
      ),
      yaxis = list(
        title = list(
          text = "Amount ($)"
        ),
        range = c(0, 15000)
      ),
      title = list(
        text = "Totals"
      )
    )
}

