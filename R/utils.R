#For small simple functions
this_month <- function(){rollback(lubridate::today(), roll_to_first = TRUE)}
default_date_range <- function(){
  if(month(this_month()) < 3){
    rollback(c(this_month() - 365, this_month()), roll_to_first = TRUE)
  } else {
    default_date_range <- c(floor_date(this_month(), unit = "year"), this_month())
  }
}

format_time_axis <- function(plotlyObj, date_range){
  
  plotlyObj %>% 
    layout(
      xaxis = list(
        title = list(
          text = "Date"
        ),
        type = "date",
        range = date_range,
        tick0 = date_range[1],
        tickangle = -45,
        tickformatstops = list(
          list(
            dtickrange = list(NULL, "M12"),
            value = "%b %Y"
          ),
          list(
            dtickrange = list("M12", NULL),
            value = "%Y Y"
          )
        )
      )
    )
  
}

line_hovertemplate <- function(customdata = TRUE, remove_extra = TRUE) {
  
  custom <- if_else(customdata, "<i>%{customdata}</i>", NULL)
  extra <- if_else(remove_extra, "<extra></extra>", NULL)
  
  text_lines <- c(
    "<b>%{fullData.name}</b>",
    "%{x|%B %Y}",
    "%{y:$,.2f}",
    custom,
    extra
  ) %>% 
    na.omit() %>% 
    paste0(collapse = "<br>")
}

custom_config <- function(plotlyObj, ...) {
  plotlyObj %>% 
    layout(
      dragmode = "pan"
    ) %>% 
    plotly::config(
      modeBarButtonsToRemove = list(
        "zoom",
        "hoverclosest",
        "autoscale",
        "hovercompare"
      ),
      ...
    )
}

var_to_title <- function(varname){
  varname %>% 
    str_replace_all("_", " ") %>% 
    str_to_title()
}

mortgage_balance <- function(original_loan, rate, n_payments, payment_amt, tax_rate, insurance, pmi, assessed_value, other){
  
  monthly_payment <- payment_amt - (((assessed_value * tax_rate) + insurance + (pmi * original_loan) + other) / 12)
  
  r <- rate / 12
  
  (original_loan * (1 + r) ** n_payments) - 
    (monthly_payment * (((1 + r) ** n_payments) - 1) / r)
}

columns <- function(num_cols = 2, ...){
  checkmate::assertIntegerish(
    x = num_cols,
    lower = 2,
    upper = 12,
    any.missing = FALSE,
    all.missing = FALSE,
    len = 1
  )
  
  content = tagList(...)
  width = 12 / num_cols
  
    map(
      content,
      ~ column(
        width = width,
        .x
      )
    ) %>% 
      fluidRow()
  
}
