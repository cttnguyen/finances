#For small simple functions
today <- function(){rollback(lubridate::today(), roll_to_first = TRUE)}
default_date_range <- function(){
  if(month(today()) < 3){
    rollback(c(today() - 365, today()), roll_to_first = TRUE)
  } else {
    default_date_range <- c(floor_date(today(), unit = "year"), today())
  }
}

count_months <- function(date_range){
  date_range %>% 
    int_diff %/%
    months(1) +
    1
}

format_time_axis <- function(plotlyObj, date_range){
  
  plotlyObj %>% 
    layout(
      xaxis = list(
        title = list(
          text = "Date"
        ),
        range = date_range,
        tick0 = date_range[1],
        nticks = count_months(date_range),
        tickangle = -45,
        tickformat = "%b %Y"
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