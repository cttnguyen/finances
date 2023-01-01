# Module to generate calendar dates
dateUI <- function(id) {
  tagList(
    pickerInput(
      NS(id, "month_select"),
      label = "Select Date Range",
      choices = c("Year to Date", "Calendar Year", "All Time", "Custom"),
      selected = "Year to Date"
    ),
    fluidRow(
      column(
        6,
        hidden(
          airDatepickerInput(
            NS(id, "month_custom1"),
            label = "From:",
            minDate = as.Date("2021-01-01"),
            maxDate = this_month(),
            view = "months",
            minView = "months",
            addon = "none"
          )
        )
      ),
      column(
        6,
        hidden(
          airDatepickerInput(
            NS(id, "month_custom2"),
            label = "To:",
            minDate = as.Date("2021-01-01"),
            maxDate = this_month(),
            view = "months",
            minView = "months",
            addon = "none"
          )
        )
      )
    )
  )
}

dateServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
     
      minDate <- data %>% 
        select_if(is.Date) %>% 
        pull() %>% 
        min()
      
      observeEvent(input$month_select, {
        
        if(input$month_select == "Custom") {
          show("month_custom1")
          show("month_custom2")
          
        } else {
          hide("month_custom1")
          hide("month_custom2")
          
          walk2(
            c("month_custom1", "month_custom2"),
            date_range(),
            ~ updateAirDateInput(
              session = session, 
              inputId = .x, 
              value = .y,
              options = list(
                minDate = minDate
              )  
            )
          )
          
        }
        
      })
      
      date_range <- reactive({
        
        YTD <- c(floor_date(this_month(), unit = "year"), this_month())
        CY <- c(this_month() - 365, this_month())
        AT <- c(minDate, this_month())
        
        if(input$month_select == "Custom"){
          custom <- c(input$month_custom1, input$month_custom2)
        } else {
          custom <- rep(NA_Date_, 2)
        }
        
        switch(
          input$month_select,
          "Year to Date" = YTD,
          "Calendar Year" = CY,
          "All Time" = AT,
          "Custom" = custom
        )
      })
      
      date_range
       
    }
  )
}