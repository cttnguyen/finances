credit_cardsUI <- function(id){
  tabItem(
    tabName = id,
      h1("Credit Cards"),
      fluidRow(
        box(
          width = 3,
          dateUI(
            NS(
              id, 
              "date_range"
            )
          )
        ),
        box(
          width = 9,
          h2("Total Credit Card Expenditures by Account"),
          withSpinner(
            type = 5,
            color = "#060606",
            areaPlotUI(
              NS(
                id, 
                "area"
              )
            )
          ),
          br(),
          h2("Total Credit Card Expenditures"),
          withSpinner(
            type = 5,
            color = "#060606",
            linePlotUI(
              NS(
                id, 
                "line"
              )
            )
          )
          
        )
      )
  )
}




credit_cardsServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session){
      
      #interpret input date range
      date_range <- dateServer("date_range", credit_cards)
      
      #Create area plot
      areaPlotServer("area", date_range)
      
      #Create line plot
      credit_cards %>%
        select(payment_date, total, adjusted_total, notes) %>%
        pivot_longer(contains("total")) %>%
        mutate(name = var_to_title(name)) %>% 
        linePlotServer(id = "line", data = ., date_range = date_range)      
    }
  )
  
}
