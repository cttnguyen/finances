savingsUI <- function(id){
  tabItem(
    tabName = id,
    h1(var_to_title(id)),
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




savingsServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session){
      
      #interpret input date range
      date_range <- dateServer("date_range", savings)
      
      #Create line plot
      savings %>% 
        pivot_longer(cols = amount:balance) %>%
        mutate(
          name = case_when(
            name == "amount" ~ "Contribution",
            name == "balance" ~ "Total Balance",
            T ~ NA_character_
          )
        ) %>%
        linePlotServer(id = "line", date_range = date_range)      
    }
  )
  
}
