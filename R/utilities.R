utilitiesUI <- function(id){
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
          ),
        ),
        p("*Water is paid semimonthly")
      )
    )
  )
}




utilitiesServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session){
      
      #interpret input date range
      date_range <- dateServer("date_range", savings)
      
      #Create line plot
      utilities %>% 
        pivot_longer(cols = gas:internet) %>%
        mutate(name = var_to_title(name)) %>%
        filter(!is.na(value)) %>% 
        linePlotServer(id = "line", date_range = date_range)      
    }
  )
  
}
