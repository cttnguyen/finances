suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinydashboardPlus)
  library(shinycssloaders)
  library(htmlwidgets)
  library(shinyWidgets)
  library(shinyjs)
  library(plotly)
  library(tidyverse)
  library(lubridate)
})

walk(
  c("box", "dashboardHeader", "dashboardPage", "dashboardSidebar", "messageItem", "notificationItem", "taskItem"),
  ~ conflicted::conflict_prefer(.x, "shinydashboardPlus", quiet = TRUE)
)
conflicted::conflict_prefer("lag", "dplyr", quiet = TRUE)
conflicted::conflict_prefer("filter", "dplyr", quiet = TRUE)
conflicted::conflict_prefer("layout", "plotly", quiet = TRUE)
conflicted::conflict_prefer("progressBar", "shinyWidgets", quiet = TRUE)
conflicted::conflict_prefer("show", "shinyjs", quiet = TRUE)

here::here("data") %>% 
  list.files(full.names = TRUE) %>% 
  walk(load, envir = .GlobalEnv)

ui <- dashboardPage(
  header = dashboardHeader(title = "Luckett Finances"),
  sidebar = dbSide,
  body = dashboardBody(
    useShinyjs(),
    tabItems(
      credit_cardsUI("credit_cards"),
      savingsUI("savings"),
      utilitiesUI("utilities")
    )
  ),
  title = "Luckett Finances"
)

server <- shinyServer(function(input, output, session) {
  
  credit_cardsServer("credit_cards")
  savingsServer("savings")
  utilitiesServer("utilities")
  session$onSessionEnded(
    function() {
      stopApp()
    }
  )
  
})

shinyApp(ui, server)