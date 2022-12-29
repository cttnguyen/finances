suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinydashboardPlus)
  library(shinycssloaders)
  library(htmlwidgets)
  library(shinyWidgets)
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

here::here("data") %>% 
  list.files(full.names = TRUE) %>% 
  walk(load, envir = .GlobalEnv)

