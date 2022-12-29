ui <- dashboardPage(
  header = dashboardHeader(title = "Luckett Finances"),
  sidebar = dbSide,
  body = dashboardBody(
    tabItems(
      cc_tab,
      savings_tab
    )
  ),
  title = "Luckett Finances"
)