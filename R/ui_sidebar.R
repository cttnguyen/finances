dbSide <- function(){ 
  dashboardSidebar(
    sidebarMenu(
      menuItem("Credit Cards", tabName = "credit_cards", icon = icon("credit-card")),
      menuItem("Insurance", tabName = "insurance", icon = icon("umbrella")),
      menuItem("Mortgage", tabName = "mortgage", icon = icon("house")),
      menuItem("Savings", tabName = "savings", icon = icon("piggy-bank")),
      menuItem("Subscriptions", tabName = "subscriptions", icon = icon("rotate")),
      menuItem("Utilities", tabName = "utilities", icon = icon("plug")),
      menuItem(
        "Retirement", icon = icon("person-cane"),
        menuSubItem("Projections", tabName = "projections", icon = icon("arrow-trend-up")),
        menuSubItem("Net Worth", tabName = "net_worth", icon = icon("hand-holding-dollar"))
      )
    )
  )
}
