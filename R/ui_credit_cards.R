cc_tab <- tabItem(
  tabName = "credit_cards",
  # fluidPage(
    h1("Credit Cards"),
    fluidRow(
      box(
        width = 3,
          airDatepickerInput(
            "cc_month",
            label = "Select Date Range:",
            range = TRUE,
            minDate = as.Date("2021-01-01"),
            maxDate = as.Date("2023-01-01"),
            value = as.Date(c("2022-01-01", "2023-01-01")),
            # dateFormat = "yyyy-MM",
            view = "months",
            minView = "months",
            addon = "none",
            inline = TRUE
          )
        ),
      box(
        width = 9,
        verbatimTextOutput("temp"),
        withSpinner(
          type = 5,
          color = "#060606",
          plotlyOutput("cc_area_plot")  
        ),
        br(),
        withSpinner(
          type = 5,
          color = "#060606",
          plotlyOutput("cc_totals_plot")
        )
        
      )
    )
)