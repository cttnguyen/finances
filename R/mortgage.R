mortgageUI <- function(id){
  tabItem(
    tabName = id,
    h1(var_to_title(id)),
    fluidRow(
      box(
        width = 3,
        mortgage_projectionUI(
          NS(
            id,
            "mortgage_projection"
          )
        )
      ),
      box(
        width = 9,
        withSpinner(
          type = 5,
          color = "#060606",
          linePlotUI(NS(id, "line"))
        )
      )
    )
  )
}




mortgageServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session){
      
      #interpret input date range
      mortgage_projection_df = mortgage_projectionServer("mortgage_projection")
      date_range <- reactive({
        mortgage_projection_df() %>% 
          select_if(is.Date) %>% 
          pull() %>% 
          range()
      })
      vlines_df <- reactive({
        tibble(
          date = c(
            this_month(),
            mortgage_projection_df() %>%
              filter(loan_to_value <= 80) %>%
              pull(date) %>%
              first()        
          ),
          value = rep(max(mortgage_projection_df()$remaining_balance), 2),
          name = c("Present Time", "Loan to Value < 80%"),
        ) %>% 
          left_join(mortgage_projection_df(), by = "date") %>% 
          mutate(
            notes = scales::dollar(remaining_balance, accuracy = 0.01)
          ) 
      })


      # output$plot <- renderPlotly({
        # mortgage_projection_df() %>%
        #   pivot_longer(remaining_balance) %>%
        #   linePlot("date") %>%
      #     layout(
      #       shapes = list(
      #         vline(ltv_80()),
      #         vline(this_month())
      #       )
      #     )
      # })
        
      observe({
        mortgage_projection_df() %>%
          pivot_longer(remaining_balance) %>%
          mutate(name = var_to_title(name)) %>% 
          linePlotServer(
            id = "line", 
            data = ., 
            date_range = date_range,
            addl_traces = list(
              "add_segments" = list(
                data = vlines_df(),
                xend = ~date,
                yend = c(0, 0),
                hovertemplate = str_remove(line_hovertemplate(), "<br>%\\{y\\:\\$,\\.2f\\}")
              )
            )
          )
      })
      
      # output$input <- renderPrint(hi())
      vline <- function(x = 0, color = "green") {
        list(
          type = "line",
          y0 = 0,
          y1 = 1,
          yref = "paper",
          x0 = x,
          x1 = x,
          line = list(color = color, dash="dot")
        )
      }
      # hline <- function(y = 0, color = "red") {
      #   list(
      #     type = "line",
      #     y0 = y,
      #     y1 = y,
      #     x0 = 0,
      #     x1 = 1,
      #     xref = "paper",
      #     line = list(color = color, dash = "dot")
      #   )
      # }
      
     
        # plot_ly(
        #   type = "scatter",
        #   mode = "lines",
        #   mortgage_projection(),
        #   x = ~ date,
        #   y = ~ remaining_balance,
        #   name = "Remaining Balance"
        # ) %>% 
        #   add_lines(
        #     x = ~ range(date),
        #     y = rep(0, 2),
        #     # color = ~ c("Today", "80% LTV"),
        #     # color = ~I(c("green", "black")),
        #     color = I(c("black", "red")),
        #     hoverinfo = "none",
        #     name = ~ c("Today", "80% LTV")
        #   ) %>% 
        #   layout(
        #     shapes = list(
        #       vline(this_month(), "black"),
        #       vline(ltv80, "red")
        #     )
        #   )
      # })
      
    }
  )
  
}
