server <- shinyServer(function(input, output, session) {
  
  output$cc_area_plot <- renderPlotly(cc_area_plot(input$cc_month))
  output$cc_totals_plot <- renderPlotly(cc_totals_plot(input$cc_month))
  
    session$onSessionEnded(
      function() {
        stopApp()
      }
    )
  }
)