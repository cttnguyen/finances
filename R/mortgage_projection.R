mortgage_projectionUI <- function(id){
  tagList(
    columns(
      num_cols = 2,
      mortgageNumericInput(
        NS(id, "sale_price"),
        label = "Sale Price",
        value = 620000
      ),
      mortgageNumericInput(
        NS(id, "downpayment"),
        label = "Downpayment (%)",
        value = 10,
        type = "integer"
      )
    ),
    columns(
      num_cols = 2,
      mortgageNumericInput(
        NS(id, "loan_amount"),
        label = "Loan Amount",
        value = 558000
      ),
      mortgageNumericInput(
        NS(id, "rate"),
        label = "Interest Rate (%)",
        value = 0.03875,
        type = "percent3"
      )
    ),
    columns(
      num_cols = 2,
      mortgageNumericInput(
        NS(id, "loan_term"),
        label = "Loan Term (years)",
        value = 30,
        type = "integer"
      ),
      airDatepickerInput(
        NS(id, "month_start"),
        label = "Start Month:",
        minDate = as.Date("2000-01-01"),
        maxDate = this_month() + years(30),
        value = as.Date("2022-04-01"),
        view = "months",
        minView = "months",
        addon = "none",
        dateFormat = "yyyy-MM"
      )
    ),
    columns(
      num_cols = 2,
      mortgageNumericInput(
        NS(id, "home_ass_value"),
        label = "Taxable Value",
        value = 565000
      ),
      mortgageNumericInput(
        NS(id, "tax_rate"),
        label = "Property Tax Rate",
        value = 0.0085,
        type = "percent2"
      )
    ),
    columns(
      num_cols = 2,
      mortgageNumericInput(
        NS(id, "home_app_value"),
        label = "Appraised Value",
        value = 630000
      ),
      mortgageNumericInput(
        NS(id, "pmi"),
        label = "PMI",
        value = 0.0013,
        type = "percent2"
      )
    ),
    mortgageNumericInput(
      NS(id, "monthly_payment"),
      label = "Monthly Payment",
      value = 3200.05
    ),
    columns(
      num_cols = 2,
      mortgageNumericInput(
        NS(id, "insurance"),
        label = "Insurance",
        value = 1273
      ),
      mortgageNumericInput(
        NS(id, "other"),
        label = "Other (e.g., HOA)",
        value = 112.7
      )
    )
  )
}



mortgage_projectionServer <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      
      observeEvent(c(input$sale_price, input$downpayment),{
        updateCurrencyInput(
          session = session,
          inputId = "loan_amount",
          value = input$sale_price * (1 - input$downpayment / 100)
        )
      })
      
      
      df <- reactive({
        validate(
          need(
            input$month_start,
            ""
          )
        )

        tibble(
          date = seq(
            from = input$month_start,
            to = input$month_start %m+% months(12 * input$loan_term),
            by = "month")
        ) %>%
          mutate(
            payment_num = 1:n() - 1,
            remaining_balance = mortgage_balance(
              original_loan = input$loan_amount,
              rate = input$rate,
              n_payments = payment_num,
              payment_amt = input$monthly_payment,
              assessed_value = input$home_ass_value,
              tax_rate = input$tax_rate,
              insurance = input$insurance,
              pmi = input$pmi,
              other = input$other
            ),
            loan_to_value = remaining_balance * 100 / input$home_app_value
          ) %>%
          select(-payment_num)
      })

      df
      
    }
  )
}
 
mortgageNumericInput <- function(inputId, label, value, type = "currency"){

  checkmate::assert_choice(
    x = type,
    choices = c(
      "currency",
      "percent2",
      "percent3",
      "numeric",
      "integer"
    )
  )

  input_list <- list(
    inputId = inputId,
    label = label,
    value = value,
    align = "right",
    format = case_when(
      type == "currency" ~ "NorthAmerican",
      type == "integer" ~ "integerPos",
      type == "percent2" ~ "percentageUS2dec",
      type == "percent3" ~ "percentageUS3dec",
      TRUE ~ NA_character_
    )
  )

  input_fn <- switch(
    type,
    "currency" = currencyInput,
    formatNumericInput
  ) 
  
  do.call(input_fn, input_list)
  
}

