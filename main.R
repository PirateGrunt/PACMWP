library(shiny)
library(DT)
library(imaginator)
library(tidyverse)

tabMain <- tabPanel(
    title = "Main"
  , sidebarLayout(
    sidebarPanel(
        numericInput("claims_per_year", "Claims per year", value = 1000)
      # , numericInput("start_year", "Start Year", value = 2010)
      # , numericInput("end_year", "End Year", value = 2020)
      # , sliderInput("retention", "Retention", value = 0.8, min = 0, max = 1)
      # , sliderInput("growth", "Growth", value = 0.0, min = 0, max = 1)
      # , actionButton("btn_simulate", "Simulate")
    ),
    mainPanel(
        DT::dataTableOutput("tbl_claim")
      , plotOutput('plt_claim')
      , DT::dataTableOutput('tbl_AY')
      , plotOutput('plt_AY')
    )
  ) # sidebarLayout
)   # tabPanel

main_output <- quote({

  tblClaim <- reactive({
    imaginator::SimulatePolicies(
              input$claims_per_year
            , PolicyYears = 2011:2020
            , Retention = 1
            , Growth = 0) %>%
      select(ClaimID = PolicyholderID, OccurrenceDate = PolicyEffectiveDate) %>%
      mutate(
        Indemnity = rgamma(n(), 30, 0.2)
        , t = as.double((OccurrenceDate - as.Date('2011-01-01')) / 365.25)
        , inflation = 1.05 ^ t
        , Indemnity = Indemnity * inflation
      )

  })

  tblAY <- reactive({
    tblClaim() %>%
      mutate(AY = lubridate::year(OccurrenceDate)) %>%
      group_by(AY) %>%
      summarize(
        MeanIndemnity = mean(Indemnity)
      )
  })

  output$tbl_claim <- DT::renderDataTable({
    tblClaim() %>%
      DT::datatable(
          rownames = FALSE
        , selection = "single"
        , style = "bootstrap")
  })

  output$tbl_AY <- DT::renderDataTable({
    tblAY() %>%
      DT::datatable(
          rownames = FALSE
        , selection = "single"
        , style = "bootstrap")
  })

  output$plt_claim <- renderPlot({
    tblClaim() %>%
      ggplot(aes(OccurrenceDate, Indemnity)) +
      geom_point()

  })

  output$plt_AY <- renderPlot({
    tblAY() %>%
      ggplot(aes(AY, MeanIndemnity)) +
      geom_point()

  })
})
