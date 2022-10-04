library(shiny)
library(shinyjs)
library(ggplot2)
library(DT) #for displaying tables, e.g the dataframe
library(dplyr)

# Define UI for application
shinyUI(fluidPage(
  shinyjs::useShinyjs(),
  # Application title
  titlePanel("ESG Data Visualization Tool"),
  h4("Created with R and the Shiny package"),
  
  # Sidebar for user input
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        "select_data",
        "Use data from one company or compare?",
        choices = c("Single Company " = "single", "Compare" = "multi")
      ),
      br(),
      radioButtons(
        "select_company",
        "Select Company",
        choices = c("Company A" = "A",
                    "Company B" = "B")
      ),
      br(),
      shinyjs::hidden(
        radioButtons(
          "select_second_company",
          "Select Company to compare values with",
          choices = c("Company A" = "A",
                      "Company B" = "B")
        )
      ),
      br(),
      selectInput(
        "select_metric",
        "Select metric to visualize",
        choices = c(
"total electricity consumed (percentage of grid & renewable)" = "electricity",
"Percentage of gender and racial group representation" = "racial",
"employee turnover  rate" = "turnover",
"Number of data breaches" = "breaches",
"Number of users whose information is used for secondary purposes" = "purposes",
"Total water withdrawn" = "water"
        )
      ),
      br(),
      shinyjs::hidden(
        radioButtons(
          "select_employees",
          "Choose employees' role ",
          choices = c(
            "Management" = "management",
            "Technical stuff" = "technical_stuff",
            "All other employees" = "all_other_employees"
          )
        )
      ),
      br(),
      shinyjs::hidden((
        radioButtons(
          "select_racial",
          "Choose the values you want to display",
          choices = c("Race" = "race",
                      "Gender" = "gender")
        )
      )),
      br(),
      (selectInput(
        "select_year",
        "Select Year",
        choices = c(
          "2020" = 2020,
          "2021" = 2021,
          "2022" = 2022
        )
      )),
      br(),
      shinyjs::hidden((
        sliderInput(
          "year_slider",
          "Select Year Range",
          min = 2020,
          max = 2022,
          step = 1,
          value = c(2020, 2022),
          ticks = FALSE,
          dragRange = TRUE
        )
      )),
      
      br(),
      actionButton("visualize_button", "Visualize !",  class = "btn-success"),
      
      actionButton("view_df", "View/Hide Dataframe"),
      br(),
      br(),
      br(),
      radioButtons(
        "select_weight",
        "Add custom weights to metrics ? ",
        choices = c("Yes" = 1, "No" = 0),
        selected = 0,
      ),
      br(),
      shinyjs::hidden(numericInput(
        "e_w", label = p("E Pillar weight:"), value = 1
      )),
      shinyjs::hidden(numericInput(
        "s_w", label = p("S Pillar weight:"), value = 1
      )),
      shinyjs::hidden(numericInput(
        "g_w", label = p("G Pillar weight"), value = 1
      )),
      
      br(),
    
      br(),
      br(),
      actionButton("esg_score", "Show ESG Scores", class = "btn-success")
      
    ),
    
    # Output
    mainPanel(
      DTOutput("score"),
      br(),
      plotOutput("Plot"),
      # Show a plot of the generated distribution
      plotOutput("SecondPlot"),
      DTOutput("table")
      
    )
  )
))