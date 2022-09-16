library(shiny)
library(shinyjs)
library(ggplot2)
library(dplyr)
library(stats) #for calculating standard deviation 
library(DT) #for displaying tables, e.g the dataframe
library(readxl)

# Define server logic required to draw diagrams
shinyServer(function(input, output, session) {
  shinyjs::useShinyjs()
  
  # declare starter variables
  df <- read_excel("C:/Users/themo/Desktop/dataframe-full.xlsx")
  comp <- NULL
  secondComp <- NULL
  metric <- NULL
  year <- NULL
  employees <- NULL
  racial <- NULL
  yearSlider <- NULL
  

  
  # Toggle show/hide input fields according to input
  observeEvent(input$select_data, {
    if (input$select_data == "multi") {
      shinyjs::show("select_second_company")
    } else {shinyjs::hide("select_second_company")}
  })
  
  observeEvent(input$select_metric, {
    if (input$select_metric == "electricity" || input$select_metric == "racial" || input$select_metric == "turnover") {
      shinyjs::show("select_year")
      shinyjs::hide("year_slider")
    } else {shinyjs::hide("select_year")
      shinyjs::show("year_slider")}
    
    if (input$select_metric == "racial") {
     # shinyjs::show("select_employees") #ADD LATER
      shinyjs::show("select_racial")
    } else {shinyjs::hide("select_employees")
        shinyjs::hide("select_racial")}
  })
  
  observeEvent(input$select_weight, {
    if (input$select_weight == 1) {
      shinyjs::show("electricity_w")
      shinyjs::show("racial_w")
      shinyjs::show("turnover_w")
      shinyjs::show("breaches_w")
      shinyjs::show("purposes_w")
      shinyjs::show("water_w")
      shinyjs::show("submit_weights")
    } else {
      shinyjs::hide("electricity_w")
      shinyjs::hide("racial_w")
      shinyjs::hide("turnover_w")
      shinyjs::hide("breaches_w")
      shinyjs::hide("purposes_w")
      shinyjs::hide("water_w")
      shinyjs::hide("submit_weights")}
  })
  
  # Save user Input 'lazily' (lazy evaluation) using the reactive() function
  
  # generalInputs <- reactive({
  #   comp <<- input$select_company 
  #   secondComp <<- input$select_second_company
  #   year <<- input$select_year
  #   metric <<- input$select_metric
  #   employees <<- input$select_employees
  #   racial <<- input$select_racial
  #   yearSlider <<- input$year_slider
  # })
  
  companyInput <- reactive({
    comp <- input$select_company
  })

  secondCompanyInput <- reactive({
    secondComp <- input$select_second_company
  })

  yearInput <- reactive({
    year <- input$select_year
  })
  metricInput <- reactive({
    metric <- input$select_metric
  })
  employeesInput <- reactive({
    employees <- input$select_employees
  })

  racialInput <-reactive({
    racial <- input$select_racial
  })

  yearSliderInput <- reactive({
    yearSlider <- input$year_slider
  })
  
  # read weights from user input for all metrics (starting weights = 1)
  weightInputs <- reactive({
    electricity_weight <<- input$electricity_w
    turnover_weight <<- input$turnover_w
    racial_weight <<- input$racial_w 
    breaches_weight <<- input$breaches_w
    purposes_weight <<- input$purposes_w
  })

  # Visualize on click using user input  
  observeEvent(input$visualize_button, { #observe visualize button click
    
   
    # Create plot according to user input & the chosen metric
    output$Plot <- renderPlot({
      
      #call functions to update df and construct plot
      create_dfs(companyInput())
      theplot <- create_plot(theplot)

      plot(theplot)

    })
    
    #create second plot if prompted by the user
    output$SecondPlot <- renderPlot({
    
      if (input$select_data == "multi") { 
        # call functions to update df and costruct plot
        create_dfs(secondCompanyInput()) 
        secondPlot <- create_plot(secondPlot)
        
      } else (secondPlot = NULL)
      
     # check if secondPlot is Null before drawing to avoid throwing error on UI  
     if (!is.null(secondPlot)) {
       plot(secondPlot)
     }
     
    })
    
    
    
  })  
  
  observeEvent(input$esg_score, {
    output$score <- renderText({paste("ESG SCORE: ", esg_score, "(", company_score ,")")})
  })
    # Main Function Declarations
    
    #function to create/update customised dataframes according to user input
    create_dfs <- function(which_company) {
      
      turnover_df <- df[df$code == "CG-EC-330a.2",]
      turnover_df <- turnover_df[turnover_df$Company == which_company, ] 
      turnover_df <<- turnover_df[turnover_df$year == yearInput(), ] 
      
      electricity_df <<- df[(df$metric_key == 2 | df$metric_key == 3),]
      electricity_df <<- electricity_df[electricity_df$Company == which_company, ]
      electricity_df <<- electricity_df[electricity_df$year == yearInput(), ]
      
      racial_df <-  df[(df$metric_key == 13 | df$metric_key == 14 | df$metric_key == 15),]
      racial_df <- racial_df[racial_df$Company == which_company, ]
      racial_df <- racial_df[racial_df$year == yearInput(), ]
      numbers <- strsplit(racial_df$text, ","); #split string of numbers
      numbers <- do.call(cbind.data.frame, numbers)
      gender_df <- numbers[1:2,] #create two new dfs
      race_df <- numbers[3:7,]
      gender_df <<- as.data.frame(sapply(gender_df, as.numeric)) 
      race_df <<- as.data.frame(sapply(race_df, as.numeric))
      colnames(gender_df) <<- c("management","technical_stuff","all_other_empoyees") #give col and row names
      colnames(race_df) <<- c("management","technical_stuff","all_other_empoyees")
      rownames(race_df) <<- c("asian","black","hispanic","white", "other")
      rownames(gender_df) <<- c("male", "female")
      
      breaches_df <- df[(df$metric_key == 7),]
      breaches_df <- breaches_df[breaches_df$Company == which_company, ]
      breaches_df <<- breaches_df[(breaches_df$year >= input$year_slider[1] & breaches_df$year <= input$year_slider[2]), ]
      
      purposes_df <- df[(df$metric_key == 6),]
      purposes_df <- purposes_df[purposes_df$Company == which_company, ]
      purposes_df <<- purposes_df[(purposes_df$year >= input$year_slider[1] & purposes_df$year <= input$year_slider[2]), ]
      
      water_df <- df[(df$metric_key == 4),] 
      water_df <- water_df[water_df$Company == which_company, ] 
      water_df <<- water_df[(water_df$year >= input$year_slider[1] & water_df$year <= input$year_slider[2]), ]
    }
    
    #function for creating/updating plots 
    create_plot <- function(my_plot) {
      calculate_esg_score() # REMOVE FROM HERE -> ADD TO CORRESPONDING PLACE!!!!!!!!!!!!!
      if (metricInput() == "turnover") {
        my_plot <- ggplot(turnover_df,aes(x = title, y = number)) +
          geom_bar(stat = "identity", width = 0.4, fill = "darkmagenta") +
          geom_text(aes(label = number), vjust=-0.5) +
          labs(x = "",y = "percentage", title = "Turnover Rate", subtitle = "Number of employees that left the business by the average number of employees" ) +
          coord_cartesian(ylim = c(5, 95.5))
        
      } else if (metricInput() == "electricity") {
        my_plot <-ggplot(electricity_df , aes(x = "", y = number, fill = title)) +
          geom_bar(width = 1, stat = "identity", color = "white" ) +
          geom_label(aes(y = number, label = number), color = "black",show.legend = FALSE,position = position_stack(vjust = 0.5)) +
          coord_polar("y", start = 0, direction = -1) +
          labs(title = "Electricity Management", subtitle = "Percentages of grid vs renewable electricity") +
          theme_void()
        
      } else if (metricInput() == "racial" & racialInput() == "race") {
        my_plot <- ggplot(race_df,aes(x = rownames(race_df), y = management)) +
          geom_bar(stat = "identity", width = 0.4, fill = "orange", colour = "black") +
          geom_text(aes(label = management), vjust=-0.5) +
          labs(x = "Race",y = "percentage", title = "employees in management by race") + coord_cartesian(ylim = c(5, 95.5))
      } else if (metricInput() == "racial" & racialInput() == "gender") {
        my_plot <- ggplot(gender_df,aes(x = rownames(gender_df), y = management)) +
          geom_bar(stat = "identity", width = 0.4, fill = "orange", colour = "black") +
          geom_text(aes(label = management), vjust=-0.5) +
          labs(x = "gender",y = "percentage", title = "employees in management by gender") + coord_cartesian(ylim = c(5, 95.5))
        
      } else if (metricInput() == "breaches") { 
        breaches_df$year <- as.factor(breaches_df$year) # make year, number columns factorials
        breaches_df$number <- as.factor(breaches_df$number)
        my_plot <- ggplot(breaches_df,aes(x = year,y = number,group = 1)) +
          geom_line(size = 1.5,color = "lightgrey",) +
          geom_point(size = 5,color = "orange") +
          labs(y = "Data Breaches",x = "Year", title = "Number of Data Breaches Per year")
        
      }  else if (metricInput() == "purposes") { 
        purposes_df$year <- as.factor(purposes_df$year) # make year, number columns factorials
        purposes_df$number <- as.factor(purposes_df$number)
        my_plot <- ggplot(purposes_df,aes(x = year,y = number,group = 1)) +
          geom_line(size = 1.5,color = "lightgrey",) +
          geom_point(size = 5,color = "orange") +
          labs(y = "Numberof users",x = "Year", title = "Number of Users whose data is used for secondary purposes")
        
      } else if (metricInput() == "water") {
        water_df$year <- as.factor(water_df$year) # make year, number columns factorials
        water_df$number <- as.factor(water_df$number)
        my_plot <- ggplot(water_df,aes(x = year,y = number,group = 1)) +
          geom_line(size = 1.5,color = "lightgrey",) +
          geom_point(size = 5,color = "green") +
          labs(y = "Thousand cubic meters",x = "Year", title = "Total Water withdrawn")

      } else {my_plot = NULL} #remove later
       
      
      
    }

    
    
    #function to calculate esg score for specific company and year
     calculate_esg_score <- function() {
       # calculate score (A,B,C..E) for each metric and convert to number (5,4,3..1)
       electricity_metric_score <- score_to_number(calculate_electricity_score())
       turnover_metric_score <- score_to_number(calculate_turnover_score())
       breaches_metric_score <- score_to_number(calculate_breaches_score())
       racial_metric_score <- score_to_number(calculate_racial_score())
       purposes_metric_score <- score_to_number(calculate_purposes_score())
       water_metric_score <- score_to_number(calculate_water_score())
       
       #calculate overall company esg score
       company_score <<- ((electricity_metric_score + water_metric_score)/100 *33.3) + ((racial_metric_score + purposes_metric_score)/100 *33.3) +
       ((turnover_metric_score + breaches_metric_score)/100 *33.3)
       print(company_score)
       
       #convert esg score to letter and publish
       esg_score <<- number_to_score(company_score) 
       
     }

     
     #Helper Function Declarations
 
      calculate_electricity_score <- function() {
        electricity_value <- electricity_df[electricity_df$metric_key == 3,]
        electricity_value$number <- electricity_value$number * weightInputs()[electricity_weight]
        if (electricity_value$number < 21) {
         electricity_score <<- "E"
        } else if (electricity_value$number <41) {
           electricity_score <<- "D"
        } else if (electricity_value$number <61) {
           electricity_score <<- "C"
        } else if (electricity_value$number <81) {
           electricity_score <<- "B"
        }  else {elecitricity_score <<- "A"}
        cat("Company ", companyInput(), " Electricity Score:", electricity_score, "\n" )
        return(electricity_score)
      }
       
      
      calculate_turnover_score <- function() {
        turnover_value <- turnover_df[turnover_df$metric_key == 11,]$number + turnover_df[turnover_df$metric_key == 12,]$number
        turnover_value <- turnover_value * weightInputs()[turnover_weight]
        if (turnover_value <= 2) {
          turnover_score <- "A"
         } else if (turnover_value <= 4) {
          turnover_score <- "B"
         } else if (turnover_value <= 6) {
          turnover_score <- "C"
         } else if (turnover_value <= 8) {
            turnover_score <- "D"
         } else {turnover_score <- "E"}
        cat("Company ", companyInput(), " Turnover Score:", turnover_score, "\n" )
        return(turnover_score)
        }
        
      
      calculate_breaches_score <- function() {
        
          breaches_value <- breaches_df[breaches_df$year == input$year_slider[2],]$number
          breaches_value <-breaches_value * weightInputs()[breaches_weight]
        if (breaches_value <= 2) {
          breaches_score <- "A"
        } else if (breaches_value <= 4) {
          breaches_score <- "B"
        } else if (breaches_value <= 6) {
          breaches_score <- "C"
        } else if (breaches_value <= 8) {
          breaches_score <- "D"
        } else {breaches_score <- "E"}
        cat("Company ", companyInput(), " Breaches Score:", breaches_score, "\n" )
        return(breaches_score)
      }
      
      calculate_racial_score <- function() {
        #calculate standard deviation for racial metrics
        standard_div <- sd(race_df$management) + sd(gender_df$management)
        
        if (standard_div <= 10) {
          racial_score <- "E"
        } else if (standard_div <= 20) {
          racial_score <- "D"
        } else if (standard_div <= 30) {
          racial_score <- "C"
        } else if (standard_div <= 40) {
          racial_score <- "B"
        } else {racial_score <- "A"}
        cat("Company ", companyInput(), " Racial Score:", racial_score, "\n" )
        return(racial_score)
      }
      
      calculate_purposes_score <- function() {
        # declare number of users for each company (if not submitted in metrics)
        if (companyInput() == "A") {
          users = 2000
        } else {users = 5000}
        
        #calculate percentage of users whose data is use out of all users
        purposes_value <- purposes_df[purposes_df$year == input$year_slider[2],]$number
        percentage <- purposes_value /users * 100
        
        if (percentage <= 20) {
          purposes_score <- "A"
        } else if (percentage <= 40) {
          purposes_score <- "B"
        } else if (percentage <= 60) {
          purposes_score <- "C"
        } else if (percentage <= 80) {
          purposes_score <- "D"
        } else {purposes_score <- "E"}
        cat("Company ", companyInput(), " purposes Score:", purposes_score, "\n" )
        return(purposes_score)
      }
      
      calculate_water_score <- function() {
        water_value <- water_df[water_df$year == input$year_slider[2],]$number
        water_value <-water_value * weightInputs()[breaches_weight]
        
        if (water_value <= 150) {
          water_score <- "A"
        } else if (water_value <= 300) {
          water_score <- "B"
        } else if (water_value <= 450) {
          water_score <- "C"
        } else if (water_value <= 600) {
          water_score <- "D"
        } else {water_score <- "E"}
        cat("Company ", companyInput(), " Water Score:", water_score, "\n" )
        return(water_score)
      }
      
      
      score_to_number <- function(score) {
        if (score == "A") {
          num <- 5
        } else if (score == "B") {
          num <- 4
        } else if (score == "C") {
          num <- 3
        } else if (score == "D") {
          num <- 2 
        } else {num <- 1}
        cat(num, "\n")
        return(num)
      }
      
      number_to_score <- function(score) {
        if (score >= 8) {
          letter <- "A"
        } else if (score >= 6) {
         letter <- "B"
        } else if (score >= 4) {
          letter <- "C"
        } else if (score >= 2) {
          letter <- "D"
        } else {letter <- "E"}
        cat(letter, "\n")
        return(letter)
      }
      
  
  
  # Button to toggle Dataframe View
  observeEvent(input$view_df, {  #observe dataframe button click
    if (input$view_df %% 2 == 1) {
        output$table = renderDT(
        df, options = list(lengthChange = TRUE))
    } else {output$table = NULL}
  })

}) 