# LIBRARIES ----

# Shiny
library(shiny)
library(bslib)

# Widgets
library(plotly)
library(DT)

# Core
library(tidyverse)


# LOAD DATASETS ----

#Modify exposures df
exposures_mod <- select(exposures, -"Latitude", -"Longitude")
exposures_mod$Location <- as.character(exposures_mod$Location)

#Define portfolio (aggregate) data frame
portfolio <- exposures_mod %>% 
  group_by(PolicyYear) %>% 
  summarize('Total Insured Value' = sum(`Total Insured Value`),
            'Premium' = sum(Premium),
            'Losses - Non Catastrophe' = sum(`Losses - Non Catastrophe`))
portfolio$Location <- "Portfolio"

#Define custom function to subset exposures 
subset_location <- function(df, i){
  location_exposures <<- df %>% filter(Location == i)
}

#Create the data list for each location's data
data_list <- list("Portfolio" = portfolio, "All" = exposures)

for (i in 1:35){
  subset_location(exposures_mod,i)
  data_list[[as.character(i)]] <- location_exposures
}

# 1.0 USER INTERFACE ----
ui <- navbarPage(

    title = "Data Explorer",

    theme = bslib::bs_theme(version = 5, bootswatch = "darkly"),

    # REQUEST 1 TAB ----
    tabPanel(
        title = "Request 1",

        sidebarLayout(
          
          #Input description
            sidebarPanel(
                width = 3,
                h1("Select a Location and Metric"),
                
                
                shiny::selectizeInput(
                  inputId = "location_choice",
                  label = "Location",
                  choices = c("Portfolio", as.character(1:35)),
                  multiple = TRUE,
                  options = list(placeholder = "Select locations")
                ),
                
                
                shiny::selectInput(
                  inputId = "metric_choice",
                  label   = "Metric",
                  choices = c("Loss Ratio(per year)","Loss Ratio(cumulative)", 
                              "Total Insured Value", "Premium", 
                              "Premium per $100 Total Insured Value", "Loss Cost")
                )

            ),

            #Output description
            mainPanel(
                h1("Graph"),
                plotlyOutput("timeplot"),
                DT::dataTableOutput("exposuresTable")
            )
          
        )
        

    ),
    
    # # SECOND TAB ----
    # tabPanel(
    #   title = "Request 2",
    # 
    #   sidebarLayout(
    # 
    #     sidebarPanel(
    #       width = 3,
    #       h1("Your mom lol"),
    # 
    #       shiny::selectizeInput(
    #         inputId = "location_choice",
    #         label = "Location",
    #         choices = c("Portfolio", as.character(1:35)),
    #         multiple = TRUE,
    #         options = list(placeholder = "Select locations")
    #       )
    #     ),
    # 
    # 
    #     mainPanel(
    #       h1("title"),
    #       DT::dataTableOutput("exposuresTable")
    #     )
    #   )
    # )




)





# 2.0 SERVER ----
server <- function(input, output) {

    rv <- reactiveValues(location = NULL)

    observe({
      
      selected_locations <- input$location_choice
      #Check to see if any location is selected
      if (length(selected_locations) > 0) {
        rv$location <- data_list[selected_locations] %>% 
          bind_rows()
        
      }
      #Error handling for no location selected
      else {
        rv$location <- NULL
      }

    })
  
    

    output$timeplot <- renderPlotly({
    #Conditional to make sure there is a location selected  
      if (!is.null(rv$location)){
      
      #Total Insured Value Graph
      if (input$metric_choice == "Total Insured Value"){
        g <- ggplot(rv$location, aes(x = PolicyYear, y = `Total Insured Value`, color = Location)) +
          geom_line() + 
          labs(x = "Year", y = "Total Insured Value", title = "Value Over Time")
      }
      #Loss Ratio graph (per year)
      if (input$metric_choice == "Loss Ratio(per year)"){
        g <- ggplot(rv$location, aes(x = PolicyYear, y = (`Losses - Non Catastrophe` / Premium), color = Location)) +
          geom_line() + 
          labs(x = "Year", y = "Loss Ratio", title = "Loss Ratio Over Time")
      }
      #Loss Ratio graph (cumulative) - NEEDS TO BE FIXED - THIS IS BROKEN WHEN VIEWING MULTIPLE
      if (input$metric_choice == "Loss Ratio(cumulative)"){
        rv$location <- rv$location %>%
          group_by(Location) %>%
          mutate(
            cumulativeLoss = cumsum(`Losses - Non Catastrophe`),
            cumulativePremium = cumsum(Premium),
            cumulativeLossRatio = cumulativeLoss / cumulativePremium
          )
        
        g <- ggplot(rv$location, aes(x = PolicyYear, y = cumulativeLossRatio, color = Location)) +
          geom_line() + 
          labs(x = "Year", y = "Loss Ratio", title = "Loss Ratio Over Time")
      }
      #Premium graph
      if (input$metric_choice == "Premium"){
        g <- ggplot(rv$location, aes(x = PolicyYear, y = Premium, color = Location)) +
          geom_line() + 
          labs(x = "Year", y = "Premium", title = "Premium Over Time")
      }
      #Premium per $100 Total Insured Value graph
      if (input$metric_choice == "Premium per $100 Total Insured Value"){
        g <- ggplot(rv$location, aes(x = PolicyYear, y = (Premium / (`Total Insured Value` / 100)), 
                                     color = Location)) +
          geom_line() + 
          labs(x = "Year", y = "Premium per $100 Total Insured Value", title = "Premium Over Time per $100 Insured")
      }
      #Loss Cost graph CUMMEAN() FUNCTION IS KINDA SKETCHY BUT IT LOOKS SO SHIT OTHERWISE IDK
      if (input$metric_choice == "Loss Cost"){
        rv$location <- rv$location %>%
          group_by(Location) %>%
          mutate(
            cumulativeMeanLoss = cummean(`Losses - Non Catastrophe`),
            totalInsured = Premium,
            cumulativeLossCost = cumulativeMeanLoss / totalInsured
          )
        
        g <- ggplot(rv$location, aes(x = PolicyYear, y = cumulativeLossCost, color = Location)) +
          geom_line() + 
          labs(x = "Year", y = "Loss Cost", title = "Loss Cost Over Time")
      }
      
      ggplotly(g)
      }
    #Error handle if there is no location selected
      else
      {
        ggplot() + geom_blank() + labs(title = "Please select a location")
      }
    })
    
    output$exposuresTable <- DT::renderDataTable(rv$location)

}

# Run the application
shinyApp(ui = ui, server = server)
