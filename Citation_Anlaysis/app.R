library(plyr)
library(lubridate)
library(scales)
library(tidyverse)
library(shiny)
library(shinythemes)
library(DT)

load("Processed Data/citation_data_tidy.RData")

ui <- fluidPage(
   
   # Application title
   #titlePanel("Citation Analysis"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput("citation_type",
                     "Citation Type:",
                     choices = citation_data %>% pull(ticketchargedescription) %>% unique() %>% sort(),
                     multiple = TRUE,
                     selected = "")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         tabsetPanel(type = "tabs",
                     tabPanel("Citations by Hour",
                              plotOutput(outputId = "citation_hour", height = 700),
                              dataTableOutput(outputId = "table_hour")),
                     tabPanel("Citations by Day of the Week",
                              plotOutput(outputId = "citation_weekday", height = 700),
                              dataTableOutput(outputId = "table_weekday")),
                     tabPanel("Citations by Day of the Month",
                              plotOutput(outputId = "citation_day", height = 700),
                              dataTableOutput(outputId = "table_day")),
                     tabPanel("Citations by Month",
                              plotOutput(outputId = "citation_month", height = 700),
                              dataTableOutput(outputId = "table_month"))
          )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  citation_reactive <- reactive({ citation_data %>%
                                    filter(ticketchargedescription %in% input$citation_type) })

  output$citation_hour <- renderPlot({
    
    citation_reactive() %>%
      group_by(ticket_hour, ticketchargedescription) %>%
      summarize(ticket_count = n()) %>%
      ungroup() %>%
      ggplot(aes(ticket_hour, ticket_count, col = factor(ticketchargedescription))) + geom_line() + 
      geom_point() + scale_x_continuous(breaks = seq.int(0, 24, 2)) + labs(x = "Hour", y = "Citation Count") +
      scale_color_discrete(name = "Citation Description")
    
  })
  
  output$table_hour <- DT::renderDataTable({
    datatable(citation_reactive() %>%
                select(ticketdatetime, ticketchargedescription, address, source))
  })
  
  output$citation_weekday <- renderPlot({
    
    citation_reactive() %>%
      group_by(ticket_weekday, ticketchargedescription) %>%
      summarize(ticket_count = n()) %>%
      ungroup() %>%
      ggplot(aes(ticket_weekday, ticket_count, col = factor(ticketchargedescription), group = factor(ticketchargedescription))) + 
      geom_line() + 
      geom_point() + scale_x_discrete(limits = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")) + 
      labs(x = "Weekday", y = "Citation Count") + scale_color_discrete(name = "Citation Description") + 
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
    
  })
  
  output$table_weekday <- DT::renderDataTable({
    datatable(citation_reactive() %>%
                select(ticketdatetime, ticketchargedescription, address, source))
  })
  
  output$citation_day <- renderPlot({
    
    citation_reactive() %>%
      group_by(ticket_day, ticketchargedescription) %>%
      summarize(ticket_count = n()) %>%
      ungroup() %>%
      ggplot(aes(ticket_day, ticket_count, col = factor(ticketchargedescription))) + geom_line() + 
      geom_point() + scale_x_continuous(breaks = seq.int(0, 31, 2)) + labs(x = "Day of Month", y = "Citation Count") +
      scale_color_discrete(name = "Citation Description")
    
  })
  
  output$table_day <- DT::renderDataTable({
    datatable(citation_reactive() %>%
                select(ticketdatetime, ticketchargedescription, address, source))
  })
  
  output$citation_month <- renderPlot({
    
    citation_reactive() %>%
      group_by(ticket_month, ticketchargedescription) %>%
      summarize(ticket_count = n()) %>%
      ungroup() %>%
      ggplot(aes(ticket_month, ticket_count, col = factor(ticketchargedescription))) + geom_line() + 
      geom_point() + scale_x_continuous(breaks = seq.int(0, 12, 1)) + labs(x = "Month", y = "Citation Count") +
      scale_color_discrete(name = "Citation Description")
    
  })
  
  output$table_month <- DT::renderDataTable({
    datatable(citation_reactive() %>%
                select(ticketdatetime, ticketchargedescription, address, source))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

