# Create a Shiny dashboard to explore the data interactively

create_shiny_dashboard <- function(data = NULL) {
  # Ensure required packages are loaded
  require(shiny)
  require(DT)
  require(wordcloud)
  require(dplyr)
  require(ggplot2)
  require(tidytext)
  require(mongolite)
  require(tidyr)
  require(igraph)
  require(ggraph)
  require(tidyverse)
  
  # UI definition
  ui <- fluidPage(
    titlePanel("Airbnb Data Analysis Dashboard"),
    
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "roomType",
          "Room Type:",
          choices = c("All")
        ),
        sliderInput(
          "priceRange",
          "Price Range:",
          min = 0,
          max = 1000,
          value = c(0, 200)
        ),
        checkboxInput(
          "superhostOnly",
          "Superhosts Only",
          value = FALSE
        ),
        hr(),
        helpText("Note: This dashboard provides interactive analysis of Airbnb listings.")
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Price Analysis", 
                   plotOutput("pricePlot"),
                   h4("Price Statistics"),
                   verbatimTextOutput("priceStats")),
          
          tabPanel("Reviews", 
                   plotOutput("reviewPlot"),
                   h4("Review Statistics"),
                   verbatimTextOutput("reviewStats")),
          
          tabPanel("Text Analysis", 
                   plotOutput("wordcloudPlot", height = "500px"),
                   h4("Top Words"),
                   tableOutput("topWords")),
          
          tabPanel("Data Table", 
                   h4("Filtered Listings"),
                   DT::dataTableOutput("listingTable"))
        )
      )
    )
  )
  
  # Server logic
  server <- function(input, output, session) {
    # Connect to MongoDB and Load Data
    connection_string <- 'mongodb+srv://mukherjeetirthankar:tirthA25@a1textmining.9gdz0.mongodb.net/?retryWrites=true&w=majority&appName=A1TextMining'
    airbnb_collection <- mongo(collection="listingsAndReviews", db="sample_airbnb", url=connection_string)
    
    airbnb_data <- airbnb_collection$find() %>% 
      mutate(
        price = as.numeric(gsub("[\\$,]", "", price)), # Clean price column
        last_review = as.Date(last_review),           # Convert last_review to Date
        first_review = as.Date(first_review),         # Convert first_review to Date
        review_gap = as.numeric(difftime(last_review, first_review, units = "days")), # Calculate review gap
        host_response_rate = as.numeric(gsub("%", "", host$host_response_rate)) / 100,     # Clean response rate
        accommodates = as.numeric(accommodates),
        bedrooms = as.numeric(bedrooms),
        bathrooms = as.numeric(bathrooms),
        beds = as.numeric(beds),
        host_is_superhost = host$host_is_superhost # Extract superhost status
      )
    
    # Update room type choices based on data
    updateSelectInput(session, "roomType", choices = c("All", unique(as.character(airbnb_data$room_type))))
    
    # Filter data based on user inputs
    filtered_data <- reactive({
      data <- airbnb_data
      if (input$roomType != "All") {
        data <- data %>% filter(room_type == input$roomType)
      }
      data <- data %>% filter(price >= input$priceRange[1] & price <= input$priceRange[2])
      if (input$superhostOnly) {
        data <- data %>% filter(host_is_superhost == TRUE)
      }
      data
    })
    
    # Price Analysis
    output$pricePlot <- renderPlot({
      price_analysis <- filtered_data() %>%
        group_by(room_type, property_type) %>%
        summarise(
          avg_price = mean(price, na.rm = TRUE),
          total_listings = n(),
          count_rating = mean(number_of_reviews, na.rm = TRUE)
        )
      
      ggplot(price_analysis, aes(x = room_type, y = avg_price, fill = property_type)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Average Price by Room Type and Property Type",
             x = "Room Type", y = "Average Price") +
        theme_minimal()
    })
    
    output$priceStats <- renderPrint({
      summary(filtered_data()$price)
    })
    
    # Reviews Analysis
    output$reviewPlot <- renderPlot({
      review_analysis <- filtered_data() %>%
        group_by(room_type) %>%
        summarise(
          avg_reviews = mean(number_of_reviews, na.rm = TRUE),
          total_reviews = sum(number_of_reviews, na.rm = TRUE)
        )
      
      ggplot(review_analysis, aes(x = room_type, y = avg_reviews, fill = room_type)) +
        geom_bar(stat = "identity") +
        labs(title = "Average Reviews by Room Type",
             x = "Room Type", y = "Average Reviews") +
        theme_minimal()
    })
    
    output$reviewStats <- renderPrint({
      summary(filtered_data()$number_of_reviews)
    })
    
    # Text Analysis
    output$wordcloudPlot <- renderPlot({
      description_tokens <- filtered_data() %>%
        select(name, description) %>%
        unnest_tokens(word, description) %>%
        anti_join(stop_words) %>% # Remove stop words
        count(word, sort = TRUE)
      
      wordcloud(words = description_tokens$word,
                freq = description_tokens$n,
                max.words = 200,
                colors = brewer.pal(8, "Dark2"))
    })
    
    output$topWords <- renderTable({
      description_tokens <- filtered_data() %>%
        select(name, description) %>%
        unnest_tokens(word, description) %>%
        anti_join(stop_words) %>% # Remove stop words
        count(word, sort = TRUE)
      
      head(description_tokens, 20)
    })
    
    # Data Table
    output$listingTable <- DT::renderDataTable({
      filtered_data() %>%
        select(name, room_type, price, number_of_reviews, host_is_superhost, accommodates, bedrooms, bathrooms, beds)
    })
  }
  
  # Run the application 
  shinyApp(ui = ui, server = server)
}

# Call the function to create and run the Shiny dashboard
create_shiny_dashboard()

