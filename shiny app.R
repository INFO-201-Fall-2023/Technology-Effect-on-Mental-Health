library(dplyr)
library(stringr)
library(shiny)
library(ggplot2)
library(tidyr)

# Read the joint dataset created earlier
source("data wrangling.R") #loads in your analysis file

# Define UI
ui <- navbarPage(
  title = "Effects of Social Media on Mental Health",
  id = "navbar",
  tabPanel("Page 1",
           mainPanel(
             verbatimTextOutput("output_text1")
           )
  ),
  
  tabPanel("Analysis",
           sidebarLayout(
             sidebarPanel(
               selectInput("groupType", "Select Group:",
                           choices = c("National Estimate", "By Age", "By Sex", "By Race/Hispanic ethnicity", "By Education"),
                           selected = "National Estimate")
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Line Plot", plotOutput("linePlot")),
                 tabPanel("Correlation Chart", plotOutput("correlationChart")),
               )
             )
           )
  )
)

server <- function(input, output) {
  output$output_text1 <- renderText({
    "Since the start of the digital age that we currently live in, 
    our lives have revolved around technology. From TVs to laptops to 
    phones to even watchesCancel changes, it is hard to find yourself 
    not being connected to some sort of technology. While technology has 
    brought numerous benefits to society, including increased 
    connectivity and access to information, there is growing concern about its 
    impact on human well-being, especially with the drastic rise in social 
    media usage. From mental health to social relationships, technology use 
    can have both positive and negative effects on our lives.

    With the meteoric rise of apps such as Instagram, Snapchat, and TikTok, a lot 
    of scrutiny has been placed on what effects these apps are having on people, 
    especially children. This is leading to people wondering if these apps are having a negative effect 
    due to things such as comparisons to other children, cyberbullying, and addiction.

    Our central theme is that technology has both positive and negative effects 
    on human well-being, and we believe this is an interesting and compelling 
    story because it affects us all. With the rise of social media and the 
    internet, technology has become an integral part of our daily lives, and 
    its impact on our well-being is a topic of growing concern. By exploring 
    this topic through data visualization, we hope to provide a unique 
    perspective on this issue and offer practical insights into how we can use 
    technology in healthier and more productive ways.
    
    Here are some link to backround research we did:
    https://etactics.com/blog/negative-effects-of-technology-on-mental-health
    
    https://www.marriage.com/advice/relationship/technology-and-relationships/
    
    https://www.pewresearch.org/internet/2018/12/10/artificial-intelligence-
    and-the-future-of-humans/
    The data we used shows the number of people who are suffing from differnt
    mental illness. We are able to analyze the increase and decrease in negative
    health effects. We also have data on the amount of social media users. Our
    date shows the amount of users among youths and we can compare this number
    with dperession rates to maybe see a corelation."
  })
  
  output$linePlot <- renderPlot({
    # Prepare the data for the line plot based on the selected group
    plot_data <- df %>%
      filter(Group == input$groupType) %>%
      group_by(Year, Subgroup) %>%
      summarise(Value = mean(Value))
    
    # Create the stacked line plot
    ggplot(plot_data, aes(x = Year, y = Value, color = Subgroup)) +
      geom_line(linewidth = 1.5) +
      labs(x = "Year", y = "Mean Value", color = "Subgroup") +
      theme_minimal()
  })
  
  output$correlationChart <- renderPlot({
    # Prepare the data for the correlation chart based on the selected group
    plot_data <- df %>%
      filter(Group == input$groupType) %>%
      select(Subgroup, SnapCorr, InstaCorr, TikTokCorr, FbCorr, PinCorr, TwitCorr, RedditCorr)
    
    # Reshape the data into long format for plotting
    plot_data <- plot_data %>%
      pivot_longer(cols = -Subgroup, names_to = "Platform", values_to = "Correlation")
    
    # Create the stacked column chart
    ggplot(plot_data, aes(x = Subgroup, y = Correlation, fill = Platform)) +
      geom_col(position = "stack") +
      labs(x = "Subgroup", y = "Correlation", fill = "Platform") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

# Run the app
shinyApp(ui = ui, server = server)