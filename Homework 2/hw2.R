library(tidyverse)
library(shiny)
library(DT) 

spending = read.csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-02-13/historical_spending.csv') %>% 
  select(Year,Candy,Flowers,Jewelry,GreetingCards,EveningOut,Clothing,GiftCards) %>% 
  pivot_longer(c(Candy,Flowers,Jewelry,GreetingCards,EveningOut,Clothing,GiftCards),names_to = 'Category', values_to = 'Value')

col <- function(df) {
  df %>% 
    ggplot() +
    geom_col(aes(Year, Value, fill = Category)) +
    scale_color_manual(values = palette)+
    scale_x_continuous(breaks = seq(2010,2022,by=1))+
    ggtitle("Overall Valentines Day Spending by Category")+
    theme_bw()
}

line <- function(df) {
  df %>% 
    ggplot() +
    geom_line(aes(Year, Value, col = Category), linewidth = 2) +
    #scale_color_manual(values = palette)+
    scale_x_continuous(breaks = seq(2010,2022,by=1))+
    ggtitle("Valentines Day Spending by Category")+
    theme_bw()
}

ui <- fluidPage(
  titlePanel("Valentines Day per Person Spending Year Over Year"),
  sliderInput("year", "Year:", min = 2010, max = 2022,value = c(2010,2022),sep=""),
  checkboxGroupInput("category","Categories:",
                     c("Candy",
                       "Flowers",
                       "Jewelry",
                       "Greeting Cards" = "GreetingCards",
                       "Evening Out" = "EveningOut",
                       "Clothing",
                       "Gift Cards" = "GiftCards"),
                     selected = c("Candy","Flowers","Jewelry","GreetingCards","EveningOut","Clothing","GiftCards"),
                     inline = TRUE),
  plotOutput("col"),
  plotOutput("line")
)

server <- function(input, output) {
  current_data <- reactive({
    spending %>% 
      mutate(selected = (
        Year >= input$year[1] & 
          Year <= input$year[2]) &
          Category %in% input$category
      ) %>% 
      filter(selected == TRUE) %>% 
      select(Year,Category,Value)
  })
  output$col <- renderPlot(col(current_data()))
  output$line <- renderPlot(line(current_data()))
}

shinyApp(ui, server)
