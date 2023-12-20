library(shiny)
library(plotly)

# Assuming your data is in a CSV file named 'PlayerAttributeData.csv'
data <- read.csv("data/PlayerAttributeData.csv")
data2 <- read.csv("data/PlayerPersonalData.csv")
data <- as.data.frame(lapply(data,as.numeric))
data <- na.omit(data)


ui <- fluidPage(
  
  titlePanel("Football Statistics Parallel Chart"),
  sidebarLayout(
    sidebarPanel(
      selectInput("player", "Choose a player:", choices = unique(data2$Club))
    ),
  mainPanel(
    plotlyOutput("ParallelChart")
  )
)
)

server <- function(input, output) {
  # Filter based on the selected club
  data_filtered <- reactive({data[data$ID == data2$ID[data2$Club == input$player],]})
  df <- reactive({data_filtered()})
  output$ParallelChart <- renderPlotly({
    df <- data_filtered()
    parallel_chart <- plot_ly(
      type = 'parcoords',
      line = list(color = 'blue'),
      dimensions = list(
        list(range= c(0,100),label = 'Acceleration', values = df$Acceleration),
        list(range= c(0,100),label = 'Aggression', values = df$Aggression),
        list(range = c(0,100),label = 'Agility', values = df$Agility),
        list(range = c(0,100),label = 'Balance', values = df$Balance),
        list(range = c(0,100),label = 'Ball control', values = df$Ball.control),
        list(range = c(0,100),label = 'Composure', values = df$Composure),
        list(range = c(0,100),label = 'Crossing', values = df$Crossing),
        list(range = c(0,100),label = 'Dribbling', values = df$Dribbling),
        list(range = c(0,100),label = 'Curve', values = df$Curve),
        list(range = c(0,100),label = 'Finishing', values = df$Finishing)
      )
    )
    parallel_chart
  })
}

shinyApp(ui, server)

