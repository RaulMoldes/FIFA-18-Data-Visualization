library(shiny)
library(plotly)

# Assuming your data is in a CSV file named 'PlayerAttributeData.csv'
data <-read.csv('data/CompleteDatasetFilteredFinal.csv')
data <- as.data.frame(data)


ui <- fluidPage(
  
  titlePanel("Football Statistics Parallel Chart"),
  sidebarLayout(
    sidebarPanel(
      selectInput("position", "Choose a position:", choices = unique(data$PreferredPositions))
    ),
    mainPanel(
      plotlyOutput("StatsPlot") 
    )
  )
)

server <- function(input, output) {
  data_filtered <- reactive({
    df <- data[data$PreferredPositions == input$position,]
    print(df$Name)
    df
  })
  
  output$StatsPlot <- renderPlotly({
    df <- data_filtered()
    stats_plot <- plot_ly(
      type = 'parcoords',
      line = list(color = 'purple'),
      dimensions = list(
        list(range = c(0,100), label = 'Acceleration', values = df$Acceleration),
        list(range = c(0,100), label = 'Aggression', values = df$Aggression),
        list(range = c(0,100), label = 'GK Reflexes',values = df$GK.reflexes),
        list(range = c(0,100), label = 'Agility', values = df$Agility),
        list(range = c(0,100), label = 'Crossing', values = df$Crossing),
        list(range = c(0,100), label = 'GK Positioning', values = df$GK.positioning),
        list(range = c(0,100), label = 'Ball control', values = df$Ball.control),
        list(range = c(0,100), label = 'Acceleration', values = df$Acceleration),
        list(range = c(0,100), label = 'Speed', values = df$Sprint.speed),
        list(range = c(0,100), label = 'Heading', values = df$Heading.accuracy),
        list(range = c(0,100), label = 'Finishing', values = df$Finishing),
        list(range = c(0,100), label = 'Interceptions', values = df$Interceptions)
        
      )
    )
    stats_plot
  })

}

shinyApp(ui, server)

