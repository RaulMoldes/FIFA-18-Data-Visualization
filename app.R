library(shiny)
library(plotly)

# Assuming your data is in a CSV file named 'PlayerAttributeData.csv'
data <-read.csv('data/CompleteDatasetFilteredFinal.csv')
data <- as.data.frame(data)

stats <- c('Acceleration','Aggression','Agility','Balance','Ball control','Composure','Crossing','Curve','Dribbling','Finishing','Free kick accuracy','GK diving','GK handling','GK kicking','GK positioning','GK reflexes','Heading accuracy','Interceptions','Jumping','Long passing','Long shots','Marking','Penalties','Positioning','Reactions','Short passing','Shot power','Sliding tackle','Sprint speed','Stamina','Standing tackle','Strength','Vision','Volleys')



ui <- fluidPage(
  
  titlePanel("Football Statistics Parallel Chart"),
  sidebarLayout(
    sidebarPanel(
      selectInput("position", "Choose a position:", choices = unique(data$PreferredPositions))
    ),
    mainPanel(
      plotlyOutput("StatsPlot") 
    )
  ), titlePanel("Cluster Analysis Wage - Stats"),
  sidebarLayout(
    sidebarPanel(
      selectInput("x_stat", "Choose a stat for X axis:", choices = stats),
      selectInput("y_stat", "Choose a stat for Y axis:", choices = stats),
      selectInput("z_stat", "Choose a stat for Z axis:", choices = stats)
    ),
    mainPanel(
      plotlyOutput("ClustersPlot") 
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
  
  output$ClustersPlot <- renderPlotly({
    df <- data
  
    
    # Crear rangos para la variable wage
    df$wage_range <- df$Wage
    
    
    clusters_plot <- plot_ly(df, x = ~df[[input$x_stat]], y = ~df[[input$y_stat]], z = ~df[[input$z_stat]], color = ~wage_range, colors = 'Blues') %>%
      add_markers() %>%
      layout(scene = list(xaxis = list(title = input$x_stat),
                          yaxis = list(title = input$y_stat),
                          zaxis = list(title = input$z_stat)))
    clusters_plot
  })
}




shinyApp(ui, server)

