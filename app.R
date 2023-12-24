library(shiny)
library(plotly)

library(leaflet)
library(RColorBrewer)
library(colorBlindness)

# Assuming your data is in a CSV file named 'PlayerAttributeData.csv'
data <-read.csv('data/CompleteDatasetFilteredFinal.csv')
data <- as.data.frame(data)
stats <- c("Acceleration","Aggression","Agility","Balance","Ball.control","Composure","Crossing","Curve","Dribbling","Finishing","Free.kick.accuracy","GK.diving","GK.handling","GK.kicking","GK.positioning","GK.reflexes","Heading.accuracy","Interceptions","Jumping","Long.passing","Long.shots","Marking","Penalties","Positioning","Reactions","Short.passing","Shot.power","Sliding.tackle","Sprint.speed","Stamina","Standing.tackle","Strength","Vision","Volleys")

spanish_teams_A = c('FC Barcelona', 'Atlético Madrid', 'Real Madrid', 'Valencia', 'Getafe', 'Sevilla', 'RCD Espanyol',
                   'Athletic Club de Bilbao', 'Real Sociedad', 'Real Betis', 'Deportivo Alavés', 'Eibar', 'CD Leganés',
                   'Villarreal', 'Levante', 'Real Valladolid', 'Celta de Vigo', 'Girona', 'Huesca', 'Rayo Vallecano')

spanish_teams_B = c('Osasuna', 'Granada', 'Málaga', 'Albacete', 'Deportivo de la Coruña', 'Cádiz',
                   'Real Oviedo', 'Sporting de Gijón', 'Almería', 'UD Las Palmas',
                   'Alcorcón', 'Real Zaragoza', 'Tenerife', 'Numancia', 'Lugo',
                   'Gimnàstic de Tarragona', 'Córdoba', 'Reus Deportiu')

english_teams_A = c('Manchester City', 'Liverpool', 'Chelsea', 'Tottenham Hotspur', 'Arsenal', 'Manchester United',
                   'Wolverhampton Wanderers', 'Everton', 'Leicester City', 'West Ham United', 'Watford',
                   'Crystal Palace', 'Newcastle United', 'Bournemouth', 'Burnley', 'Southampton',
                   'Brighton & Hove Albion', 'Cardiff City', 'Fulham', 'Huddersfield Town')

english_teams_B = c('Leicester City', 'Ipswich Town', 'Leeds United', 'Southampton', 'West Bromwich Albion', 'Hull City',
                   'Sunderland', 'Preston North End', 'Watford', 'Norwich City', 'Blackburn Rovers', 'Cardiff City',
                   'Middlesbrough', 'Bristol City', 'Coventry City', 'Plymouth Argyle', 'Birmingham City',
                   'Swansea City', 'Stoke City', 'Millwall', 'Huddersfield Town', 'Queens Park Rangers',
                   'Sheffield Wednesday', 'Rotherham United')

italian_teams_A = c('Juventus', 'Napoli', 'Atalanta', 'Inter', 'Milan', 'Roma', 'Torino', 'Lazio',
                   'Sampdoria', 'Bologna', 'Sassuolo', 'Udinese', 'SPAL', 'Parma', 'Cagliari', 'Fiorentina', 'Genoa',
                   'Empoli', 'Frosinone', 'Chievo Verona')

italian_teams_B = c('Ascoli', 'Benevento', 'Brescia', 'Carpi', 'Cittadella', 'Cremonese', 'Crotone', 'Foggia',
                   'Hellas Verona', 'Lecce', 'Palermo', 'Perugia', 'Pescara', 'Salernitana',
                   'Spezia', 'F.B.C. Unione Venezia')

# Define a mapping from detailed positions to broader categories
position_mapping <- list(
 "Forward" = c("LW", "RW", "ST"),
  "Midfielder" = c("LM", "RM", "CM","CAM","CDM"),
  "Defender" = c("LB", "RB", "CB","LWB","RWB"),
  "Goalkeeper" = c("GK")
)

club_mapping <- list(
  "Spain" = c(spanish_teams_A,spanish_teams_B),
  "England" = c(english_teams_B,english_teams_A),
  "Italy" = c(italian_teams_B,italian_teams_A)
)

division_mapping <-list(
  "First"= c(spanish_teams_A,english_teams_A,italian_teams_A),
  "Second" = c(italian_teams_B,spanish_teams_B,english_teams_B)
)
data$PreferredPositions <- lapply(data$PreferredPositions, function(x) strsplit(x, " ")[[1]])
data$PreferredPositions <- sapply(data$PreferredPositions, function(x) x[[1]])

#Function to find the country where the player does play
find_country <- function(club){
  for (country in names(club_mapping)){
    if(club %in% club_mapping[[country]]){
      return (country)
    }
  }
  return(NA)
}

# Function to find the category for a given position
find_category <- function(position) {
  for (category in names(position_mapping)) {
    if (position %in% position_mapping[[category]]) {
      return(category)
    }
  }
  return("Midfielder")  # Return NA if no match found
}

# Function to find the category for a given position
find_division <- function(club) {
  for (div in names(division_mapping)) {
    if (club %in% division_mapping[[div]]) {
      return(div)
    }
  }
  return(NA)  # Return NA if no match found
}


data$Position <- sapply(data$PreferredPositions, find_category)

data$Country <- sapply(data$Club, find_country)
data$Division <- sapply(data$Club, find_division)

ui <- fluidPage(
  titlePanel("Football Statistics Parallel Chart"),
  sidebarLayout(
    sidebarPanel(
      selectInput("attributes", "Select attributes to be displayed on the y axis", choices = stats, multiple = TRUE),
      selectInput("positions", "Select player positions to be displayed", choices = unique(data$PreferredPositions), multiple =TRUE)
    ),
    mainPanel(
      plotlyOutput("StatsPlot") 
    )
  ), titlePanel("Cluster Analysis"),
  sidebarLayout(
  
    sidebarPanel(
      selectInput("variable", "Select a variable to analyze clusters", choices = c("Player value", "Player wage", "Club country", "Player category", "Player position"))
    ),
  mainPanel(
    plotlyOutput("ClustersPlot")
  
)

))

server <- function(input, output) {
  filtered<-reactive({
    data[data$PreferredPositions == input$positions,]
  })
  
  output$StatsPlot <- renderPlotly({
    req(input$positions)
    req(input$attributes)  
    data_filt <- filtered()
    
    attributes <- input$attributes
   
     # Subset dimensions based on user's selection
   dimensions <- lapply(attributes, function(attr) {
      list(range = c(0,100), label = attr, values = data_filt[[attr]])
   })
    
    stats_plot <- plot_ly(
      
      type = 'parcoords',
      line = list(color = 'purple'),
      dimensions = dimensions,
    )
    stats_plot
  })
  
  output$ClustersPlot <- renderPlotly({
    # Ensure PCA result is available
    df <- data
  
    # Subset the data based on user's selection
    selected_data <- data[, stats]
    print(selected_data)
    # Remove rows with missing values
    selected_data <- selected_data[complete.cases(selected_data), ]
    selected_data[] <- lapply(selected_data, as.numeric)
    df <- df[complete.cases(selected_data),]
    selected_data <- selected_data[complete.cases(selected_data), ]
 
    pca <-prcomp(selected_data, scale. = TRUE)
    
  
    selected_variable <- input$variable
   
    df$PC1 <- pca$x[,1]
    df$PC2 <- pca$x[,2]
    df$PC3 <- pca$x[,3]
    
    if(input$variable == "Player wage"){

    clusters_plot <- plot_ly(df, x = df$PC1, y = df$PC2, z = df$PC3, color = df$Wage, colors= "Blues") %>%
      add_markers() %>%
      layout(scene = list(xaxis = list(title = "PC1"),
                          yaxis = list(title = "PC2"),
                          zaxis = list(title = "PC3")))
    
    }else if(input$variable == "Player value"){

    clusters_plot <- plot_ly(df, x = df$PC1, y = df$PC2, z = df$PC3, color = df$Value, colors = "Reds") %>%
      add_markers() %>%
      layout(scene = list(xaxis = list(title = "PC1"),
                          yaxis = list(title = "PC2"),
                          zaxis = list(title = "PC3")))
    
    }else if(input$variable == "Club country"){
      
  
    
    clusters_plot <- plot_ly(df, x = df$PC1, y = df$PC2, z = df$PC3, color = df$Country) %>%
      add_markers() %>%
      layout(scene = list(xaxis = list(title = "PC1"),
                          yaxis = list(title = "PC2"),
                          zaxis = list(title = "PC3")))
    
    }else if(input$variable == "Player category"){
     
      clusters_plot <- plot_ly(df, x = df$PC1, y = df$PC2, z = df$PC3, color = df$Division) %>%
        add_markers() %>%
        layout(scene = list(xaxis = list(title = "PC1"),
                            yaxis = list(title = "PC2"),
                            zaxis = list(title = "PC3")))
    }else if(input$variable == "Player position"){
      clusters_plot <- plot_ly(df, x = df$PC1, y = df$PC2, z = df$PC3, color = df$Position) %>%
        add_markers() %>%
        layout(scene = list(xaxis = list(title = "PC1"),
                            yaxis = list(title = "PC2"),
                            zaxis = list(title = "PC3")))
    }
    
    
    clusters_plot
  })
}



shinyApp(ui, server)

