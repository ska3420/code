library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(DT)
library(ggplot2)
library(collapsibleTree)
library(sf)
library(rgdal)
library(maptools)

# Set the file path to the shapefile (.shp)
shapefile_dir <- "F:/apurva documents/r prog/biodiversity/10m_cultural/10m_cultural"

# Read the shapefile

# Read shapefile
parks_shapefile <- st_read(dsn = shapefile_dir)


# Print information about the shapefile
print(shapefile)

# Read data
observations <- read.csv("F:/apurva documents/r prog/biodiversity/observations.csv")
species_info <- read.csv("F:/apurva documents/r prog/biodiversity/species_info.csv")

######
# Define UI
# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Biodiversity in National Parks"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Parks Map", tabName = "map"),
      menuItem("Species Data", tabName = "data"),
      menuItem("Species Tree", tabName = "tree"),
      menuItem("Species Chart", tabName = "chart"),
      menuItem("Choropleth Map", tabName = "choropleth")
    )
  ),
  dashboardBody(
    tabItems(
      # Parks Map
      tabItem(tabName = "map",
              fluidRow(
                leafletOutput("parksMap", width = "100%", height = "600px")
              )
      ),
      
      # Species Data
      tabItem(tabName = "data",
              fluidRow(
                dataTableOutput("speciesDataTable")
              )
      ),
      
      # Species Tree
      tabItem(tabName = "tree",
              fluidRow(
                box(
                  title = "Park and Category Selection",
                  width = 3,
                  selectInput("selectedParkTree", "Select a park:", choices = unique(species_info$park_name)),
                  selectInput("selectedCategoryTree", "Select a category:", choices = unique(species_info$category))
                ),
                box(
                  title = "Species Tree",
                  width = 9,
                  collapsibleTreeOutput("tree")
                )
              )
      ),
      
      # Species Chart
      tabItem(tabName = "chart",
              fluidRow(
                box(
                  title = "Category Selection",
                  width = 3,
                  selectInput("selectedCategoryChart", "Select a category:", choices = unique(species_info$category))
                ),
                box(
                  title = "Group 1",
                  width = 6,
                  plotOutput("ggplot2Group1")
                ),
                box(
                  title = "Group 2",
                  width = 6,
                  plotOutput("ggplot2Group2")
                )
              )
      ),
      
      # Choropleth Map
      tabItem(tabName = "choropleth",
              fluidRow(
                box(
                  title = "State and Category Selection",
                  width = 3,
                  selectInput("statesCombo", "Select a state:", choices = unique(parks_shapefile$state_name)),
                  selectInput("selectedCategoryChoro", "Select a category:", choices = unique(species_info$category))
                ),
                box(
                  title = "Species Count per State",
                  width = 9,
                  leafletOutput("choroplethMap", width = "100%", height = "600px")
                )
              )
      )
    )
  )
)

# Define server
server <- function(input, output) {
  # Parks Map
  output$parksMap <- renderLeaflet({
    leaflet(data = parks_shapefile) %>%
      addTiles() %>%
      addPolygons(
        fillColor = "#BDBDBD",
        fillOpacity = 0.7,
        color = "black",
        weight = 1,
        label = ~paste("<strong>Park:</strong> ", park_name)
      )
  })
  
  # Species Data
  output$speciesDataTable <- renderDataTable({
    species_info
  })
  
  # Species Tree
  output$tree <- renderCollapsibleTree({
    subset_species <- species_info[species_info$park_name == input$selectedParkTree &
                                     species_info$category == input$selectedCategoryTree, ]
    root_node <- list(name = "Species", children = list())
    
    for (i in 1:nrow(subset_species)) {
      category_node <- list(name = subset_species$common_names[i], size = subset_species$conservation_status[i])
      root_node$children[[i]] <- category_node
    }
    
    collapsibleTree(root_node)
  })
  
  # Species Chart
  output$ggplot2Group1 <- renderPlot({
    group1 <- species_info[species_info$category == input$selectedCategoryChart & species_info$group == "Group 1", ]
    ggplot(data = group1, aes(x = scientific_name, y = conservation_status)) +
      geom_bar(stat = "identity") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$ggplot2Group2 <- renderPlot({
    group2 <- species_info[species_info$category == input$selectedCategoryChart & species_info$group == "Group 2", ]
    ggplot(data = group2, aes(x = scientific_name, y = conservation_status)) +
      geom_bar(stat = "identity") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Choropleth Map
  output$choroplethMap <- renderLeaflet({
    state_species <- species_info[species_info$category == input$selectedCategoryChoro, ]
    state_counts <- state_species %>%
      group_by(state_name) %>%
      summarise(Count = sum(observations))
    
    leaflet(data = parks_shapefile) %>%
      addTiles() %>%
      addPolygons(
        data = state_counts,
        fillColor = ~colorQuantile("YlOrRd", Count)(Count),
        fillOpacity = 0.7,
        color = "#BDBDBD",
        weight = 1,
        label = ~paste("<strong>State:</strong> ", state_name, "<br>",
                       "<strong>Count:</strong> ", Count)
      ) %>%
      addLegend(
        position = "bottomright",
        pal = colorQuantile("YlOrRd", state_counts$Count),
        values = state_counts$Count,
        title = "Species Count"
      )
  })
}

# Run the application
shinyApp(ui = ui_1, server = server_1)