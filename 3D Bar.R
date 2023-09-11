df <- read.csv("Tech.csv", check.names = FALSE)


# Remove rows where "Year" is NA
df <- df[complete.cases(df$Year), ]

df[is.na(df)] <- FALSE


# Sort the data frame based on the 'Year' variable in ascending order
df <- df[order(df$Year), ]



library(networkD3)
library(dplyr)
library(plotly)
library(shiny)
library(ggplot2)
library(tidyr)
library(reshape2)
library(RColorBrewer)


# Define the categories

colnames(df) <- gsub("Data science", "Data Science", colnames(df))
colnames(df) <- gsub("Data mining", "Data Mining", colnames(df))
colnames(df) <- gsub("Big data", "Big Data", colnames(df))
colnames(df) <- gsub("Open data", "Open Data", colnames(df))
colnames(df) <- gsub("Remote sensing", "Remote Sensing", colnames(df))
colnames(df) <- gsub("3dScanning", "3D Scanning", colnames(df))
colnames(df) <- gsub("3dModelling", "3D Modelling", colnames(df))
colnames(df) <- gsub("AI&related", "AI Related", colnames(df))
colnames(df) <- gsub("Mat_Stat_not_ai_or_dss", "Other Digital Modelling", colnames(df))
colnames(df) <- gsub("SocialNetworks", "Social Networks", colnames(df))
colnames(df) <- gsub("SocialNews", "Social News", colnames(df))
colnames(df) <- gsub("MediaSharing", "Media Sharing", colnames(df))
colnames(df) <- gsub("PointCloud", "Point Cloud", colnames(df))
colnames(df) <- gsub("Machine learning", "Machine Learning", colnames(df))


category_columns <- list(
  Data = c("Data", "Data Science", "Data Mining", "Big Data", "Crowdsourcing", "Open Data"),
  `VR/AR/MR` = c("VR", "AR", "MR"),
  Robotic = c("Robotic", "DronUAV/UAS"),
  GIS = c("GIS"),
  `Remote Sensing` = c("Remote Sensing"),
  `3D Modelling` = c("BIM", "LIM", "Point Cloud", "Rendering", "3D Scanning", "3D Modelling"),
  `AI Related` = c("AI Related", "Deep Learning", "Artificial Intelligence", "Machine Learning", "MAS"),
  `Decision Support` = c("MCDA/AHP", "PSS/DSS"),
  `Other Digital Modelling` = c("SIM", "Other Digital Modelling"),
  `Social Media` = c("Social Media", "Social Networks", "Social News", "Blogging", "Bookmarking", "Media Sharing"),
  `Other ICT` = c("GPS", "GNSS", "ICT", "5G/Beyond", "Blockchain", "Cloud", "IoT", "Devices")
)



filter_category_columns <- function(df, category) {
  category_columns_selected <- category_columns[[category]]
  df_filtered <- df[, c("Year", category_columns_selected)]
  return(df_filtered)
}

# Define the UI for the Shiny app
ui <- fluidPage(
  titlePanel("Stacked Bar Plot"),
  sidebarLayout(
    sidebarPanel(
      selectInput("category", "Select a category", choices = names(category_columns)),
      br(),
      actionButton("plotButton", "Plot")
    ),
    mainPanel(
      plotlyOutput("plot")
    )
  )
)

# Define a color palette
color_palette <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b")

# Define the server for the Shiny app
server <- function(input, output) {
  filtered_data <- eventReactive(input$plotButton, {
    filter_category_columns(df, input$category)
  })
  
  output$plot <- renderPlotly({
    data <- filtered_data()
    
    plot_df <- data %>%
      group_by(Year) %>%
      summarise(across(everything(), sum)) %>%
      pivot_longer(-Year, names_to = "Technology", values_to = "Count")
    
    
    # Assign colors to each unique technology
    unique_technologies <- unique(plot_df$Technology)
    technology_colors <- setNames(color_palette[1:length(unique_technologies)], unique_technologies)
    plot_df$Color <- technology_colors[plot_df$Technology]
    
    plot <- plot_ly(
      plot_df,
      x = ~Year,
      y = ~Count,
      type = "bar",
      opacity = 0.8,
      marker = list(line = list(width = 1, color = "white"),
                    color = ~Color),
      text = ~paste(Technology)
    ) %>%
      layout(
        xaxis = list(title = "Year"),
        yaxis = list(title = "Count")
      )
    plot
  })
}


# Run the Shiny app
shinyApp(ui, server)

