install.packages('rsconnect')
library(shiny)
library(ggplot2)
library(dplyr)
library(randomForest)
library(rsconnect)

#data <- read.csv("heart_data.csv")
file_path <- file.choose()

#data <- read.csv(file_path)

ui <- fluidPage(
  titlePanel("Exploring Heart Health Data"),
  sidebarLayout(
    sidebarPanel(
      selectInput("x_var", "Select X Variable:",
                  choices = c("age", "gender", "height", "weight", "ap_hi", "ap_lo", "cholesterol", "gluc", "smoke", "alco", "active")),
      checkboxInput("filter_cardio", "Filter Cardiovascular Disease", value = FALSE),
      actionButton("update", "Update Plot")
    ),
    mainPanel(
      plotOutput("plot"),
      textOutput("algorithm_info"),
      tableOutput("rf_results")
    )
  )
)

server <- function(input, output) {
  
  filter_data <- function(data, filter_cardio) {
    if (filter_cardio) {
      data <- data %>% filter(cardio != 0)
    }
    return(data)
  }
  
  output$plot <- renderPlot({
    data_to_plot <- filter_data(data, input$filter_cardio)
    ggplot(data_to_plot, aes_string(x = input$x_var, y = "cardio")) +
      geom_point(alpha = 0.5, color = "red") +  
      labs(x = input$x_var, y = "Cardiovascular Disease",
           title = paste("Relationship between", input$x_var, "and Cardiovascular Disease")) +
      theme_minimal()
  })
  
  output$algorithm_info <- renderText({
    "Random Forest Classification Algorithm:\n\nRandom Forest is an ensemble learning method that operates by constructing a multitude of decision trees at training time and outputting the class that is the mode of the classes (classification) of the individual trees. It is one of the most popular and effective algorithms for classification tasks."
  })
  
  rf_model <- eventReactive(input$update, {
    filtered_data <- filter_data(data, input$filter_cardio)
    formula <- as.formula(paste("factor(cardio) ~", input$x_var))
    rf <- randomForest(formula, data = filtered_data, importance = TRUE, ntree = 100)
    importance <- importance(rf)
    var_importance <- data.frame(Variable = rownames(importance), Importance = importance[, 1])
    var_importance <- var_importance[order(-var_importance$Importance), ]  # Sort by importance
    var_importance
  })
  
  output$rf_results <- renderTable({
    rf_model()
  })
}

shinyApp(ui = ui, server = server)
