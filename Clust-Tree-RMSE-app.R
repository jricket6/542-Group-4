library(shiny)
library(dplyr)
library(ggplot2)
library(cluster)
library(forcats)
library(tidyverse)
library(caret)
library(rpart)
library(rpart.plot)


# sals <- read.csv("salaries.csv")
# 
# 
# sals <- sals %>%
#   filter(salary_currency=="USD" &
#            company_location=="US")
# 
# sals <- sals[,-c(6,7,8,10)]
# 
# sals <- sals %>%
#   mutate(job_title = fct_lump(job_title, prop = 0.01))
# 
# sals$job_title<- as.character(sals$job_title)



sals <- read.csv("salaries.csv")

sals <- sals %>%
  filter(work_year %in% c(2022, 2023), company_location == "US")

 for (i in 1:nrow(sals)) {
   if (sals$work_year[i] == 2022) {
     sals$salary_in_usd[i] <- sals$salary_in_usd[i] * 1.08
   }
 }

 Q1 <- quantile(sals$salary_in_usd, 0.25)
 Q3 <- quantile(sals$salary_in_usd, 0.75)
 IQR <- Q3 - Q1

 lower_bound <- Q1 - 1.5 * IQR
 upper_bound <- Q3 + 1.5 * IQR

sals <- sals[sals$salary_in_usd >= lower_bound & sals$salary_in_usd <= upper_bound, ]




train_indices <- sample(1:nrow(sals), 0.8 * nrow(sals))


train_data <- sals[train_indices, ]
test_data <- sals[-train_indices, ]


factor_vars <- c("experience_level", "job_title", "salary_currency", "employee_residence", "company_size", "company_location", "employment_type")
for (var in factor_vars) {
  train_data[[var]] <- as.factor(train_data[[var]])
  test_data[[var]] <- as.factor(test_data[[var]])
}


for (var in factor_vars) {
  test_data[[var]] <- factor(test_data[[var]], levels = levels(train_data[[var]]))
}


tree_model <- rpart(salary_in_usd ~ work_year + experience_level + employment_type + job_title + employee_residence + remote_ratio + company_location + company_size, data = train_data)



ui <- fluidPage(
  titlePanel("Clustered Salary Levels"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("company_size", "Select Company Size:",
                  choices = c("All", unique(sals$company_size)),
                  selected = "All"),
      selectInput("experience_level", "Select Experience Level:",
                  choices = c("All", unique(sals$experience_level)),
                  selected = "All"),
      selectInput("employment_type", "Select Employment Type:",
                  choices = c("All", unique(sals$employment_type)),
                  selected = "All"),
      selectInput("job_title", "Select Job Title:",
                  choices = c("All", unique(sals$job_title)),
                  selected = "All")
    ),
    
    mainPanel(
      plotOutput("cluster_plot"),
      textOutput("rmse_output"),
      plotOutput("Dec_Tree")
    )
  )
)


server <- function(input, output) {
  output$cluster_plot <- renderPlot({
    
    filtered_data <- sals %>%
      filter(
        if (input$company_size != "All") company_size == input$company_size else TRUE,
        if (input$experience_level != "All") experience_level == input$experience_level else TRUE,
        if (input$employment_type != "All") employment_type == input$employment_type else TRUE,
        if (input$job_title != "All") job_title == input$job_title else TRUE
      )
    
    columns <- filtered_data[, c("work_year", "salary_in_usd")]
    
    
    num_unique_data_points <- nrow(unique(columns))
    num_cluster_centers <- 3
    
    
    if (num_unique_data_points < num_cluster_centers) {
      num_cluster_centers <- num_unique_data_points
    }
    
    kmeans_model <- kmeans(columns, centers = num_cluster_centers, nstart = 20)
    
    filtered_data$cluster <- as.factor(kmeans_model$cluster)
    
    ggplot(filtered_data, aes(work_year, salary_in_usd, color = cluster)) + 
      geom_point() + 
      labs(y="Salary in USD", x="Work Year", title = "Clustered Salary Level") +
      scale_y_continuous(labels = scales::number_format())+
      scale_x_continuous(breaks = seq(min(filtered_data$work_year), max(filtered_data$work_year), by = 1))
  })
  
  output$rmse_output <- renderText({
    filtered_data <- sals %>%
      filter(
        if (input$company_size != "All") company_size == input$company_size else TRUE,
        if (input$experience_level != "All") experience_level == input$experience_level else TRUE,
        if (input$employment_type != "All") employment_type == input$employment_type else TRUE,
        if (input$job_title != "All") job_title == input$job_title else TRUE
      )
    
    filtered_data$job_title <- factor(filtered_data$job_title, levels = levels(train_data$job_title))
    filtered_data$employee_residence <- factor(filtered_data$employee_residence, levels = levels(train_data$employee_residence))
    
    # Make sure the variables used in the decision tree model are factors!!!!!!!!!
    factor_vars <- c("experience_level", "job_title", "salary_currency", "employee_residence", "company_size", "company_location", "employment_type")
    for (var in factor_vars) {
      filtered_data[[var]] <- as.factor(filtered_data[[var]])
    }
    
    
    tree_predictions <- predict(tree_model, newdata = filtered_data)
    
    
    actual_salary_tree <- filtered_data$salary_in_usd
    
    
    complete_cases_tree <- complete.cases(actual_salary_tree, tree_predictions)
    actual_salary_tree <- actual_salary_tree[complete_cases_tree]
    tree_predictions <- tree_predictions[complete_cases_tree]
    
    
    rmse_tree <- sqrt(mean((tree_predictions - actual_salary_tree)^2))
    
    
    paste("Root Mean Squared Error (Decision Tree):", round(rmse_tree,2), "\n")
  })
  
  
  output$Dec_Tree <- renderPlot({
    rpart.plot(tree_model, extra = 1, type = 3)
  })
  
}




shinyApp(ui, server)
