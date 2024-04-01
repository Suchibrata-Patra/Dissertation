# Load required packages
library(shiny)
library(ggplot2)
library(caTools)
library(pROC)
library(reshape2)
library(car)

# Define UI
ui = fluidPage(
  
  # App title
  titlePanel("Statistical Analysis and Visualization App"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      
      # File upload feature
      fileInput("file", "Upload dataset"),
      
      # Variable selection
      checkboxGroupInput("variables", "Select predictor variables", choices = NULL),
      
      # Response variable selection
      selectInput("response", "Select response variable", choices = NULL),
      
      # Select all predictor variables
      actionButton("selectAllPredictorsBtn", "Select All"),
      
      # Analyze button
      actionButton("analyzeBtn", "Analyze")
      
    ),
    
    # Main panel with tabs for displaying outputs
    mainPanel(
      tabsetPanel(
        # Tab for summary
        tabPanel("Summary",
                 # Correlation heatmap
                 plotOutput("correlationPlot"),
                 # VIF calculation
                 verbatimTextOutput("vifOutput"),
                 # Significance testing
                 verbatimTextOutput("significanceOutput"),
                 # Model selection
                 verbatimTextOutput("modelOutput")
        ),
        # Tab for ROC curve
        tabPanel("ROC Curve",
                 plotOutput("rocPlot")
        ),
        # Tab for model accuracy
        tabPanel("Model Accuracy",
                 verbatimTextOutput("accuracyOutput")
        )
      )
    )
  )
)

# Define server logic
server = function(input, output, session) {
  
  # Read uploaded dataset
  data = reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  # Update variable selection options based on uploaded dataset
  observe({
    updateCheckboxGroupInput(session, "variables", "Select predictor variables", choices = colnames(data()))
    updateSelectInput(session, "response", "Select response variable", choices = colnames(data()))
  })
  
  # Select all predictor variables
  observeEvent(input$selectAllPredictorsBtn, {
    updateCheckboxGroupInput(session, "variables", "Select predictor variables", choices = colnames(data()), selected = colnames(data()))
  })
  
  # Perform analysis on button click
  observeEvent(input$analyzeBtn, {
    
    # Error handling
    tryCatch({
      
      # Correlation heatmap
      output$correlationPlot = renderPlot({
        correlation_matrix = cor(data()[, input$variables])
        heatmap(correlation_matrix, symm = TRUE)
      })
      
      # VIF calculation
      output$vifOutput = renderPrint({
        vif_results = car::vif(lm(as.formula(paste(input$response, "~", paste(input$variables, collapse = "+"))), data()))
        vif_results
      })
      
      # Significance testing
      output$significanceOutput = renderPrint({
        model = glm(as.formula(paste(input$response, "~", paste(input$variables, collapse = "+"))), data(), family = binomial)
        summary(model)
      })
      
      # Model selection
      output$modelOutput = renderPrint({
        model = glm(as.formula(paste(input$response, "~", paste(input$variables, collapse = "+"))), data(), family = binomial)
        step_model = step(model)
        step_model
      })
      
      # ROC curve
      output$rocPlot = renderPlot({
        model = glm(as.formula(paste(input$response, "~", paste(input$variables, collapse = "+"))), data(), family = binomial)
        roc_curve = roc(data()[[input$response]], predict(model, type = "response"))
        plot(roc_curve)
        auc = auc(roc_curve)
        legend("bottomright", legend = paste("AUC:", round(auc, 2)))
      })
      
      # Model accuracy
      output$accuracyOutput = renderPrint({
        model = glm(as.formula(paste(input$response, "~", paste(input$variables, collapse = "+"))), data(), family = binomial)
        predictions = ifelse(predict(model, type = "response") > 0.5, 1, 0)
        confusion_matrix = table(data()[[input$response]], predictions)
        sensitivity = confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
        specificity = confusion_matrix[1, 1] / sum(confusion_matrix[1, ])
        accuracy = mean(predictions == data()[[input$response]])
        list(Sensitivity = sensitivity, Specificity = specificity, Accuracy = accuracy)
      })
      
    }, error = function(e) {
      cat("An error occurred:", conditionMessage(e), "\n")
      cat(traceback())
    })
    
  })
  
  # Update analysis when predictor variables change
  observeEvent(input$variables, {
    output$rocPlot = renderPlot({
      model = glm(as.formula(paste(input$response, "~", paste(input$variables, collapse = "+"))), data(), family = binomial)
      roc_curve = roc(data()[[input$response]], predict(model, type = "response"))
      plot(roc_curve)
      auc = auc(roc_curve)
      legend("bottomright", legend = paste("AUC:", round(auc, 2)))
    })
    
    output$accuracyOutput = renderPrint({
      model = glm(as.formula(paste(input$response, "~", paste(input$variables, collapse = "+"))), data(), family = binomial)
      predictions = ifelse(predict(model, type = "response") > 0.5, 1, 0)
      confusion_matrix = table(data()[[input$response]], predictions)
      sensitivity = confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
      specificity = confusion_matrix[1, 1] / sum(confusion_matrix[1, ])
      accuracy = mean(predictions == data()[[input$response]])
      list(Sensitivity = sensitivity, Specificity = specificity, Accuracy = accuracy)
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
