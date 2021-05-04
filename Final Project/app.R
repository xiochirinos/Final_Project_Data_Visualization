# DATA VISUALIZATION - FINAL PROJECT: MALL CUSTOMER SHINY DASHBOARD - XIOMARA CHIRINOS 
library(tidyverse)
library(forecast)
library(caTools)
library(earth)
library(randomForest)
library(kernlab)
library(h2o)
library(neuralnet)
library(Metrics)
library(caret)
library(hms)
library(lubridate)
library(ggplot2)
library(png)
library(plotly)
library(shiny)
library(shinydashboard)
library(DT)


# Remove scientific notations
options(scipen = 999)


# MODEL DATA IMPORT AND TRANSFORMATIONS
Mall_customers <- read.csv("Mall_Customers.csv", stringsAsFactors = T)

Mall_customers <- Mall_customers %>% rename(
    Annual_Income = Annual.Income..k..,
    Spending_Score = Spending.Score..1.100.)

Mall_customers <- Mall_customers %>%
    select(Genre, Age, Annual_Income, Spending_Score)

Mall_customers <- Mall_customers %>% mutate(
    Gender = if_else(Genre == "Male", true = 1, false = 0)
) %>% 
    select(-Genre)
Mall_customers

    ## DATA SPLIT

set.seed(123)

sample = sample.split(Mall_customers$Gender, SplitRatio = .75)
train = subset(Mall_customers, sample == TRUE)
test = subset(Mall_customers, sample == FALSE)

        
# NEURAL NETWORK
        
        ## Neural Networks Model
        NNModel <- neuralnet(Gender ~ .,
                             data = train)
        
        ## Prediction
        NNPrediction <- predict(NNModel, test)
        
        ## Generate Confusion Matrix
        NNconfusionMatrix <- table(NNPrediction,
                                   test$Gender,
                                   dnn = c("Prediction", "Actual"))
        
        ## Calculate Accuracy
        NNaccuracy <- round(sum(diag(NNconfusionMatrix))/ sum(NNconfusionMatrix), digits = 4)
        
# DEEP LEARNING NEURAL NETWORK
        
        ## Neural Networks Model
        DNNModel <- neuralnet(Gender ~ .,
                              data = train,
                              hidden = 4)
        
        ## Prediction
        DNNPrediction <- predict(DNNModel, test)
        
        ## Generate Confusion Matrix
        DNNconfusionMatrix <- table(DNNPrediction,
                                    test$Gender,
                                    dnn = c("Prediction", "Actual"))
        
        ## Calculate Accuracy
        DNNaccuracy <- round(sum(diag(DNNconfusionMatrix))/ sum(DNNconfusionMatrix), digits = 4)
        
# RAMDON FOREST
        
        ## Random Forest Model
        RFModel <- randomForest(Gender ~ ., data = train, mtry = 3, ntree = 64)
        
        ## Prediction
        RFPrediction <- predict(RFModel, test, type = "class")
        
        ## Generate Confusion Matrix
        RFconfusionMatrix <- table(RFPrediction,
                                   test$Gender,
                                   dnn = c("Prediction", "Actual"))
        
        ## Calculate Accuracy
        RFaccuracy <- round(sum(diag(RFconfusionMatrix))/ sum(RFconfusionMatrix), digits = 4)
        
# SUPPORT VECTOR MACHINE
        
        ## Support Vector Machine Model
        SVMmodel <- ksvm(Gender ~ .,
                         data = train,
                         kernel = "vanilladot")

        ## Prediction
        SVMpred <- predict(SVMmodel, test)
        
        ## Generate Confusion Matrix
        SVMconfusionMatrix <- table(SVMpred,
                                    test$Gender,
                                    dnn = c("Prediction", "Actual"))
        
        ## Calculate Accuracy
        SVMaccuracy <- round(sum(diag(SVMconfusionMatrix))/ sum(SVMconfusionMatrix), digits = 4)

 
# AUTO MACHINE LEARNING
        
        localH2O = h2o.init()
        
        ## Convert the data frame to an H2O Data Frame
        autoMall_customers <- as.h2o(Mall_customers)
        
        ## Sample Data
        autoSplit <- h2o.splitFrame(data = autoMall_customers, ratios = c(.75))
        AMLtrain <- autoSplit[[1]]
        AMLtestValidation <- autoSplit[[2]]
        
        testValidationSplit <- h2o.splitFrame(data = AMLtestValidation, ratios = c(.75))
        AMLtest <- testValidationSplit[[1]]
        AMLvalidation <- testValidationSplit[[2]]
        
        ## AutoML
        autoMLModel <- h2o.automl(y = "Gender",
                                  x = c("Age", "Annual_Income", "Spending_Score"),
                                  training_frame = AMLtrain,
                                  validation_frame = AMLvalidation,
                                  balance_classes = TRUE,
                                  max_runtime_secs = 60,
                                  seed = 1234)
        
        ## Prediction
        AMLprediction = h2o.predict(object = autoMLModel, newdata = AMLtest)
        
        AutoMLtable <- as.data.frame(h2o.get_leaderboard(object = autoMLModel, extra_columns = 'ALL'))
        
        ## Performance
        print(h2o.performance(autoMLModel@leader, AMLtest))


# PLOT DATA
Mall_customers_Plot1 <- read.csv("Mall_Customers.csv")

Mall_customers_Plot1 <- Mall_customers_Plot1 %>% rename(
    Annual_Income = Annual.Income..k..,
    Spending_Score = Spending.Score..1.100.,
    Gender = Genre)

Mall_customers_Plot1 <- Mall_customers_Plot1 %>%
    select(Gender, Age, Annual_Income, Spending_Score)

Mall_customers_Plot1$Gender <- as.factor(Mall_customers_Plot1$Gender)


# SHINY DASHBOARD

# Sidebar 
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("GitHub", icon = icon("github"), href = "https://github.com/xiochirinos/Final_Project_Data_Visualization"),
        menuItem("Overview", icon = icon("chart-bar"), tabName = "OV"),
        menuItem("Neural Networks", icon = icon("brain"), tabName = "NN"),
        menuItem("Deep Learning Neural Networks", icon = icon("project-diagram"), tabName = "DNN"),
        menuItem("Random Forest", icon = icon("tree"), tabName = "RF"),
        menuItem("Support Vector Machine", icon = icon("desktop"), tabName = "SVM"),
        menuItem("AutoML", icon = icon("magic"), tabName = "AML")
    )
)

body <- dashboardBody(
    tabItems(
        tabItem(tabName = "OV",
                h2("Overview"),
                fluidRow(
                    box(
                        title = "Annual Income per Age", status = "primary", solidHeader = TRUE,
                        collapsible = TRUE,
                        plotlyOutput("Age_AnnualIncome")
                    ),
                    
                    box(
                        title = "Spending Score per Age", status = "primary", solidHeader = TRUE,
                        collapsible = TRUE,
                        plotlyOutput("Age_SpendingScore")
                    ),
                    
                    box(
                        title = "Annual Income vs. Spending Score", status = "primary", solidHeader = TRUE,
                        collapsible = TRUE,
                        tags$img(src = "AnnualIncome_SpendingScore.gif", height = "400px", width = "600px")
                    ),
                    
                    box(
                        title = "Inputs", status = "warning", solidHeader = TRUE,
                        selectInput(inputId = "category",
                                    label = h4("Gender"),
                                    choices = c(levels(Mall_customers_Plot1$Gender))
                        ),
                        sliderInput(inputId = "point",
                                    label = h4("Size:"),
                                    min = 18,
                                    max = 70,
                                    value = 70)
                    )
                    
                )

        ),
        tabItem(tabName = "NN",
                h2("Neural Networks"),
                fluidRow(

                    box(
                        title = "Plot", status = "primary", solidHeader = TRUE,
                        collapsible = TRUE,
                        tags$img(src = "NNmodelPlot2.png", height = "400px", width = "600px"),
                        
                        valueBox(
                            value = 18.47 ,
                            subtitle = "Error",
                            color = "yellow",
                            icon = icon("brain")
                        ),
                        
                        valueBox(
                            value = 34 ,
                            subtitle = "Steps",
                            color = "yellow",
                            icon = icon("brain")
                        ),
                        
                        valueBox(
                            value = paste(NNaccuracy),
                            subtitle = "Accuracy",
                            color = "yellow",
                            icon = icon("brain")
                        )
                    )

                        )
        ),
        tabItem(tabName = "DNN",
                h2("Deep Learning Neural Networks"),
                fluidRow(
                    box(
                        title = "Plot", status = "primary", solidHeader = TRUE,
                        collapsible = TRUE,
                        tags$img(src = "DNNmodelPlot2.png", height = "400px", width = "600px"),
                        
                        valueBox(
                            value = 16.23,
                            subtitle = "Error",
                            color = "yellow",
                            icon = icon("project-diagram")
                        ),
                        
                        valueBox(
                            value = 14848,
                            subtitle = "Steps",
                            color = "yellow",
                            icon = icon("project-diagram")
                        ),
                        
                        valueBox(
                            value = paste(DNNaccuracy),
                            subtitle = "Accuracy",
                            color = "yellow",
                            icon = icon("project-diagram")
                        )
                    )
                )
        ),
        tabItem(tabName = "RF",
                h2("Random Forest"),
                fluidRow(
                    box(
                        title = "Plot", status = "primary", solidHeader = TRUE,
                        collapsible = TRUE,
                        tags$img(src = "RFModelPlot.png", height = "400px", width = "600px"),
                        
                        valueBox(
                            value = 64,
                            subtitle = "Number of Trees",
                            color = "yellow",
                            icon = icon("tree")
                        ),
                        
                        valueBox(
                            value = 3,
                            subtitle = "Variables tried at each split",
                            color = "yellow",
                            icon = icon("tree")
                        ),
                        valueBox(
                            value = paste(RFaccuracy),
                            subtitle = "Accuracy",
                            color = "yellow",
                            icon = icon("tree")
                        )
                    )
                )
        ),
        tabItem(tabName = "SVM",
                h2("Support Vector Machine"),
                fluidRow(
                    # A static valueBox
                    valueBox(135, "Number of Support Vectors", icon = icon("quote-right"), color = "yellow"),

                    # A static valueBox
                    valueBox(-119.3168, "Training error", icon = icon("quote-right"), color = "yellow"),
                    
                    # A static valueBox
                    valueBox(1.606914, "Objective Function Value", icon = icon("quote-right"), color = "yellow"),
                    
                    # A static valueBox
                    valueBox(SVMaccuracy, "Accuracy", icon = icon("quote-right"), color = "yellow"),
                    
                )),
 
        tabItem(tabName = "AML",
                h2("Auto Machirne Learning"),
                dataTableOutput("AutoMLtable")
        )
    )
)
    
    

# MAIN DASHBOARD
ui <- fluidPage(
    
    # Dashboard Page
    dashboardPage( skin = "yellow",
        dashboardHeader(title = "Mall Customers"),
        sidebar,
        body
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

    # AML TABLE
    
    output$AutoMLtable <- renderDataTable(AutoMLtable)
    
    
    # PLOT 1
    
    output$Age_AnnualIncome <- renderPlotly({
        

        # PLOT
        Age_AnnualIncome <- ggplot(Mall_customers_Plot1 %>%
                                       filter(Gender == input$category) %>%
                                       filter(Age <= input$point), 
                                  aes(x = Age, y = Annual_Income, fill = Gender)) +
            geom_bar(alpha = 0.7, stat = "identity", position = "dodge") +
            theme_minimal() +
            scale_fill_manual(values = c("orange")) +
            theme(legend.position = "none") +
            theme(axis.text.x = element_text(face = "bold", size = 10, angle = 90)) +
            theme(axis.text.y = element_text(face = "bold", size = 10)) +
            theme(axis.title.x = element_text(size = 15, face = "bold"),
                  axis.title.y = element_text(size = 15, face = "bold")) +
            labs(x = 'Age',
                 y = 'Annual Income')
        
        ggplotly(Age_AnnualIncome)
    })
    
    
    # PLOT 2
    output$Age_SpendingScore <- renderPlotly({
        

        # PLOT
        
        Age_SpendingScore <- ggplot(Mall_customers_Plot1 %>%
                                        filter(Gender == input$category) %>%
                                        filter(Age <= input$point),
                                    aes(x = Age, y = Spending_Score, fill = Gender)) +
            geom_bar(alpha = 0.7, stat = "identity", position = "dodge") +
            theme_minimal() +
            scale_fill_manual(values = c("orange")) +
            theme(legend.position="none") +
            theme(axis.text.x = element_text(face = "bold", size = 10, angle = 90)) +
            theme(axis.text.y = element_text(face = "bold", size = 10)) +
            theme(axis.title.x = element_text(size = 15, face = "bold"),
                  axis.title.y = element_text(size = 15, face = "bold")) +
            labs(x = 'Age',
                 y = 'Spending Score')
        
        ggplotly(Age_SpendingScore)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
