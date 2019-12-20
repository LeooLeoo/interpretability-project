library(shiny)
library(shinydashboard)
library(tree)
library(ISLR)
library(randomForest)
library(e1071)
library(caret)
library(lime)
library(dplyr)
library(lattice)
library(ggplot2)

#the data is fixed here
train <- read.csv(file="../data/processed_data/deployed_data/final_model_train.csv")[,-1]
#lapply(train,class)
train$Gender <- as.factor(train$Gender)

#Train the model
set.seed(3)
rf.data <-randomForest(train$SepsisLabel~.,train,ntree=300)

#LIME
expln <- lime(train, as_classifier(rf.data), n_bins = 4)



ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "LIME"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Prediction", tabName = "prediction", icon = icon("desktop")),
      menuItem("Model details", tabName = "modelDetails", icon = icon("info"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      tabItem(tabName = "modelDetails",
              fluidRow(
              column(width = 10,
                     box(width = NULL, status = "danger",
                         HTML("<h1 style='text-align: center; color:red; font-weight: bold;'>LIME</h1>"),
                         HTML("<img src='/lime.png' style='display: block; margin-left: auto; margin-right: auto; width: 80%;'/>"),
                         HTML("<p style='text-align: center; color:green; font-weight: bold; font-style: italic;'>Local Interpretable Model-agnostic Method</p>")
                     )
              ),
              column(width = 10,
                     box(width = NULL, status = "danger",
                         h3("Usage"),
                         p(align = "justify", "By navigating to the prediction tab, you will be able to enter the variables this tool needs to calculate its prediciton. In total, this tool needs information about 13 different
                           parameters. When all of them are entered, press the Apply button to get the prediction."),
                         h3("Background knowledge"),
                         HTML("<p align='justify'>Local Interpretable Model-agnostic Method is an algorithm for any machine lerning classifiers to provide a local explanation. We recommend you to read more details of this algorithm
                                by reading this <a href='https://arxiv.org/pdf/1606.05386.pdf'>paper</a>.</p>"),
                         h3("About us"),
                         HTML("<p align='justify'>We are Health Informatics students from <a href='https://ki.se'>Karolinska Institutet</a> in Stockholm. If you are interested 
                              in our source code or would like to get in touch with us, please have a look at our <a href='https://github.com/LeooLeoo/heartspot'>GitHub</a> repository.
                              The system you have accessed is an AI-powered web application build for health care professionals to predict the presence of sepsis in ICU setting.
                              We have implemented a machine learning model along with LIME, and used the <a href='https://physionet.org/content/challenge-2019/1.0.0/'>PhysioNet 2019 challenge dataset</a> to train and test this system.</p>")
                         )
                         )
                     )
              ),
      tabItem(tabName = "prediction",
              fluidRow(
                column(width = 7,
                       h2("LIME explanation"),
                       box(width = NULL, status = "danger",
                           plotOutput('plot1'),
                           actionButton(inputId="button", label="Apply to input")
                       ),
                       box(width = NULL, title = "Lab results", status = "danger",
                           numericInput('FiO2', 'Fraction of inspired oxygen % (FiO2)','40', width = "80%"),
                           numericInput('Creatinine', 'Creatinine (mg/dL)', '1.20', width = "80%"),
                           numericInput('Bilirubin_total', 'Total bilirubin (mg/dL)', '1.7', width = "80%"),
                           numericInput('Platelets', 'The number of Platelets (count*10^3/??L)', '150', width = "80%")
                       )
                ),
                column(width = 5,
                       h2("Data Input"),
                       box(width = NULL, title = "Demographics", status = "danger",
                           numericInput('Age', 'Age','65', width = "80%"),  
                           selectInput('Gender', 'Gender',  c('male','female'), width = "80%")
                       ),
                       box(width =NULL , title = "Vital sign", status = "danger",
                           sliderInput('Temp', 'Temperature (Deg C)', 34,42,37.8,0.1),
                           numericInput('HR', 'Heart Rate (beats per minute)', '130', width = "80%"),
                           numericInput('Resp', 'Respiration Rate (breaths per minute)', '20', width = "80%"),
                           numericInput('O2Sat', 'Pulse oximetry (%)', '93', width = "80%"),
                           numericInput('SBP', 'Systolic BP (mm Hg)','120', width = "80%"),
                           numericInput('DBP', 'Diastolic BP (mm Hg)', '80', width = "80%"),
                           numericInput('MAP', 'Mean arterial pressure (mm Hg)', '90', width = "80%")
                       )
                       
                       
                )
              )
      )
    ),
    tags$script("document.getElementById('button').addEventListener('click', function(){
                alert('For demo purpose only');
    });")
  )
)




server <- function(input, output) {
  #input
  gender <- reactive({
    if (input$Gender == 'male'){
      gender <- 1
    } else if(input$Gender == 'female'){
      gender <- 0
    } 
  })


  observeEvent(input$button, {
    userfeatures <- data.frame(
      HR = input$HR,
      O2Sat = input$O2Sat,
      Temp = input$Temp,
      SBP = input$SBP,
      MAP = input$MAP,
      DBP = input$DBP,
      Resp = input$Resp,
      FiO2 = input$FiO2*0.01,
      Creatinine = input$Creatinine,
      Bilirubin_total = input$Bilirubin_total,
      Platelets = input$Platelets,
      Age = input$Age,
      Gender = as.factor(gender())
      )
    
    explanation <- explain(x=userfeatures, 
                           expln,
                           n_labels = 1,
                           n_features = 5,
                           feature_select = "highest_weights")
    output$plot1 <- renderPlot({ 
      plot_features(explanation)
    })
    
    
    # output$result <- renderUI({ 
    #   
    #   result <- ""
    #   color <- ""
    #   
      # if (pred == 1){
      #   result <- "high"
      #   color <- "red"
      # } else{
      #   result <- "low"
      #   color <- "blue"
      # }
      # HTML(sprintf("
      #      <p align='justify'>Based on your input, this patient is at a <span style='color:%s; font-weight:bold;'>%s</span> 
      #              risk of having or developing Heart Disease. If you would like to understand and recreate the prediction, you have 
      #              to follow the corresponding path in the Decision Tree provided above. The following table explains the feature 
      #              abbreviations.</p>
      #              <img src='features.png' style='display: block; margin-left: auto; margin-right: auto; width: 80%%;'/>
      #              ", color, result)
      # )
    # })
  })
  
  
}

shinyApp(ui = ui, server = server)
