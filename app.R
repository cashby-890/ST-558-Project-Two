#Install packages, if necessary
library(tidyverse)
library(shiny)
library(shinyalert)
library(shinydashboard)
library(bslib)
library(DT)

source("helpers.R")

#The original dataset will come from here.
viral_videos=read.csv("C:/Users/cashb/OneDrive/Documents/ST-558-Project-Two/viral_videos.csv",header=TRUE)

#Set up the UI for the app that allows numerical and graphical EDA
ui <- fluidPage(
  titlePanel("Exploratory Data Analysis of YouTube Videos (Using Synthetic Data)"), 
  sidebarLayout(
    sidebarPanel(
      #Select at least two categorical variables for subsetting
      checkboxGroupInput(inputId="catvars",
                         label="Choose at least two variables below for grouping:",
                         choices=catvars,
                         selected=NULL),
      selectInput(inputId="numvar1",
                  label="Choose one numeric variable:",
                  choices=numvars,
                  selected=NULL),
      sliderInput(inputId="slider1",
                  label="Place bounds on this variable:",
                  min=0,
                  max=10000,
                  value=c(0,10000)),
      selectInput(inputId="numvar2",
                  label="Choose another numeric variable:",
                  choices=numvars,
                  selected=NULL),
      sliderInput(inputId="slider2",
                  label="Place bounds on this variable also:",
                  min=0,
                  max=10000,
                  value=c(0,10000)),
      actionButton("subset",
                   "Subset the Data"),
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("About",
                 tags$img(src="YouTube_logo.png",height="500px",width="750px"),
                 app_description),
        tabPanel("Data Download",
                 dataTableOutput("chosen_data"),
                 downloadButton("download_data",label="Download the Data")),
        tabPanel("Data Exploration",
                 checkboxInput(inputId="variable_type",
                               label="Choose what type of variable you're interested in summarizing:",
                               choices=c("Numerical","Categorical"),
                               selected=NULL),
                 conditionalPanel(condition = "input.variable_type == 'Numerical'",
                                  selectizeInput(inputId="Num_Var",
                                                 label="Select your numerical variable(s) of interest:",
                                                 choices=numvars,
                                                 selected=NULL)),
                 conditionalPanel(condition = "input.variable_type == 'Categorical'",
                                  selectizeInput(inputId="Cat_Var",
                                                 label="Select your categorical variable(s) of interest:",
                                                 choices=catvars,
                                                 selected=NULL)),
                 radioButtons(inputId="summary_type",
                              label="Choose what type of summary you're interested in:",
                              choices=c("Numerical","Graphical","Numerical and Graphical"),
                              selected=NULL),
                 conditionalPanel(condition = "input.summary_type == 'Numerical'",
                                  ,)
                 verbatimTextOutput("number_sum"),
                 plotOutput("graph_sum")
                 )
      )
    )
  ),
  theme = bs_theme(preset = "yeti")
)

#Here is an accessible version of the data set with a few modifications to create categorical variables
viral_data <- read_rds("C:/Users/cashb/OneDrive/Documents/ST-558-Project-Two/viral_videos.rds")

#The server function, where most of the dirty work is done, so to speak.
server <- function(input,output,session){
  
  selected_data <- eventReactive(input$subset,{
    
    if(length(input$catvars) < 2){
      stop("Please select another categorical variable.")
    }
    
    viral_data |>
      select(input$catvars,input$numvar1,input$numvar2)
    
  })
  
  output$chosen_data <- renderDataTable(
    
    selected_data()
    
  )
  
  output$download_data <- downloadHandler(
    filename = function(){
      paste0("viral_video_portion_",Sys.Date(),".csv")
    }
    content = function(file){
      write.csv(selected_data,file,row.names = FALSE)
    }
  )
  
  observeEvent(input$variable_type,{
    
    if(input$variable_type == "Numerical"){
      updateSelectizeInput(session,"Num_Var",choices=numvars,selected=NULL)
    }
    else{
      updateSelectizeInput(session,"Cat_Var",choices=catvars,selected=NULL)
    }
    
  })
   
  
  output$number_sum <- renderPrint(
    

  )
  
  output$graph_sum <- renderPlot(
    
  )
  #Lots and lots of stuff will go here! Fret not, reactive contexts will be included.
}

#We'll combine these and see what happens!
shinyApp(ui = ui, server = server)

# Below is information for getting the Shiny app uploaded to shinyapps.io
install.packages('rsconnect')
rsconnect::setAccountInfo(name='cashby890',
                          token='5F49AC8FDEE906233A8FF5BBF00F9AEF',
                          secret='5oePbydl8bso/VmsrP6cS0BzfOuBOALEbBdA48Y6')
library(rsconnect)
rsconnect::deployApp('path/to/your/app')
