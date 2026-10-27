#Install packages, if necessary
library(tidyverse)
library(shiny)
library(shinyalert)
library(shinydashboard)
library(bslib)

#Is source() necessary?

#The original dataset will come from here.
viral_videos=read.csv("C:/Users/cashb/OneDrive/Documents/ST-558-Project-Two/viral_videos.csv",header=TRUE)

#Set up the UI for the app that allows numerical and graphical EDA
ui <- page_sidebar(
  sidebar = sidebar(
    #I really want to make this look neat. Bear with me!
  )
)

#Here is an accessible version of the data set with a few modifications to create categorical variables
viral_data <- read_rds("C:/Users/cashb/OneDrive/Documents/ST-558-Project-Two/viral_videos.rds")

#The server function, where most of the dirty work is done, so to speak.
server <- function(input,output,session){
  #Lots and lots of stuff will go here! Fret not, reactive contexts will be included.
}