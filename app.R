# Install packages, if necessary
library(tidyverse)
library(corrplot)
library(labelled)
library(shiny)
library(shinyalert)
library(shinydashboard)
library(bslib)
library(DT)

source("helpers.R")

cat(file = stderr(), "message\n")

# Set up the UI for the app that allows numerical and graphical EDA
ui <- fluidPage(
  titlePanel("Exploratory Data Analysis of YouTube Videos (Using Synthetic Data)"),
  sidebarLayout(
    sidebarPanel(
      # Select at least two categorical variables for subsetting
      checkboxGroupInput(
        inputId = "catvars",
        label = "Choose at least two variables below for grouping:",
        choices = catvars,
        selected = NULL
      ),
      uiOutput("many_cats"),
      selectInput(
        inputId = "numvar1",
        label = "Choose one numeric variable:",
        choices = numvars,
        selected = NULL
      ),
      sliderInput(
        inputId = "slider1",
        label = "Place bounds on this variable:",
        min = 0,
        max = 10000,
        value = c(0, 10000)
      ),
      selectInput(
        inputId = "numvar2",
        label = "Choose another numeric variable:",
        choices = numvars,
        selected = NULL
      ),
      sliderInput(
        inputId = "slider2",
        label = "Place bounds on this variable also:",
        min = 0,
        max = 10000,
        value = c(0, 10000)
      ),
      actionButton(
        "subset",
        "Subset the Data"
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "About",
          tags$img(src = "YouTube_logo.png", height = "500px", width = "750px"),
          app_description
        ),
        tabPanel(
          "Data Download",
          dataTableOutput("chosen_data"),
          downloadButton(outputId="download_data", label = "Download the Data")
        ),
        tabPanel(
          "Data Exploration",
          checkboxGroupInput(
            inputId = "variable_type",
            label = "Choose what type of variable you're interested in summarizing:",
            choices = c("Numerical", "Categorical"),
            selected = NULL
          ),
          conditionalPanel(
            condition = "input.variable_type == 'Numerical'",
            selectizeInput(
              inputId = "Num_Var",
              label = "Select your numerical variable(s) of interest:",
              choices = numvars,
              selected = NULL
            )
          ),
          conditionalPanel(
            condition = "input.variable_type == 'Categorical'",
            selectizeInput(
              inputId = "Cat_Var",
              label = "Select your categorical variable(s) of interest:",
              choices = catvars,
              selected = NULL
            )
          ),
          radioButtons(
            inputId = "summary_type",
            label = "Choose what type of summary you're interested in:",
            choices = c("Numerical", "Graphical", "Numerical and Graphical"),
            selected = NULL
          ),
          checkboxInput(
            inputId = "grouping",
            label = "Do you want to include a grouping (categorical) variable?"
          ),
          conditionalPanel(
            condition = "input.grouping == 'Yes'",
            selectizeInput(
              inputId = "Group_Var",
              label = "Choose a (categorical) variable to group by:",
              choices = catvars,
              selected = NULL,
              multiple = FALSE
            )
          ),
          uiOutput("summarize"),
          verbatimTextOutput("number_sum"),
          plotOutput("graph_sum")
        )
      )
    )
  ),
  theme = bs_theme(preset = "yeti")
)

# Here is an accessible version of the data set with a few modifications to create categorical variables
viral_data <- read_rds("viral_videos.rds")

# The server function, where most of the dirty work is done, so to speak.
server <- function(input, output, session) {
  
  #viral_data <- reactiveVal(viral_data)
  
  output$many_cats <- renderUI({
    catvar_collect <- lapply(input$catvars, function(catvar) {
      catvar_choices <- levels(viral_data[catvar])

      SelectInput("catvar_levels",
        label = "Choose the levels that you're interested in:",
        choices = catvar_choices,
        selected = NULL,
        multiple = TRUE
      )
    })

    do.call(tagList, catvar_collect)
  })

  updated_sliders <- observeEvent(input$numvar1, input$numvar2, {
    min1 <- min(data[[input$numvar1]])
    max1 <- max(data[[input$numvar1]])
    min2 <- min(data[[input$numvar2]])
    max2 <- max(data[[input$numvar2]])

    updateSliderInput(
      session,
      inputId = "slider1",
      min = min1,
      max = max1,
      values = c(min1, max1)
    )

    updateSliderInput(
      session,
      inputId = "slider2",
      min = min2,
      max = max2,
      values = c(min2, max2)
    )
  })

  selected_data <- observeEvent(input$subset, {
    
    for (k in input$catvars) {
      choice <- paste0("levels ", k)

      if (!is.null(input[[choice]])) {
        viral_data() <- viral_data[viral_data[[k]] %in% input[[choice]], ]
      }
    }

    # Uses updated viral_data object with desired levels for each categorical variable
    viral_data() |>
      select(input$catvars, input$numvar1, input$numvar2)
  })

  output$chosen_data <- renderDataTable(
    selected_data()
  )

  output$download_data <- downloadHandler(
    filename = function() {
      paste0("viral_video_portion_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(selected_data, file, row.names = FALSE)
    }
  )

  output$summarize <- observeEvent(input$summary_type, input$variable_type, {
    if (input$variable_type == "Numerical") {
      if (input$summary_type %in% c("Numerical", "Numerical and Graphical")) {
        radioButtons("numsum_type",
          label = "Choose a type of numerical summary:",
          choices = c("Partial (mean/median/SD)", "Whole"),
          selected = NULL
        )
      } else if (input$summary_type %in% c("Graphical", "Numerical and Graphical")) {
        radioButtons("graphsum_type",
          label = "Choose a type of graphical summary:",
          choices = c("Histogram", "Scatterplot", "Correlation Plot"),
          selected = NULL
        )
      }
    } else if (input$variable_type == "Categorical") {
      if (input$summary_type %in% c("Numerical", "Numerical and Graphical")) {
        radioButtons("numsum_type2",
          label = "Choose a type of numerical summary:",
          choices = c("Contingency Table", "Proportion Table"),
          selected = NULL
        )
      } else if (input$summary_type %in% c("Graphical", "Numerical and Graphical")) {
        radioButtons("graphsum_type2",
          label = "Choose a type of graphical summary:",
          choices = c("Bar Chart", "Box Plot", "Bubble Plot"),
          selected = NULL
        )
      }
    }
  })

  output$number_sum <- renderPrint(
    if (input$numsum_type) {
      if (input$grouping) {
        if (input$numsum_type == "Partial (mean/median/SD)") {
          viral_data() |>
            group_by(input$Group_Var) |>
            select(input$Num_Var) |>
            summarize(Mean = mean(input$Num_Var), Median = median(input$Num_Var), SD = sd(input$Num_Var))
        } else {
          viral_data() |>
            group_by(input$Group_Var) |>
            select(input$Num_Var) |>
            summarize(
              Min = min(input$Num_Var), Q1 = quantile(input$Num_Var, 0.25),
              Mean = mean(input$Num_Var), Median = median(input$Num_Var),
              Q3 = quantile(input$Num_Var, 0.75), Max = max(input$Num_Var),
              SD = sd(input$Num_Var), Variance = var(input$Num_Var)
            )
        }
      }
      if (input$numsum_type == "Partial (mean/median/SD)") {
        viral_data() |>
          select(input$Num_Var) |>
          summarize(Mean = mean(input$Num_Var), Median = median(input$Num_Var), SD = sd(input$Num_Var))
      } else {
        viral_data() |>
          select(input$Num_Var) |>
          summarize(
            Min = min(input$Num_Var), Q1 = quantile(input$Num_Var, 0.25),
            Mean = mean(input$Num_Var), Median = median(input$Num_Var),
            Q3 = quantile(input$Num_Var, 0.75), Max = max(input$Num_Var),
            SD = sd(input$Num_Var), Variance = var(input$Num_Var)
          )
      }
    } else {
      if (input$numsum_type2 == "Contingency Table") {
        Viral_data() |>
          group_by(input$Cat_Var) |>
          summarize(count = n())
      } else {
        Viral_data() |>
          group_by(input$Cat_Var) |>
          summarize(count = n()) |>
          mutate(proportion = count / sum(count))
      }
    }
  )

  output$graph_sum <- renderPlot(
    if (input$graphsum_type) {
      if (input$graphsum_type == "Histogram" & length(input$Num_Var) == 1) {
        ggplot(viral_data(), aes(input$Num_Var)) +
          geom_histogram(binwidth = 100) +
          labs(x = var_label(input$Num_Var), y = "Frequency", title = paste0("Histogram of ", var_label(input$Num_Var)))
      } else if (input$graphsum_type == "Scatterplot") {
        if (length(input$Num_Var) < 2) {
          shinyalert(title = "A second numeric variable is required in order to produce a scatterplot!")
        }
        if (input$grouping) {
          ggplot(viral_data(), aes(x = input$Num_Var[1], y = input$Num_Var[2], color = factor(input$Cat_Var))) +
            geom_points() +
            labs(
              x = var_label(input$Num_Var[1]), y = var_label(input$Num_Var[2]), color = var_label(input$Group_Var),
              title = paste0("Scatterplot of ", var_label(input$Num_Var[1]), " vs. ", var_label(input$Num_Var[2]), " Grouped by ", var_label(input$Group_Var))
            )
        }
        ggplot(viral_data(), aes(x = input$Num_Var[1], y = input$Num_Var[2])) +
          geom_points() +
          labs(
            x = var_label(input$Num_Var[1]), y = var_label(input$Num_Var[2]),
            title = paste0("Scatterplot of ", var_label(input$Num_Var[1]), " vs. ", var_label(input$Num_Var[2]))
          )
      } else {
        if (length(input$Num_Var) < 2) {
          shinyalert(title = "A second numeric variable is required in order to produce a correlation plot!")
        }
        corrplot.mixed(cor(input$Num_Var),
          upper = "number",
          lower = "square",
          order = "hclust",
          title = paste0("Correlation Plot of ", var_label(input$Num_Var))
        )
      }
    } else {
      if (input$graphsum_type2 == "Bar Chart" & length(input$Cat_Var) == 1) {
        ggplot(viral_data(), aes(input$Cat_Var)) +
          geom_bar() +
          labs(x = var_label(input$Cat_Var), title = paste0("Barplot of ", var_label(input$Cat_Var)))
      } else if (input$graphsum_type2 == "Box Plot" & (length(input$Cat_Var) == 1 & length(input$Num_Var) == 1)) {
        if (is.null(input$Num_Var)) {
          shinyalert(title = "You need a numeric variable in order to produce a boxplot")
        }
        ggplot(viral_data(), aes(z = input$Cat_Var, y = input$Num_Var)) +
          geom_boxplot() +
          labs(
            x = var_label(input$Cat_Var), y = var_label(input$Num_Var),
            title = paste0("Boxplot of ", var_label(input$Num_Var), " Grouped by ", var_label(input$Cat_Var))
          )
      } else {
        if (length(input$Cat_Var) < 2) {
          shinyalert(title = "You need a second categorical variable in order to do a bubble plot!")
        }
        ggplot(viral_data(), aes(x = input$Cat_Var[1], y = input$Cat_Var[2])) +
          geom_count() +
          labs(
            x = var_label(input$Cat_Var[1]), y = var_label(input$Cat_Var[2]),
            title = paste0("Bubble Plot of ", var_label(input$Cat_Var[2]), " Grouped by ", var_label(input$Cat_Var[1]))
          )
      }
    }
  )

  # I do not have a good feeling about this part; lots more changes will likely be made.
}

# We'll combine these and see what happens!
shinyApp(ui = ui, server = server)

# Below is information for getting the Shiny app uploaded to shinyapps.io
# install.packages('rsconnect')
# rsconnect::setAccountInfo(name='cashby890',
#                          token='5F49AC8FDEE906233A8FF5BBF00F9AEF',
#                          secret='5oePbydl8bso/VmsrP6cS0BzfOuBOALEbBdA48Y6')
# library(rsconnect)
# rsconnect::deployApp('path/to/your/app')
