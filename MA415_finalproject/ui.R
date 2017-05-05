
library(shiny)
library(ggplot2)
library(plotly)

load(file = "shiny_data.Rdata") 
# This is the list of the states that I will be using throughout the whole app
list_of_states <- list("All", "Alaska", "Alabama", "Arkansas", "Arizona", "California", 
                       "Colorado", "Connecticut", "District of Columbia", "Delaware", 
                       "Florida", "Georgia", "Hawaii", "Iowa", "Idaho", "Illinois", 
                       "Indiana", "Kansas", "Kentucky", "Louisiana", "Massachusetts", 
                       "Maryland", "Maine", "Michigan", "Minnesota", "Missouri", 
                       "Mississippi", "Montana", "North Carolina", "North Dakota", 
                       "Nebraska", "New Hampshire", "New Jersey", "New Mexico", "Nevada", 
                       "New York", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", 
                       "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Virginia", 
                       "Vermont", "Washington", "Wisconsin", "West Virginia", "Wyoming")

# these are the names of the categories that I use in the graph below
nms <- list("Gender"= "gender", "Age"= "age", "Severity"= "sev", "Race" = "race")



# beginning of the fluid page
shinyUI(fluidPage(
  tags$img(height = 100, src="nfirs_header.gif"),
  titlePanel("Victims of Fire Incidents in the US (2006-2015)", windowTitle = "title"),
 tags$p("I have decided to focus on the victims of fire incidents in the US. 
        More specifically, the number as well as their characteristics such as their location, gender, age etc. 
        I aim at finding patterns within these variables. "), 
 tags$p("The structure of the analysis therefore answering three key questions about the victims:"),
 tags$ol(
 tags$li("Where - where are the victims located?"),
 tags$li("Who - what are the characteristics of the victims?"),
 tags$li("When - when are there more victims, over time, period of year etc... ?")
 ),
  
tags$p("Both the", tags$a(href = "https://github.com/sezzagore/final_project/blob/master/MA415_finalproject/ui.R", "UI"), "and", tags$a(href = "https://github.com/sezzagore/final_project/blob/master/MA415_finalproject/server.R", "Server"), "files can be found on my Github."), 
tags$p("The data used has been cleaned and prepared as explained in my", tags$a(href = "https://github.com/sezzagore/final_project/blob/master/Final_project_preparation.pdf","preparation document.")), 

  tags$h1("Where are the victims located?"),
  tags$p("This section provides information about the location of the victims. You can visualize by state or breakdown by county. You can also choose the time period of interest."),
  tags$p("The table on the right tab provides the same information in a tabular format."),
  tags$br(),
sidebarLayout(
    sidebarPanel(
      selectizeInput("state", "Choose a State", choices = list_of_states, multiple = FALSE,
      selected = "Massachusetts"),
      sliderInput("range", "Time Period", min = 2006, max = 2015, value = c(2006,2015))
    ),
    
    
    mainPanel(
      tabsetPanel( tabPanel("Map", plotOutput("map01")), 
                   tabPanel("Table", DT::dataTableOutput("table01")))
    
    )
  ),
  
 tags$h1("Who are the victims?"),
 tags$h2("Characteristics of victims"),
tags$br(),
 tags$p("This section provides information about the characteristics of the victims."),
 tags$br(),
  sidebarPanel(
    checkboxInput("allstates",
                  "All states", value = TRUE),
    conditionalPanel(
      condition = "!input.allstates",
      selectizeInput("state4", "Choose a State", choices = list_of_states[-1], multiple = FALSE, selected = "Massachusetts")
        ),
    selectInput('x', 'X variable', choices = nms, selected = "gender"),
    selectInput('color', 'Color', choices = nms, selected = "sev"),
    selectInput('facet_row', 'Facet', c(None = '.', nms), selected = "None")
    
  ),
  
  mainPanel(
   plotOutput("trendPlot")
   
  ),
tags$br(),  
tags$br(),
# I wanted to implement a test in order to see "who" the victims were i.e. by gender and by race
# My test compares the two proportions in the population
tags$h3(" Testing for probabilities of being a victim within the characteristics"),
tags$p("The following test compares two sub-samples of the population and evaluates whether or 
       not if individuals in each sub-sample have the same probabilty of being victims in a fire incident."),
tags$br(),
sidebarLayout(
  sidebarPanel(
    helpText("Chose the characteristics of the sample."), # Instruction to reader
    checkboxInput("allstates2", "All states", value = TRUE),
    conditionalPanel(condition = "!input.allstates2",
    selectizeInput("state5", "Choose a State", choices = list_of_states[-1], multiple = FALSE, selected = "Massachusetts")),
    
    checkboxInput("allages2", "All ages", value = TRUE),
    conditionalPanel(condition = "!input.allages2",
    sliderInput("range2", "Age range", min = 1, max = 100, value = c(1,100))),
    
    checkboxInput("allsev2", "All types of injury", value = TRUE),
    conditionalPanel(condition = "!input.allsev2",
    selectizeInput("type2", "Age range", choices = levels(df$sev), multiple= TRUE, selected="Minor")),
    
    helpText("Compare the probabilities of being in the fire incident for subsamples over:"),
    radioButtons("sub2", "Subsamples", choices = list("Female vs Male"="g", "Black vs White"="r"),selected = "Female vs Male"),
    actionButton("button", "Calculate") #reader has to recalcuate (click button) for every change in the selection process
  ), #end side bar
  mainPanel(
    verbatimTextOutput("tt") # this gives an output similar to that in the console
  )) ,



tags$h1("When did the victims endure injury?"),
tags$h3("Evolution over time (in years)"),
tags$p("This section provides information about when the incidents occured over time."),
tags$br(),
sidebarLayout(
  sidebarPanel(
    selectizeInput("state2", "Choose a/many States", choices = list_of_states[-1], multiple = TRUE, selected = c("Massachusetts","Alabama")),
    checkboxInput("adjust", 
                  "Casualties per 100'000 people", value = FALSE)
  ),
  
  
  mainPanel(
    plotlyOutput("map02")
    
  )
),
  
# I wanted to track the distribution of casualties over time 
tags$h3("Evolution over time (in months and weekdays)"),
tags$p("In this section you can breakdown the number of victims on average by month and day of the week"),
tags$br(),
sidebarLayout(
  sidebarPanel(
    selectizeInput("state6", "Choose a State", choices = list_of_states, multiple = FALSE, selected = "All"),
    radioButtons("button1", "Choose Period of Interest",
                  choices = list("Month", "Weekday"),selected = "Month", inline = FALSE)
  ),
  
  
  mainPanel(
   plotOutput("time")
    
  )
),
tags$br(),
tags$br(),
tags$br(),
tags$p(strong(" Sarah Gore, MA415 Final Project, Spring 2017 "))
)) # end of ui 
