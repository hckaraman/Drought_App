library(shiny)
library(RSQLite)
library(leaflet)
library(plotly)
library(shinycssloaders)

shinyUI(pageWithSidebar(
  headerPanel("Drought Analysis"),
  sidebarPanel(selectInput("Vector", "Select Station", stations$Istasyon_No, selected = 17192, multiple = FALSE),
               selectInput("Freq", "Select Frequency", c(1,3,6,9,12,24,36,48), selected = 9, multiple = FALSE),
               selectInput("Index", "Select Index", c('SPI','SPEI'), selected = 'SPI', multiple = FALSE),
               selectInput("PET", "Select PET Formulation", c('Thornthwaite','Hargreaves'), selected = 'Thornthwaite', multiple = FALSE),
               selectInput("Dist", "Select Distribution", c('log-Logistic','Gamma','PearsonIII'), selected = 'SPI', multiple = FALSE),
               downloadButton("downloadData", "Download Results")),
  mainPanel(
    
    tabsetPanel(type = "tabs",
                tabPanel("Data", withSpinner(plotlyOutput(outputId ="data_plot", height = "800px"),type = 6),
                         ),
                tabPanel("Data Summary",
                         fluidRow(
                           withSpinner(plotOutput("mary"),type = 6),
                         verbatimTextOutput("at"))),
                tabPanel("Data Summary", withSpinner(DT::dataTableOutput("ysummary"),type = 6),verbatimTextOutput("total"),
                         plotlyOutput("totalplot")),
                tabPanel("SPI Plot", withSpinner(plotOutput("main_plot", height = "800px"),type = 6)),
                tabPanel("SPI Summary", withSpinner(verbatimTextOutput("summary"),type = 6)),
                tabPanel("PET Plot", withSpinner(plotlyOutput(outputId ="pet_plot", height = "500px"),type = 6)),
                tabPanel("Map", withSpinner(leafletOutput("mymap",height = "800px"),type = 6)),
                tabPanel("Update db",     fluidRow( column(6,fileInput(
                  'file1',
                  'Choose CSV File',
                  accept = c('text/csv',
                             'text/comma-separated-values,text/plain',
                             '.csv')
                ))),
                fluidRow(
                  verbatimTextOutput("updatetable")
                ),
                fluidRow(
                  DT::dataTableOutput("templatetable")
                ),
                fluidRow(
                  DT::dataTableOutput("update")
                ),
                fluidRow( 
                  plotlyOutput(outputId ="update_plot", height = "800px"),
                )
                
                
                )
                
    
    #plotOutput("main_plot", height = "800px")
    
    )
)))