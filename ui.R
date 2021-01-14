library(shiny)
library(RSQLite)
library(leaflet)
library(plotly)

conn <- dbConnect(RSQLite::SQLite(), './Data/data.db')
query = "SELECT DISTINCT Istasyon_No from data ;"
data = dbGetQuery(conn, query)




shinyUI(pageWithSidebar(
  headerPanel("Drought Analysis"),
  sidebarPanel(selectInput("Vector", "Select Station", data$Istasyon_No, selected = 17192, multiple = FALSE),
               selectInput("Freq", "Select Frequency", c(1,3,6,9,12,24,36,48), selected = 9, multiple = FALSE),
               selectInput("Index", "Select Index", c('SPI','SPEI'), selected = 'SPI', multiple = FALSE),
               selectInput("PET", "Select PET Formulation", c('Thornthwaite','Hargreaves'), selected = 'Thornthwaite', multiple = FALSE),
               selectInput("Dist", "Select Distribution", c('log-Logistic','Gamma','PearsonIII'), selected = 'SPI', multiple = FALSE),
               downloadButton("downloadData", "Download Results")),
  mainPanel(
    
    tabsetPanel(type = "tabs",
                tabPanel("Data", plotlyOutput(outputId ="data_plot", height = "800px"),
                         ),
                tabPanel("Data Summary",
                         fluidRow(
                         plotOutput("mary"),
                         verbatimTextOutput("at"))),
                tabPanel("Data Summary", DT::dataTableOutput("ysummary"),verbatimTextOutput("total"),
                         plotlyOutput("totalplot")),
                tabPanel("SPI Plot", plotOutput("main_plot", height = "800px")),
                tabPanel("SPI Summary", verbatimTextOutput("summary")),
                tabPanel("PET Plot", plotlyOutput(outputId ="pet_plot", height = "500px")),
                tabPanel("Map", leafletOutput("mymap",height = "800px"))
                
    
    #plotOutput("main_plot", height = "800px")
    
    )
)))