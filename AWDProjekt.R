library(ggplot2)
library(shiny)
library(dplyr)

AC5 <- read.csv("data/AC5.csv",sep = ";" )
AC5w <- AC5[, c(1,2,3,4,5,6,7,8,9,10,11,12)]
models <- as.data.frame( AC5w[, c(7)])
colnames(models)[1] <- "Model"
models2<-models %>% count(Model)
models2<-arrange(models2,desc(n))
sortedbydeaths <- as.data.frame(arrange(AC5w, desc(Fatalities)))
sort3 <- select(sortedbydeaths, c(1,2,3,4,5,6,7,11,12))

AC5w$Date <- format(as.Date(AC5$Date, format="%m/%d/%Y"),"%Y")

da <- AC5w[, c(1,11)]
da$Fatalities <- as.numeric(da$Fatalities)
da<-aggregate(Fatalities~Date,da,FUN = sum)
da<-da[-c(1),]

ui <- fluidPage(
  
  titlePanel("Airplane Crashes"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      numericInput(inputId = "obs",
                   label = "Number of most tragical accidents:",
                   value = 10),
      numericInput(inputId = "model",
                   label = "Number of most popular models:",
                   value = 8)
    ,
    br(),
    
    sliderInput("yearsin",
                "Fatalities in years:",
                min = 1912,
                max = 2009,
                value = c(1912,1939))
    
  ),
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Data", dataTableOutput("data")),
                  tabPanel("Most Tragical", tableOutput("mt")),
                  tabPanel("Years", plotOutput("yearsout")),
                  tabPanel("Models by accidents", tableOutput("models")),
                  tabPanel("Map", imageOutput("map"))
      )
      
    )
  )
  )

server <- function(input, output) {
  
  datasetInput <- reactive({
    l1 <- (min(input$yearsin) - 1912)
    l2 <- (max(input$yearsin) - 1912)
    top <- da[l1:l2,]
    top
  })
  
  output$data <- renderDataTable(
    AC5w
  )
  
  output$models <- renderTable({
    head(models2,n = input$model)}
  )
  
  output$mt <- renderTable({
    head(sort3, n = input$obs)
  })
  
  output$map <- renderImage({
    list(src = "data/katastrofy.png",
         contentType = 'image/png',
         width = 600,
         height = 500)
  },deleteFile = FALSE)
  
  output$yearsout <- renderPlot({
    
    ggplot(datasetInput(), aes(x=datasetInput()$Date, y=datasetInput()$Fatalities) ) + 
           geom_bar(stat="identity", width=.5, fill="darkblue") + 
           labs(title="Fatalities in years", x="Years", y = "Amount of fatalities") + 
           theme(axis.text.x = element_text(angle=65, vjust=0.5))
  },height = 600)
  
}

shinyApp(ui = ui, server = server)

  