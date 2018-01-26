# Load (install) packages ----
library(devtools)
# Necessary to run this
#   devtools::install_github("jcheng5/googleCharts")
#if it crashes on linux do:
# sudo apt-get install libcurl4-openssl-dev
# sudo apt-get install libssl-dev and try again

library(googleCharts)

# Necessary libraries
library(shiny)
library(maps)
library(mapproj)
library(dplyr)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(countrycode)
library(rworldmap)

# If not all plots shown, open the application in browser

# Load data ----
data <- read.table("europe.csv", sep=",", header=TRUE)
colnames(data)[5] <- "Life expectancy"
colnames(data)[7] <- "Population growth"
defaultColors <- c( "red", "blue", "orange", "magenta", "cyan", "gold", "black", "darkgreen")

# User interface ----
ui <- navbarPage(title="Shiny App", fluid = TRUE,
    tabPanel("Bubble chart",
             
             # This line loads the Google Charts JS library
             googleChartsInit(),
             
             # Use the Google webfont "Source Sans Pro"
             tags$link(
               href=paste0("http://fonts.googleapis.com/css?",
                           "family=Source+Sans+Pro:300,600,300italic"),
               rel="stylesheet", type="text/css"),
             tags$style(type="text/css",
                        "body {font-family: 'Source Sans Pro'}"
             ),
             h2("Bubble Chart for Europe dataset", align = "center"),
             # Define the sidebar with one input
             sidebarPanel(width = 4,
                          selectInput("variableX", "X Axis:", 
                                      selected = "Unemployment", choices = c("GDP","Inflation","Life expectancy","Military","Population growth","Unemployment"))
             ),
             # Define the sidebar with one input
             sidebarPanel(width = 4,
                          selectInput("variableY", "Y Axis:", 
                                      selected = "Life expectancy", choices = c("GDP","Inflation","Life expectancy","Military","Population growth","Unemployment"))             ),
             # Define the sidebar with one input
             sidebarPanel(width = 4,
                          numericInput('variableC', 'Cluster count', 3,
                                       min = 1, max = 8)),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             splitLayout(cellArgs = list(style = "padding: 0px"), cellWidths = c("50%", "50%"), 
                         googleBubbleChart("chart",
                                           width="95%", height = "450px",
                                           options = list(
                                             fontName = "Source Sans Pro",
                                             fontSize = 13,
                                             # The default padding is a little too spaced out
                                             chartArea = list(
                                               top = 35, left = 50,
                                               height = "80%", width = "100%"
                                             ),
                                             # Allow pan/zoom
                                             explorer = list(),
                                             # Set bubble visual properties
                                             bubble = list(
                                               opacity = 0.8, stroke = "none", 
                                               # Show label
                                               textStyle = list(
                                                 color = "black"
                                               )
                                             ),
                                             # Set fonts
                                             titleTextStyle = list(
                                               fontSize = 16
                                             ),
                                             tooltip = list(
                                               textStyle = list(
                                                 fontSize = 12
                                               )
                                             )
                                           )
                         ),
                         plotOutput(width="99%", height="500px", outputId = "countriesGraph")
                         
             )
            
    ),
    tabPanel("Bar plots", 
             h2("Bar plots", align = "center"),
             br(),
             splitLayout(cellArgs = list(style = "padding: 0px"), cellWidths = c("50%", "50%"), 
 
                         sidebarPanel(width=12,
                           selectInput("selectedCountry", "Country", choices = data$Country
                           ),
                           plotOutput(width="99%", outputId = "summaryCountryGraph")
                         ),
                         sidebarPanel(width=12,
                                      selectInput("selectedCountry2", "Country", choices = data$Country, selected = "Germany"
                                      ),
                                      plotOutput(width="99%", outputId = "summaryCountryGraph2")
                         )

             ),
             h4("Green bar suggest a positive result comparing to the mean, red - negative while blue is not comparable", align="center")
             
             
             
    ),
    tabPanel("Data table", tabPanel("table", DT::dataTableOutput("mytable3"))
    )

  )


# Server logic ----
server <- function(input, output, session) {
  #### bubble
  selectedData1 <- reactive({
    data[, c(input$variableX, input$variableY)]
  })
  
  clusters1 <- reactive({
    kmeans(selectedData1(), input$variableC)
  })
  
  data2 <- reactive({
    cbind(data[,1:8], Cluster=as.character(clusters1()$cluster))
  })
  
  series <- reactive({
    structure(
    lapply(defaultColors[1:input$variableC], function(color) { list(color=color) }),
    names = levels(data2()$Cluster)
  )
  })

  Data <- reactive({

    df <- data2() %>%
      select(Country, input$variableX, input$variableY,
             Cluster, Area) %>%
      arrange(Cluster)
  })
  
  output$chart <- reactive({
    # Return the data and options
    list(
      data = googleDataTable(Data()),
      options = list(
        hAxis = list(
          title = sprintf("X = %s",input$variableX)
        ),
        vAxis = list(
          title = sprintf("Y = %s",input$variableY)
        ),
        title = sprintf(
          "Coloring based on clusters"),
        series=series()
      )
    )
  })
  #### map
  output$countriesGraph <- renderPlot({
    theCountryCode <- countrycode(data$Country, 'country.name', 'iso3c')
    # ISO3 names of the countries to plot in cluster colors
    
    malDF <- data.frame(country = theCountryCode,
                        Europe = data2()$Cluster)
    
    malMap <- joinCountryData2Map(malDF, joinCode = "ISO3",
                                  nameJoinColumn = "country")
    # This will join the malDF data.frame to the country map data
    myPalette <- defaultColors[1:input$variableC]
    mapCountryData(malMap, nameColumnToPlot="Europe", catMethod = "categorical",
                   missingCountryCol = gray(0.7), xlim = c(-20,34),colourPalette = myPalette, ylim = c(37,67), addLegend = FALSE)
    # palette as for clusters
  })
  #### bar1
  output$summaryCountryGraph <- renderPlot({
    selectedRow <- which(data$Country == input$selectedCountry)
    displayData <- data[,1:8]
    displayData$Country <- 'Mean'
    displayData[selectedRow, "Country"] <- input$selectedCountry
    getMeanIntoOne <- function(x)
    {
      if(is.numeric(x))
      {
        return (x / mean(x))
      }
      else
        return(x)
    }
    getColor <- function(x){
      vec <- c(2,1,-1,1,1,1,-1)
      for(i in 1: length(vec))
      {
        if(vec[i] != 2)
        {
          if(x[,i] < 1)
          {
            vec[i] <- vec[i]*(-1)
          }  
        }
      }
      vec <- as.character(vec)
    }
    
    res <- as.data.frame(apply(displayData[,-1],2, getMeanIntoOne))
    displayData <- cbind(displayData$Country, res)
    names(displayData)[1] <- "Country"
    displayData[selectedRow,] %>%  
      gather(Characteristics, Proportion_Of_Mean, -Country) %>%
      ggplot(., aes(x=Characteristics, y=Proportion_Of_Mean*100, fill=getColor(displayData[selectedRow,-1])))+
      geom_bar(stat='identity', position='dodge', colour="black")+
      labs(y = "% of european mean")+
      labs(x = "Chosen characteristics") +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 15))+
      guides(fill=FALSE)
    
  })
  #### bar2
  output$summaryCountryGraph2 <- renderPlot({
    selectedRow <- which(data$Country == input$selectedCountry2)
    displayData <- data[,1:8]
    displayData$Country <- 'Mean'
    displayData[selectedRow, "Country"] <- input$selectedCountry2
    getMeanIntoOne <- function(x)
    {
      if(is.numeric(x))
      {
        return (x / mean(x))
      }
      else
        return(x)
    }
    getColor <- function(x){
      vec <- c(2,1,-1,1,1,1,-1)
      for(i in 1: length(vec))
      {
        if(vec[i] != 2)
        {
          if(x[,i] < 1)
          {
            vec[i] <- vec[i]*(-1)
          }  
        }
      }
      vec <- as.character(vec)
    }
    
    res <- as.data.frame(apply(displayData[,-1],2, getMeanIntoOne))
    displayData <- cbind(displayData$Country, res)
    names(displayData)[1] <- "Country"
    displayData[selectedRow,] %>%  
      gather(Characteristics, Proportion_Of_Mean, -Country) %>%
      ggplot(., aes(x=Characteristics, y=Proportion_Of_Mean*100, fill=getColor(displayData[selectedRow,-1])))+
      geom_bar(stat='identity', position='dodge', colour="black")+
      labs(y = "% of european mean")+
      labs(x = "Chosen characteristics") +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 15))+
      guides(fill=FALSE)
    
  })
  #### 
  output$mytable3 <- DT::renderDataTable({
    DT::datatable(data, options = list(pageLength = 14))
  })

}

# Run app ----
shinyApp(ui, server)
