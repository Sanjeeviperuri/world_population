library(shiny)
library(knitr)
library(kableExtra)
library(ggplot2)
library(plotly)
library(scales)
library(psych) 
require(rmarkdown)
library(reshape)
options(scipen=999)
path = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path)
raw_data <- read.csv(file = 'world.csv',
                     sep=',',
                     fileEncoding ='UTF-8-BOM')
colnames(raw_data) <- c("Country","Code",1960:2020)

forecast_data <- as.data.frame(readRDS("model_generator/forecast_data.Rds"))
colnames(forecast_data) <- c(2021:2030)

data <- cbind(raw_data,forecast_data)

# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("World Population"),

    # Input 
    sidebarLayout(
        sidebarPanel(
          sliderInput("year", "Year",
                      min=1960,
                      max=2030, 
                      value=c(1960,1970),
                      step=1, animate=TRUE
          ),
          selectInput("box_population",
                      "Box Plot Type", 
                      choices = list("Logarithmic" = 0,
                                     "Linear" = 1),
                      selected = 0),
          selectInput("population", 
                      "Population Label", 
                      choices = list("Disabled" = 0, 
                                     "Enebled" = 1),
                      selected = 0),
        ),

        # Output
        mainPanel(
          tabsetPanel(type = "tabs",
                      tabPanel("Box Plot",plotlyOutput("box")),
                      tabPanel("Population",plotOutput("plot")),
                      tabPanel("Summary",{ui <- fluidPage(
                        br(),
                        verbatimTextOutput("summary"))}),
                      tabPanel("World Data", div(style='width:100%;overflow-x: scroll;height:70vh;overflow-y: scroll;', tableOutput("table"))))
        )
    )
)

# Define server logic
server <- function(input, output) {

    output$plot <- renderPlot({
      world = t(data[nrow(data),
                     ((input$year[1]-1957):(input$year[2]-1957))])
      year = c(input$year[1]:input$year[2])
      mydef <-data.frame(year,
                         world)
      colnames(mydef)<- c("Year",
                          "World")
      mydef$World <- as.numeric(mydef$World)
      q <- ggplot(data=mydef,
                  aes(x=Year, 
                      y=World,
                      group=1)) +
        geom_line(size=1)+
        geom_point(size=1.8)+
        scale_x_continuous(breaks= pretty_breaks())
      if(input$population == 1){
        q <- q + geom_text(aes(label=World),
                      hjust=-0.1, 
                      vjust=1) +
          geom_text(aes(label=Year),
                    hjust=1.2,
                    vjust=-0.3) +
          ggtitle(paste("Total World Population"))
      } else
      {
        q <- q + ggtitle(paste("Total World Population"))
      }
      
      if ((input$year[2]) > 2020){
        q + geom_vline(xintercept = 2020.5, linetype="dotted", 
                       color = "red", size=1) 
      } else {
        q
      }
      
       
    })
    
    output$box <- renderPlotly({
      box_data = data[1:nrow(data)-1,
                      (c((input$year[1]-1957):(input$year[2]-1957)))]
      year = c(input$year[1]:input$year[2])
      if (input$year[1]==input$year[2]){
        meltData <- melt(box_data)
        p <- ggplot(meltData, 
                    aes(factor(year), 
                        value)) + 
          geom_boxplot(alpha=0.7) 
      } else {
        colnames(box_data)<- year
        meltData <- melt(box_data)
        p <- ggplot(meltData,
                    aes(factor(variable),
                        value)) +
          geom_boxplot(alpha=0.7)
      }
      
      
      if(input$box_population == 0){
        p <- p +
          scale_y_log10(breaks=c(1e+3,1e+4,
                                 1e+5,1e+6,1e+7,
                                 1e+8,1e+9,1e+10),
                        limit=c(1e+3,1e+10)) +
          xlab("Year") +
          ylab("Population") +
          ggtitle(paste(nrow(data)-1,
                        "Countries"))+
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
      } else
      {
        p <- p +
          xlab("Year") +
          ylab("Population") +
          ggtitle(paste("Box Plot of",
                        nrow(data)-1,
                        "Countries"))+
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
      }
      
      p

    })
    
    # Generate a summary
    output$summary <- renderPrint({
      sum_data = data[1:nrow(data)-1,
                      (c((input$year[1]-1957):(input$year[2]-1957)))]
      ds = describe(sum_data,fast=TRUE)
      ds = ds[,c(2:7)]
      ds
    })
    
    htmlOutput("tableset") 
    
    # server component
    output$table <- renderText({
      data_table = data [,c(1,
                            (input$year[1]-1957):(input$year[2]-1957))]
      kable_styling(kable(data_table))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
