library(shiny)
library(knitr)
library(kableExtra)
library(ggplot2)
library(plotly)
library(scales)
library(psych) 
require(rmarkdown)
library(dplyr)
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

x <- c(1960:2030)

# Define UI for application 
ui <- fluidPage(
  # Application title
  br(),
  tabsetPanel(type = "tabs",
              tabPanel("Interactive Compare",
                       {
                         ui <- fillPage(
                           br(),
                           sidebarLayout(
                             sidebarPanel(
                               paste("Reload if the chart not show correctly!"),
                               sliderInput("year", "Year",
                                           min=min(x), 
                                           max=max(x), 
                                           value=c(1960,1980),
                                           step=1, animate=TRUE
                               ),
                               {choices = setNames(data$Country,data$Country)
                               c1 <- selectInput("country1", "Country 1",  choices, selected="Virgin Islands (U.S.)")},
                               {choices2 = setNames(c("Disabled",data$Country),c("Disabled",data$Country))
                               c2 <- selectInput("country2", "Country 2",  choices2, selected="Micronesia, Fed. Sts.")},
                               c3 <- selectInput("country3", "Country 3",  choices2)
                             ),
                             
                             # Output
                             mainPanel(
                               div(style='width:100%;overflow-x: scroll;height:80vh;overflow-y: scroll;',
                                    plotlyOutput("plot"))
                             )
                           )
                         )
                       }),
              tabPanel("Filter Data",{
                
                ui <- fillPage(
                  br(),
                  sidebarLayout(
                      sidebarPanel(
                        {f_choices = setNames(data$Country,data$Country)
                        f1 <- selectInput("filter1", "Country 1",  f_choices, selected="United States")},
                        f2 <- selectInput("filter2", "Country 2",  f_choices, selected="Russian Federation"),
                        f3 <- selectInput("filter3", "Country 3",  f_choices, selected="Ukraine")),
                      mainPanel(
                        div(style='width:100%;overflow-x: scroll;height:80vh;overflow-y: scroll;', 
                            tableOutput("filter")))
                      ))
                }),
               
              tabPanel("Rank",{
                ui <- fillPage(sliderInput("rank", "Year",
                                           min=min(x), 
                                           max=max(x), 
                                           value=1960,
                                           step=1, animate=animationOptions(interval = 1400),width="100%"),
                               div(style='width:100%;overflow-x: scroll;height:70vh;overflow-y: scroll;',
                                   plotOutput("rank"))
                              
                )})
              ) 
    )

# Define server logic 
server <- function(input, output) {
  output$plot <- renderPlotly({
    year <- c(input$year[1]:input$year[2])
    index <- (input$year[1]-1957):(input$year[2]-1957)
    t_data <- as.data.frame(t(data[,index]))
    colnames(t_data)<-c(data$Country)
    
    
    x = t_data[input$country1]
    x_data = as.data.frame(c(x))
    
    
    
    country_x = x_data
    format(country_x, scientific = FALSE)
    my_x <-data.frame(year,
                      country_x)
    colnames(my_x)<- c("year","country")
    my_x$country <- as.numeric(my_x$country)
    
    
    
    q <-ggplot(data=my_x,aes(y=country)) + 
      geom_line(data=my_x, aes(x=year, group=1, color=input$country1),size=0.8)
      
    
    if (input$country2 != "Disabled"){
      y = t_data[input$country2]
      y_data = as.data.frame(c(y))
      
      country_y = y_data
      format(country_y, scientific = FALSE)
      my_y <-data.frame(year,
                        country_y)
      colnames(my_y)<- c("year","country")
      my_y$country <- as.numeric(my_y$country)
      
      q <- q + geom_line(data=my_y, aes(x=year, group=1, color=input$country2),size=0.8)
    }
    
    if (input$country3 != "Disabled"){
      z = t_data[input$country3]
      z_data = as.data.frame(c(z))
      
      country_z = z_data
      format(country_z, scientific = FALSE)
      my_z <-data.frame(year,
                        country_z)
      colnames(my_z)<- c("year","country")
      my_z$country <- as.numeric(my_z$country)
      
      q <- q + geom_line(data=my_z, aes(x=year, group=1, color=input$country3),size=0.8)
      
    }
    
    q <- q + 
      scale_x_continuous(breaks= pretty_breaks())+
      scale_color_brewer(palette="Paired")+
      labs(x = "Year",
           y = "Country Population",
           color = "Legend")
    
    
    if ((input$year[2]) > 2020){
      q <- q + geom_vline(aes(xintercept = as.numeric(2020.5)),
                          linetype="dotted", 
                          color = "red")}
    else {
      q
    }
   
     ggplotly(q) %>% 
       layout(legend = list(orientation = "h", x = 0, y = -0.2))
    
  })
  
  output$rank <- renderPlot({
    rank_data <- data[1:nrow(data)-1,input$rank-1957]
    rank_country <- data[1:nrow(data)-1,1]
    rank_df <-data.frame(rank_country,rank_data)
    rank_or <-rank_df[order(rank_data),]
    rank <-data.frame(rank_or$rank_country,rank_or$rank_data)
    colnames(rank) <- c("rank_country","rank_data")
    rank$rank_data <- as.numeric(rank$rank_data)
    ggplot(rank,aes(y=rank_data)) + geom_col(aes(x=rank_country,fill=rank_country))+
      geom_text(aes(x=rank_country,label=rank_data,hjust=-0.1),color="black")+
      scale_x_discrete(limits=rank$rank_country)+
      coord_flip(ylim=c(0, 2000000000))+
      ylab("Population") +
      xlab("Country")+
      theme(legend.position = "none") 
    },height=6000)
  
  output$filter <- renderText ({
    dat = as.data.frame(t(data[,3:length(data)]))
    colnames(dat) <- c(data[,1])
    
    data_table = dat[,c(input$filter1,input$filter2,input$filter3)]
    library(knitr)
    kable_styling(kable(data_table))
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
