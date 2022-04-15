---
output:
  html_document: default
---



### Code - Markdown

This is an R Markdown document that provide the code of the "Forecasting" page.

```r
library(shiny)
library(shinyjs)
library(knitr)
library(keras)
library(ggplot2)
library(plotly)
library(reticulate)
library(kableExtra)
library(psych)
# Data Processing
options(scipen=999)
path = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path)
data <- read.csv(file = 'world.csv',
                 sep=',',
                 fileEncoding ='UTF-8-BOM')
row.names(data)<-t(data$Code)
x <- range(1960,2020)

# Time Series Model
data_model <- function(data,time){
  time <- time*2
  io <- as.data.frame(matrix(0,(length(data)-time+1),time))
  for (i in 1:(length(data)-time+1)) {
    io[i,] <- data[i:(i+time-1)]
  }
  return(io)
}


#UI
ui <- fluidPage(
  useShinyjs(),
  br(),
  sidebarLayout(
    sidebarPanel(
      {choices2 = setNames(data$Code,data$Country)
      c2 <- selectInput("country", "Forecasting",  choices2)},
      actionButton("run", "Load Model"), br(),br(),
      sliderInput("year", "Visualize Fit",
                  min=1970, 
                  max=2010, 
                  value=1970,animate = TRUE),
      paste("The model use MAPE as key performance indicator, its because the model predict 10 years directly."),
      br(),br(),
      paste("We choosed validation MAPE with 30:70 validation ratio as model performance that iterated using 20 Epochs."),
      br(),br(),
      paste("The output may generate decimal population forecast. We keep it for precision purposes.")
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Saved Result",{
                    ui <- fluidPage(
                      br(),
                      tabsetPanel(type = "pills",
                                  tabPanel("Forecast Future",
                                           {ui <- fluidPage(
                                             br(),
                                             verbatimTextOutput("saved_text_future"),
                                             plotlyOutput("saved_plot_future")
                                           )})
                                           ,
                                  tabPanel("Saved Future Data",
                                           div(style='width:100%;overflow-x: scroll;height:70vh;overflow-y: scroll;',
                                               tableOutput("future"))
                                  ),
                                  tabPanel("Saved Future MAPE",
                                           div(style='width:100%;overflow-x: scroll;height:70vh;overflow-y: scroll;', 
                                               tableOutput("performance"))
                                  ),
                                  tabPanel("Saved Model Summary",{ui <- fluidPage(
                                    br(),
                                    verbatimTextOutput("model"))})
                      )
                    )
                  }),

                  tabPanel("Model",{ui <- fluidPage(
                    br(),"Click Load Model",br(),br(),
                    verbatimTextOutput("load"),
                    verbatimTextOutput("text"),
                    verbatimTextOutput("summary")
                    )}),
                  
                  tabPanel("Performance Plot",{ui <- fluidPage(
                    br(),
                    verbatimTextOutput("performance_text"),
                    plotlyOutput("plot")
                  )}),
                  tabPanel("Fit Visualization",{ui <- fluidPage(
                    br(),
                    verbatimTextOutput("text_fit"),
                    plotlyOutput("plot_fit")
                  )}),
                  
                  tabPanel("Forecast Future",{ui <- fluidPage(
                    br(),
                    verbatimTextOutput("text_future"),
                    plotlyOutput("plot_future")
                  )})
                  )
    )
  )
)

server <- function(input, output) {
  
  observeEvent(c(input$run, input$button2, input$button3), ignoreInit = TRUE, {
    output$load <- renderPrint({
      print("It may a minutes at first! Loading Tensorflow ...")
    })
  })
  
  observeEvent(input$run, delay(10,{
    model <- load_model_tf(c(paste0("model_generator/",input$country,"/")))
    history <- readRDS(c(paste0("model_generator/",input$country,"/history.Rds")))
    loaded <- input$country
    
    t = array(data[input$country,3:63])
    
    stamp <- 10
    z <- data_model(t,stamp)
    
    x_train <- as.matrix(z[,1:stamp])
    y_train <- as.matrix(z[,(1+stamp):(2*stamp)])
    
    eval_data <- as.matrix(z[nrow(z),(1+stamp):(2*stamp)])
    eval <- model %>% predict(eval_data)
    
    
    output$text <- renderPrint({
      print(paste("Country:",data[loaded,1]))
      print("MAPE in %")
      print(history)
      
    })
    
    output$summary <- renderPrint({
      print("Model Summary")
      summary(model)
    })
    
    output$plot <- renderPlotly({
      x <- as.data.frame(history)
      
      ggplot(data=x,aes(x=epoch,y=value,group=data)) +
        geom_point(aes(color=data))+
        geom_smooth(se=FALSE, formula = y ~ x, method = "loess", aes(color=data), size=0.8) +
        labs(x = "Epoch",
             y = "MAPE (%)",
             color = "MAPE")
    })
    
    output$performance_text <- renderText({
      print(paste0("Model Performance: ", data[loaded,1],"\n",
                   "Final MAPE validation: ",history$metrics$val_loss[20],"%"))
      
    })
    
    
    output$plot_fit <- renderPlotly({
      fit <- input$year-1968
      fit_predict <- as.matrix(z[fit,1:stamp])
      fit_actual <- as.matrix(z[fit,(1+stamp):(2*stamp)])
      fit_eval <- model %>% predict(fit_predict)
      fit_performance <- model %>% 
        evaluate(fit_eval, fit_actual, verbose = 0)
      
      output$text_fit <- renderText({
        paste0("Fit Performance: ", data[loaded,1],"\n","Fit MAPE: ",fit_performance,"%")
        
      })
      
      f_actual <- as.data.frame(t(fit_actual))
      colnames(f_actual)<-c("population")
      
      f_eval <- as.data.frame(t(fit_eval))
      colnames(f_eval)<-c("population")
      
      colors <- c("black", "red","dark grey")
      names(colors)<- c("Actual","Forecast","Smooth")
      
      f_year <- c((input$year+1):(input$year+10))
      
      
      ggplot(data=f_eval,aes(x=f_year,y=population)) +
        geom_smooth(formula = y ~ x, method = "loess", aes(color="Smooth")) +
        geom_line(data=f_eval, aes(x=f_year,group=1, color="Forecast"),size=0.8) +
        geom_line(data=f_actual, aes(x=f_year,group=1, color="Actual"),size=0.8) +
        labs(x = "Year",
             y = "Country Population",
             color = "Legend") +
        scale_color_manual(values = colors)+
        scale_x_discrete(limits=f_year)
      
    })
    
    
    output$text_future <- renderText({
      paste0(data[loaded,1],"'s Future Population Forecast in 2021 - 2030 \n",
            "Model Performance (100% - Final MAPE Validation): ",100-history$metrics$val_loss[20],"%")
      
    })
    
    output$plot_future <- renderPlotly({
      
      t_data <- as.data.frame(t(t))
      colnames(t_data)<-c("population")
      
      p_data<- as.data.frame(rbind(t(t),t(eval)))
      colnames(p_data)<-c("population")
      
      colors <- c("black", "red","dark grey")
      names(colors)<- c("Actual","Forecast","Smooth")
      
      
      ggplot(data=p_data,aes(x=c(1960:2030),y=population)) +
        geom_smooth(formula = y ~ x, method = "loess", aes(color="Smooth")) +
        geom_line(data=p_data, aes(x=c(1960:2030),group=1, color="Forecast"),size=0.8) +
        geom_line(data=t_data, aes(x=c(1960:2020),group=1, color="Actual"),size=0.8) +
        labs(x = "Year",
             y = "Country Population",
             color = "Legend") +
        scale_color_manual(values = colors) 
      
  
    })
  }))
  
  output$future <- renderText({
    forecast_data <- as.data.frame(readRDS("forecast_data.Rds"))
    rownames(forecast_data) <- c(data[,1])
    colnames(forecast_data) <- c(2021:2030)
    kable_styling(kable(forecast_data))
  })
  
  output$performance <- renderText({
    history_data <- as.data.frame(readRDS("history_data.Rds"))
    rownames(history_data) <- c(data[,1])
    colnames(history_data) <- c("Loss MAPE (%)","Validation MAPE (%)")
    kable_styling(kable(history_data))
  })
  
  output$model <- renderPrint({
    sum_data = as.data.frame(readRDS("history_data.Rds"))
    colnames(sum_data) <- c("Loss MAPE (%)","Validation MAPE (%)")
    ds = describe(sum_data,fast=TRUE)
    ds = ds[,c(2:7)]
    ds
  })
  
  
  output$saved_text_future <- renderText({
    sum_data = as.data.frame(readRDS("history_data.Rds"))
    rownames(sum_data) <- c(data[,2])
    
    paste0(data[c(input$country),1],"'s Future Population Forecast in 2021 - 2030 \n",
           "Model Performance (100% - Final MAPE Validation): ",100-sum_data[c(input$country),2],"%")
    
  })
  
  
  output$saved_plot_future <- renderPlotly({
    
    raw_data <- data[,3:63]
    colnames(raw_data) <- c(1960:2020)
    forecast_data <- as.data.frame(readRDS("forecast_data.Rds"))
    colnames(forecast_data) <- c(2021:2030)
    
    data_saved <- cbind(raw_data,forecast_data)
    
    rownames(data_saved) <- c(data[,2])
    data_saved <- as.data.frame(t(data_saved))
    
    
    
    t_data <- as.data.frame(data_saved[1:61,c(input$country)])
    colnames(t_data)<-c("population")
    
    p_data<- as.data.frame(data_saved[,c(input$country)])
    colnames(p_data)<-c("population")
    
    colors <- c("black", "red","dark grey")
    names(colors)<- c("Actual","Forecast","Smooth")
    
    
    ggplot(data=p_data,aes(x=c(1960:2030),y=population)) +
      geom_smooth(formula = y ~ x, method = "loess", aes(color="Smooth")) +
      geom_line(data=p_data, aes(x=c(1960:2030),group=1, color="Forecast"),size=0.8) +
      geom_line(data=t_data, aes(x=c(1960:2020),group=1, color="Actual"),size=0.8) +
      labs(x = "Year",
           y = "Country Population",
           color = "Legend") +
      scale_color_manual(values = colors) 
    
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
```
