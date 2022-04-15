---
output:
  html_document: default
---



### Code - Markdown

This is an R Markdown document that provide the code of the "Map Visualization" page.

```r
library(shiny)
library(ggplot2)
library(plotly)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
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
row.names(data)<-t(data$Code)

earth <- ne_countries(scale = "small", returnclass = "sf", type = "countries")
class(earth)

w_data <- earth[,c("iso_a3","geometry")]
colnames(w_data) <- c("Code","geometry")


# Marge Data

merged <- merge(data, w_data, by = "Code")


ui <- fluidPage(
  br(),
  sliderInput("year", "Year",
              min=1960, 
              max=2030, 
              value=1960,
              step=1, animate=animationOptions(interval = 6000),width="100%"),
  
  div(style='width:100%;overflow-x: scroll;height:80vh;overflow-y: scroll;', 
     plotlyOutput("map",width="100%"))
  )

server <- function(input, output) {
  
  output$map <- renderPlotly({
    
    map_data <- as.data.frame(merged[,c(74,2,input$year-1957,input$year-1957)])
    colnames(map_data) <- c("geometry","Country","Maping","Population")
    
    q <- ggplot() +
      geom_sf(data=w_data, color = "grey")+
      ggtitle("World map")+
      
      scale_fill_viridis_c(option = "viridis", trans = "log",
                           limits=c(1e+3,1e+10),
                           breaks =c(1e+3,1e+5,1e+7,1e+9),
                           aesthetics ="fill" )+
      geom_sf(data = map_data,aes(geometry=geometry, group=Country,
                                  text = paste("Population:", Population),
                                  fill=Maping), color="black")+
      xlab("Longitude") + ylab("Latitude") 
    ggplotly(q) %>% layout(
      autosize = F, height = 450, width = 950, margin = list(
        l = 0,
        r = 0,
        b = 0,
        t = 0,
        pad = 0
      )
    )
  })
}

shinyApp(ui = ui, server = server)
```
