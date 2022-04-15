library(shiny)
require(rmarkdown)
library(knitr)

# Define UI for application 
ui <- fluidPage(

    # Application title
    titlePanel("Source Code"),
      tabsetPanel(type = "pills",
                  tabPanel("World",uiOutput('world')),
                  tabPanel("Country",uiOutput('country')),
                  tabPanel("Generator",uiOutput('generator')),
                  tabPanel("Forecasting",uiOutput('forecast')),
                  tabPanel("Map",uiOutput('map'))
                  )
    
)

# Define server logic 
server <- function(input, output) {
  output$world <- renderUI({
    txt <- markdown::markdownToHTML(knit('world_code.Rmd', quiet = TRUE))
    HTML(txt)
    
  })
  
  output$country <- renderUI({
    txt <- markdown::markdownToHTML(knit('country_code.Rmd', quiet = TRUE))
    HTML(txt)
  })
  
  output$forecast <- renderUI({
    txt <- markdown::markdownToHTML(knit('forecasting_code.Rmd', quiet = TRUE))
    HTML(txt)
  })
  
  output$map <- renderUI({
    txt <- markdown::markdownToHTML(knit('map_code.Rmd', quiet = TRUE))
    HTML(txt)
  })
  
  output$generator <- renderUI({
    txt <- markdown::markdownToHTML(knit('generator_code.Rmd', quiet = TRUE))
    HTML(txt)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
