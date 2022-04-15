---
output:
  html_document: default
---



### Code - Markdown

This is an R Markdown document that provides the code of the forecasting generator program to generate a model and forecast for each country. This result of the program will be opened and visualized in this shiny app.

```r
library(keras)
# Data Processing
options(scipen=999)
path = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path)
data <- read.csv(file = 'world.csv',
                 sep=',',
                 fileEncoding ='UTF-8-BOM')
x <- range(1960,2020)

# Forecast Model
data_model <- function(data,time){
  time <- time*2
  io <- as.data.frame(matrix(0,(length(data)-time+1),time))
  for (i in 1:(length(data)-time+1)) {
    io[i,] <- data[i:(i+time-1)]
  }
  return(io)
}

stamp <- 10

# Model A
a <- keras_model_sequential() 
a %>% 
  layer_dense(units = 10, input_shape = c(stamp)) %>%
  layer_dropout(rate = 0) 
  layer_dense(units = 8) %>% 
  layer_dropout(rate = 0.1)

# Model B
b <- keras_model_sequential() 
b %>% 
  layer_dense(units = 10, input_shape = c(stamp)) %>%
  layer_dropout(rate = 0.1) 
  layer_dense(units = 8) %>% 
  layer_dropout(rate = 0.1)

# Training
n_country <- c(nrow(data))
forecast_data <- matrix(0,ncol=10,nrow=n_country)
history_data <- matrix(0,ncol=2,nrow=n_country)

for (i in c(1:n_country)) {
  print(paste("Iteration ",i))
  t = array(data[i,3:63])
  z <- data_model(t,stamp)
  x_train <- as.matrix(z[,1:stamp])
  y_train <- as.matrix(z[,(1+stamp):(2*stamp)])
  model_a <- clone_model(a)
  model_b <- clone_model(b)
  
  model_a %>% compile(
    loss = 'mape',
    optimizer = 'adam'
  )
  
  model_b %>% compile(
    loss = 'mape',
    optimizer = 'adam'
  )
  
  history_a <- model_a %>% fit(
    x_train, y_train, 
    epochs = 20, batch_size = 1, 
    validation_split = 0.3,
    verbose = 0,
    use_multiprocessing=True
  )
  
  if (history_a$metrics$val_loss[20] > 20){
    history_b <- model_b %>% fit(
      x_train, y_train, 
      epochs = 20, batch_size = 1, 
      validation_split = 0.3,
      verbose = 0,
      use_multiprocessing=True
    )
    if (history_a$metrics$val_loss[20] > history_b$metrics$val_loss[20])
    {
      eval_data <- as.matrix(z[42,(1+stamp):(2*stamp)])
      eval <- model_b %>% predict(eval_data)
      history <- c(history_b$metrics$loss[20],history_b$metrics$val_loss[20])
      save_model_tf(model_b, c(data[i,2]))
      saveRDS(history_b, file = paste0(c(data[i,2]),"/","history.Rds"))
      
    }
    else 
    {
      eval_data <- as.matrix(z[42,(1+stamp):(2*stamp)])
      eval <- model_a %>% predict(eval_data)
      history <- c(history_a$metrics$loss[20],history_a$metrics$val_loss[20])
      save_model_tf(model_a, c(data[i,2]))
      saveRDS(history_a, file = paste0(c(data[i,2]),"/","history.Rds"))

    }
  } else {
    eval_data <- as.matrix(z[42,(1+stamp):(2*stamp)])
    eval <- model_a %>% predict(eval_data)
    history <- c(history_a$metrics$loss[20],history_a$metrics$val_loss[20])
    save_model_tf(model_a, c(data[i,2]))
    saveRDS(history_a, file = paste0(c(data[i,2]),"/","history.Rds"))

  }
  
  forecast_data[i,] <- c(eval)
  history_data[i,] <- history
  
}
saveRDS(forecast_data, file = "forecast_data.Rds")
saveRDS(history_data, file = "history_data.Rds")
```
