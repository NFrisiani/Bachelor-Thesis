data <- read.csv("/Users/Nic/Downloads/coindesk-bpi-USD-close_data-2017-06-01_2017-06-21.csv")

data_train <- data[1:100,2]
data_test <- data[101:501,2]

acc <- matrix(0,2,20)

for (j in 1:20)
{
  data_train <- data[1:100,2]
  c_count <- 0
  
  p <- round(runif(1, 1, 6))
  d <- 0
  q <- round(runif(1, 1, 6))
  
  for (i in 1:401)
  {
    btc_arima <- arima(data_train, order = c(p,d,q), method = "CSS")
    btc_pred <- predict(btc_arima, 1)
    
    if(i >= 2)
    {
      if(((btc_pred$pred[1] > data_test[i-1]) && (data_test[i] > data_test[i-1])) || ((btc_pred$pred[1] < data_test[i-1]) && (data_test[i] < data_test[i-1])))
      {
        c_count = c_count+1
      }
    }
    
    data_train <- c(data_train, data_test[i]) 
  }
  acc[1,j] <- p*100+d*10+q
  acc[2,j] <- c_count/400.00  
}

acc
mean(acc[2,])