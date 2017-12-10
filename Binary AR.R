data <- read.csv("/Users/Nic/Downloads/coindesk-bpi-USD-close_data-2017-06-01_2017-06-21.csv")

data_train <- data[1:100,2]
data_test <- data[101:501,2]

acc <- 0
for (j in 1:20)
{
  data_train <- data[1:100,2]
  c_count <- 0
  
  for (i in 1:401)
  {
    btc_ar <- ar.yw(data_train, order.max = j, aic = FALSE)
    btc_pred <- predict(object = btc_ar, n.ahead = 1)
    
    if(i >= 2)
    {
      if(((btc_pred$pred[1] > data_test[i-1]) && (data_test[i] > data_test[i-1])) || ((btc_pred$pred[1] < data_test[i-1]) && (data_test[i] < data_test[i-1])))
      {
        c_count = c_count+1
      }
    }
    
    data_train <- c(data_train, data_test[i]) 
  }
  acc[j] <- c_count/400.00  
}

acc
mean(acc)