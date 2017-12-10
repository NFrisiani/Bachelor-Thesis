data <- read.csv("/Users/Nic/Downloads/coindesk-bpi-USD-close_data-2017-06-01_2017-06-21.csv")

data_train <- data[1:300,2]
data_test <- data[301:501,2]
acc = 0

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

data_train <- normalize(data_train)
data_test <- normalize(data_test)

w <- runif(10, -0.2, 0.2)
b = 0
alpha = 0.1
y = integer(8)
right_value = 0

for(i in 1:290)
{
  trainingData <- data_train[i:(i+9)]
  
  if(data_train[i+10] > data_train[i+9])
  {
    right_value = 1
  }else
  {
    right_value = 0
  }
  
  if((trainingData%*%w + b) > 0)
  {
    y[i] = 1
  }else
  {
    y[i] = 0
  }
  
  w = w + alpha*(right_value - y[i])*right_value
  b = b + alpha*(right_value - y[i])
}


#prediction
real_result = integer(200)
result = integer(200)

for(i in 2:200)
{
  if(data_test[i] > data_test[i-1])
  {
    real_result[i] = 1
  }else
  {
    real_result[i] = 0
  }
}
  
for(i in 2:191)
{
  testData <- data_test[i:(i+9)]
  
  if((testData%*%w + b) > 0)
  {
    result[i] = 1
  }else
  {
    result[i] = 0
  }
  
  if(result[i] == real_result[i])
  {
    acc = acc+1
  }
}

plot(result, col = "green")
par(new = TRUE)
plot(real_result, col = "red")

result
real_result
acc/200