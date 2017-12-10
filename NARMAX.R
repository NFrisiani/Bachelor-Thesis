data <- read.csv("/Users/Nic/Downloads/coindesk-bpi-USD-close_data-2017-06-01_2017-06-21.csv")

trainingdata <- data[1:400,2]
testdata <- data[401:504,2]

predictions <- matrix(0,1,104)
acc = integer(103)

for (i in 1:104)
{
  narxBTC <- nnetTs(trainingdata, m=5, size = 10, steps = 1)
  predictions[i] <- predict(narxBTC)
  trainingdata <- c(trainingdata, testdata[i]) 
  
  if(i >= 2)
  {
    if((predictions[i] > testdata[i-1] && testdata[i] > testdata[i-1]) || (predictions[i] < testdata[i-1] && testdata[i] < testdata[i-1]))
    {
      acc[i-1] = 1  
    }
  }
}

x1 <- seq(401,504,1)
plot(trainingdata[1:400], ylim=range(trainingdata), xlim=range(1,504), xlab = "Hours since 01/06/2017", ylab = "BTC to USD",
     type = "l", col = "blue", main = "NARMAX")
par(new = TRUE)
plot(x = x1, y = testdata, ylim=range(trainingdata), xlim=range(1,504), axes = FALSE,
     xlab = "", ylab = "", col = "black", type = "l")
par(new = TRUE)
plot(x = x1, y = predictions, ylim=range(trainingdata), xlim=range(1,504),
     axes = FALSE, xlab = "", ylab = "", type = "l", col = "red")
par(new = TRUE)
points(1:length(trainingdata),fitted(narxBTC),type="l",col="green")

sum(acc)/103
