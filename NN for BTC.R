data <- read.csv("/Users/Nic/Downloads/coindesk-bpi-USD-close_data-2017-06-01_2017-06-21.csv")

trainingdata <- data[1:400,2]
testdata <- data[401:504,2]

nnBTC <- nnetar(trainingdata)
nnBTC_pred <- forecast(nnBTC,h=104)

x1 <- seq(401,504,1)
plot(nnBTC_pred, ylim=range(trainingdata), xlim=range(1,504), axes = FALSE, xlab = "", 
     ylab = "", type = "l", col = "blue", main = "Simple ANN optimised for Time Series")
par(new = TRUE)
plot(trainingdata, ylim=range(trainingdata), xlim=range(1,504),
     xlab = "Hours since 01/06/2017", ylab = "BTC to USD", col = "black", type = "l")
par(new = TRUE)
plot(x = x1, y = testdata, ylim=range(trainingdata), xlim=range(1,504),
     axes = FALSE, xlab = "", ylab = "", type = "l", col = "red")
par(new = TRUE)
points(1:length(trainingdata),fitted(nnBTC),type="l",col="blue")

legend("topleft", 
       legend = c("Train Data", "Test Data", "ANN Model"), 
       col = c("black","red","blue"), 
       bty = "n", 
       cex = 0.9,
       pch = 16,
       text.col = "black")
