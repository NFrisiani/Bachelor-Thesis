data <- read.csv("/Users/Nic/Downloads/coindesk-bpi-USD-close_data-2017-06-01_2017-06-21.csv")

plot(x = data[,1], y=data[,2])


trainingdata <- data[1:400,2]
testdata <- data[401:504,2]

x1 <- seq(401,504,1)


plot(trainingdata, ylim=range(trainingdata), xlim=range(1,504), type = "l", col = "black",
     ylab = "BTC to USD", xlab = "Hours since 01/06/2017", main = "ARMA")
par(new = TRUE)
plot(x = x1, testdata, ylim=range(trainingdata), xlim=range(1,504), axes = FALSE, xlab = "", 
     ylab = "", type = "l", col = "blue")
par(new = TRUE)

auto.arima(trainingdata)

bitcoin.arima022 <- arima(trainingdata, order = c(4,0,4), method = "CSS")
bitcoin.pred022 <- predict(bitcoin.arima022, 104)

plot(bitcoin.pred022$pred, ylim=range(trainingdata), xlim=range(1,504), axes = FALSE, 
     xlab = "", ylab = "", type = "l", col = "red")
par(new = TRUE)
points(1:length(trainingdata),fitted(bitcoin.arima022),type="l",col="red")
par(new = TRUE)


bitcoin.arima234 <- arima(trainingdata, order = c(1,0,2), method = "CSS")
bitcoin.pred234 <- predict(bitcoin.arima234, 104)

plot(bitcoin.pred234$pred, ylim=range(trainingdata), xlim=range(1,504), axes = FALSE, 
     xlab = "", ylab = "", type = "l", col = "green")
par(new = TRUE)
points(1:length(trainingdata),fitted(bitcoin.arima234),type="l",col="green")
par(new = TRUE)


legend("topleft", 
       legend = c("Train Data", "Test Data", "p=4, q=4", "p=1, q=2"), 
       col = c("black","blue","red","green"), 
       bty = "n", 
       cex = 0.9,
       pch = 16,
       text.col = "black")


