data <- read.csv("/Users/Nic/Downloads/coindesk-bpi-USD-close_data-2017-06-01_2017-06-21.csv")

trainingdata <- data[1:400,2]
testdata <- data[401:504,2]


x1 <- seq(401,504,1)
plot(trainingdata, ylim=range(trainingdata), xlim=range(1,504), xlab="Hours since 01/06/2017",
     ylab="BTC to USD", type = "l", col = "black", main = "AR")
par(new = TRUE)
plot(x = x1, y = testdata, ylim=range(trainingdata), xlim=range(1,504), axes = FALSE, 
     xlab = "", ylab = "", type = "l", col = "blue")
par(new = TRUE)

ar1 <- ar.burg(trainingdata, order.max = 6, aic = FALSE)
ar1_pred <- predict(object = ar1, n.ahead = 104)
plot(ar1_pred$pred, ylim=range(trainingdata), xlim=range(1,504), axes = FALSE, xlab = "", 
     ylab = "", type = "l", col = "red")
par(new = TRUE)
points(1:length(trainingdata),fitted(ar1),type="l",col="red")
par(new = TRUE)


ar2 <- ar.burg(trainingdata, order.max = 12, aic = FALSE)
ar2_pred <- predict(object = ar2, n.ahead = 104)
plot(ar2_pred$pred, ylim=range(trainingdata), xlim=range(1,504), axes = FALSE, xlab = "",
     ylab = "", type = "l", col = "orange")
par(new = TRUE)
points(1:length(trainingdata),fitted(ar2),type="l",col="orange")
par(new = TRUE)


ar3 <- ar.yw(trainingdata, order.max = 6, aic = FALSE)
ar3_pred <- predict(object = ar3, n.ahead = 104)
plot(ar3_pred$pred, ylim=range(trainingdata), xlim=range(1,504), axes = FALSE, xlab = "", 
     ylab = "", type = "l", col = "green")
par(new = TRUE)
points(1:length(trainingdata),fitted(ar3),type="l",col="green")
par(new = TRUE)


ar4 <- ar.yw(trainingdata, order.max = 12, aic = FALSE)
ar4_pred <- predict(object = ar4, n.ahead = 104)
plot(ar4_pred$pred, ylim=range(trainingdata), xlim=range(1,504), axes = FALSE, xlab = "", 
     ylab = "", type = "l", col = "darkviolet")
par(new = TRUE)
points(1:length(trainingdata),fitted(ar4),type="l",col="darkviolet")
par(new = TRUE)


legend("topleft", 
       legend=c("Test Data","Train Data","6th Order Burg", "12th Order Burg", 
                "6th Order Yule-Walker", "12th Order Yule-Walker"), 
       col=c("black","blue","red", "orange", "green", "darkviolet"), 
       bty = "n", 
       cex = 0.9, 
       pch = 16, 
       text.col = "black")