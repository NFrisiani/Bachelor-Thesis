data <- read.csv("/Users/Nic/Documents/Scuola/5-UoM/Year 3/3rd Year Project/Exercise 1 - AR/bitcoinPrice.csv")
data <- data[1:376,]

plot(x = data[,1], y=data[,2])


trainingdata <- data[1:282,2]
testdata <- data[283:376,2]

x1 <- seq(283,376,1)


plot(trainingdata, ylim=range(data[,2]), xlim=range(1,376), type = "l", col = "blue", ylab = "BTC to USD", xlab = "Hours since the 1st of October", main = "AR Yule-Walker")
par(new = TRUE)
plot(x = x1, testdata, ylim=range(data[,2]), xlim=range(1,376), axes = FALSE, xlab = "", ylab = "", type = "l", col = "green")
par(new = TRUE)


bitcoin.ar6 <- ar.yw(trainingdata, aic = FALSE, order.max = 6)
bitcoin.pred6 <- predict(object = bitcoin.ar6, n.ahead = 94)

plot(x = x1, bitcoin.pred6$pred, ylim=range(data[,2]), xlim=range(1,376), axes = FALSE, xlab = "", ylab = "", type = "l", col = "red")
par(new = TRUE)


bitcoin.ar12 <- ar.yw(trainingdata, aic = FALSE, order.max = 12)
bitcoin.pred12 <- predict(object = bitcoin.ar12, n.ahead = 94)

plot(x = x1, bitcoin.pred12$pred, ylim=range(data[,2]), xlim=range(1,376), axes = FALSE, xlab = "", ylab = "", type = "l", col = "orange")
par(new = TRUE)


bitcoin.ar100 <- ar.yw(trainingdata, aic = FALSE, order.max = 100)
bitcoin.pred100 <- predict(object = bitcoin.ar100, n.ahead = 94)

plot(x = x1, bitcoin.pred100$pred, ylim=range(data[,2]), xlim=range(1,376), axes = FALSE, xlab = "", ylab = "", type = "l", col = "brown")
par(new = TRUE)


bitcoin.ar200 <- ar.yw(trainingdata, aic = FALSE, order.max = 200)
bitcoin.pred200 <- predict(object = bitcoin.ar200, n.ahead = 94)
plot(x = x1, bitcoin.pred200$pred, ylim=range(data[,2]), xlim=range(1,376), axes = FALSE, xlab = "", ylab = "", type = "l", col = "black")
par(new = TRUE)

legend("topleft", 
       legend = c("Train Data", "Pred Data", "6th Order", "12th Order", "100th Order", "200th Order"), 
       col = c("blue","green","red","orange","brown","black"), 
       bty = "n", 
       cex = 0.9,
       pch = 16,
       text.col = "black")
