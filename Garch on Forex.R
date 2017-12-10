data <- read.csv("/Users/Nic/Downloads/coindesk-bpi-USD-close_data-2017-06-01_2017-06-21.csv")

trainingdata <- data[1:400,2]
testdata <- data[401:504,2]

x1 <- seq(401,504,1)

plot(trainingdata, ylim=range(data[,2]), xlim=range(1,504), type = "l", col = "black", ylab = "BTC to USD", xlab = "Hours since 01/06/2017", main = "ARCH")
par(new = TRUE)
plot(x = x1, testdata, ylim=range(data[,2]), xlim=range(1,504), axes = FALSE, xlab = "", ylab = "", type = "l", col = "blue")
par(new = TRUE)

set.seed(123)
garchPred1 = integer(104)


  garchModel1 <- garchFit(~garch(4,1),data = data[1:399+i,2],trace=F)
  temp <-predict(garchModel1, n.ahead = 104)
  
for(i in 1:104)
{
  garchPred1[i] <- (temp$meanForecast[i] + temp$meanError[i])-160
}

plot(x = x1, y = garchPred1, ylim=range(data[,2]), xlim=range(1,504), axes = FALSE, xlab = "", ylab = "", type = "l", col = "red")
par(new = TRUE)

legend("topleft", 
       legend = c("Train Data", "Test Data", "pred"), 
       col = c("black","blue","red"), 
       bty = "n", 
       cex = 0.9,
       pch = 16,
       text.col = "black")
