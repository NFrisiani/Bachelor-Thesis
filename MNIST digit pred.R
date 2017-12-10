data <- read.csv("/Users/Nic/Documents/Scuola/5-UoM/Year 3/3rd Year Project/Exercise 3 - NN/MNIST_train.csv")

for(i in 1:1000)
{
  if(!(data$label[i] == 0 || data$label[i] == 1))
  {
    data <- data[-i, ]
    i = i-1
  }
  
  if(i%%10 == 0)
  {
    print(i)
  }
}

cleanData <- data[1:2000, ]
trainingData <- cleanData[1:1500, ]
testData <- cleanData[1501:2000, ]

for(i in 1:1500)
{
  if(trainingData$label[i] == 0)
  {
    trainingData$label[i] = -1
  }
}

w = integer(784)
b = 0
alpha = 0.1
y = integer(1500)

for(i in 1:1500)
{
  if((sum(t(trainingData[i,2:785])*w) + b) > 0)
  {
    y[i] = 1
  }else
  {
    y[i] = -1
  }
  
  w = w + alpha*(trainingData$label[i] - y[i])*trainingData[i,2:785]
  b = b + alpha*(trainingData$label[i] - y[i])
  
  if(i%%100 == 0)
  {
    print(i)
  }
}


#prediction

result = integer(500)

for(i in 1:500)
{
  if((sum(t(testData[i,2:785])*w) + b) > 0)
  {
    result[i] = 1
  }else
  {
    result[i] = 0
  }
}

acc = 0

for(i in 1:500)
{
  if(result[i] == testData$label[i])
  {
    acc = acc + 1
  }
}

acc/500


plot(result, ylim=range(0,1), col = "red")
par(new = TRUE)
plot(testData$label, ylim=range(0,1), axes = FALSE, xlab = "", ylab = "", col = "green")
par(new = TRUE)

