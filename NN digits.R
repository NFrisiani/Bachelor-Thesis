data <- read.csv("/Users/Nic/Documents/Scuola/5-UoM/Year 3/3rd Year Project/Exercise 3 - NN/digits.csv")

trainingData <- data[1:8, ]
testData <-data[9:12, ]

w = integer(25)
b = 0
alpha = 0.1
y = integer(8)

for(i in 1:8)
{
  if((sum(t(trainingData[i,2:26])*w) + b) > 0)
  {
    y[i] = 1
  }else
  {
    y[i] = 0
  }
  
  w = w + alpha*(trainingData$Number[i] - y[i])*trainingData[i,2:26]
  b = b + alpha*(trainingData$Number[i] - y[i])
}


#prediction

result = integer(4)

for(i in 1:4)
{
  if((sum(t(testData[i,2:26])*w) + b) > 0)
  {
    result[i] = 1
  }else
  {
    result[i] = 0
  }
}