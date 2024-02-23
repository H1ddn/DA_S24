set.seed(42)
par(mar = rep(0.2,4))
data_matrix <-matrix(rnorm(400), nrow=40)
image(1:10, 1:40, t(data_matrix)[,nrow(data_matrix):1])

heatmap(data_matrix)

for(i in 1:40){
  coin_flip <- rbinom(1, size = 1, prob = 0.5)
  if(coin_flip){
    data_matrix[i,] <- data_matrix[i,] + rep(c(0,3), each = 5)
  }
}

heatmap(data_matrix)

abalone_raw <- read.csv('abalone.csv')

library(caTools)
library(dplyr)

View(abalone_raw)

abalone <- select(abalone_raw, -1)

split = sample.split(abalone$Rings, 
                     SplitRatio = 0.75)
train = subset(abalone, 
               split == TRUE)
test = subset(abalone, 
              split == FALSE)

train_wo_target <- select(train, -8)
test_wo_target <- select(test, -8)

train_scaled = scale(train_wo_target[-13])
test_scaled = scale(test_wo_target[-13])

library(class)
test_pred <- knn(
  train = train_scaled, 
  test = test_scaled,
  cl = train$Rings, 
  k=10
)


data("iris")
View(iris)

iris_selected <- select(iris, -5)

iris_clusters <- kmeans(x=iris_selected, centers=3)

iris_clusters

library(useful)

plot(iris_clusters, data=iris_selected)
