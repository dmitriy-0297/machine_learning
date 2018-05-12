#Unit 1
library(cluster)
data("pluton")
for(i in seq(2, 16, by = 2))
{
  cl <- kmeans(pluton, 3, iter.max = i)
  png(filename = paste(toString(i),'claster.png'))
  plot(pluton, col = cl$cluster, main=paste('max=',toString(i)))
  points(cl$centers, col = 1:3, pch = 8, cex=2)
  dev.off()
}

#Unit 2
library(cluster)
cl1 <- c()
cl2 <- c()
for(i in seq(1,50, by=2))
{
  cl1 <- c(cl1,runif(1, min=10, max=11.5))
  cl1 <- c(cl1,runif(1, min=15, max=18.5))
  cl1 <- c(cl1,runif(1, min=20, max=26))
  cl2 <- c(cl2, i)  
}

fr<-data.frame(cl2,cl1)
plot(fr)
res<-clara(fr, 3,metric = "euclidean", stand = FALSE)
png(file = 'cl_1.jpg')
plot(fr, col=res$clustering)
dev.off()

#Unit 3
library(cluster)
data(votes.repub)
plot(agnes(votes.repub))

#Unit 4
library(cluster)
data(animals)
plot(agnes(animals))

#Unit 5
library(cluster)
A_raw <- read.table("C://Users//gdk17//OneDrive//Рабочий стол//универ//machine_learning//Lab_4_Cluster//seeds_dataset.txt", stringsAsFactors = TRUE)
cl <- kmeans(A_raw[,-8], 3, iter.max = 20)
plot(A_raw, col = cl$cluster)






