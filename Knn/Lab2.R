##исследуем, как объём обучающей выборки и количество тестовых данных влияют на вероятность ошибочной классификации 
library(kknn)
A_raw <- read.table("C:/Users/gdk17/OneDrive/Рабочий стол/универ/machine_learning/Lab2_Knn/Tic_tac_toe.txt", sep = ",", stringsAsFactors = TRUE)
n <- dim(A_raw)[1]
set.seed(12345)
A_rand <- A_raw[ order(runif(n)), ]
for(i in seq(0.1, 0.9, by = 0.1))
{
  nt <- as.integer(n*i)
  A_train <- A_rand[1:nt, ]
  A_test <- A_rand[(nt+1):n, ]
  
  A_classifier <- train.kknn(V10 ~ ., A_train, kmax = 15,kernel = c("triangular", "rectangular", "epanechnikov", "optimal"), distance = 1)
  A_predicted <- predict(A_classifier, A_test)
  t <- table(A_predicted, A_test$V10)
  print(t)  
}

library(kknn)
library(kernlab)
data(spam)
#spam[0:1, ]
size <- length(spam)

k_pos <- integer(9)
k_neg <- integer(9)

k_pos_test <- integer(9)
k_neg_test <- integer(9)
for (i in 1:9) {
  idx <- sample(1:dim(spam)[1], size * i / 10)
  spamtrain <- spam[-idx, ]
  spamtest <- spam[idx, ]
  
  kknn_predicted <- kknn(type ~ ., spamtrain, spamtest)
  kknn_res_table <- table(kknn_predicted$fitted.values, spamtest$type)
  pos <- kknn_res_table['nonspam', 'nonspam'] + kknn_res_table['spam', 'spam']
  neg <- kknn_res_table['nonspam', 'spam'] + kknn_res_table['spam', 'nonspam']
  k_pos[i] <- pos / length(kknn_predicted$fitted.values)
  k_neg[i] <- neg / length(kknn_predicted$fitted.values)
}

png(file = 'k_spam.jpg')
plot(k_pos, type = 'l', col="red", ylim=range(c(k_pos, k_neg)))
lines(k_neg, col="green", ylim=range(c(k_pos, k_neg)))
dev.off()



## создаем фрейм
id <- seq(1,214)
RI <- runif(214, 0, 3)
Na <- runif(214, 0, 20)
Mg <- runif(214, 0, 3)
Al <- runif(214, 0, 3)
Si <- runif(214, 0, 100)
K <- runif(214, 0, 2)
Ca <- runif(214, 0, 20)
Ba <- runif(214, 0, 1)
Fr <- runif(214, 0, 2)
class <- factor(c(rep((1), 30), rep((2), 30), rep((3),30), rep((4),30), rep((5),30), rep((6),30), rep((7),34)))
glass = data.frame(id, RI, Na, Mg, Al, Si, K, Ca, Ba, Fr, class)
glass <- glass[,-1]

##построение графиков ошибок от K(ядра), анализ oшибок при варьировании distace
library(kknn)
glass.learn <- glass[1:200,]
glass.valid <- glass[-c(1:200),]
fit.kknn <- kknn(class ~ ., glass.learn, glass.valid)
fit.train1 <- train.kknn(class ~ ., glass.learn, kmax = 15, kernel = c("triangular", "rectangular", "epanechnikov", "optimal"), distance = 1)
fit.train2 <- train.kknn(class ~ ., glass.learn, kmax = 15, kernel = c("triangular", "rectangular", "epanechnikov", "optimal"), distance = 4)
#plot(fit.train1)
plot(fit.train2)

##запуск  knn с одним ядром, построение диограммы
m <- dim(glass)[1]
val <- sample(1:m, size = round(m/3), replace = FALSE,prob = rep(1/m, m))
glass.learn <- glass[-val,]
glass.valid <- glass[val,]
glass.kknn <- kknn(class~., glass.learn, glass.valid, distance = 1, kernel = "triangular")
summary(glass.kknn)
fit <- fitted(glass.kknn)
table(glass.valid$class, fit)
pcol <- as.character(as.numeric(glass.valid$class))
pairs(glass.valid[1:10], pch = pcol, col = c("green3", "red")
      [(glass.valid$class != fit)+1])

##получение класса по заданным признакам
RI <- c(1.51)
Na <- c(11.7)
Mg <- c(1.01)
Al <- c(1.19)
Si <- c(72.59)
K <- c(0.43)
Ca <- c(0.0)
Ba <- c(0.02)
Fr <- c(0.10)
class <- c(1)

example <- data.frame(RI, Na, Mg, Al, Si, K, Ca, Ba, Fr, class)

example.kknn <- kknn(class~., glass.learn, example, distance = 1, kernel = "triangular")
summary(example.kknn)
fit <- fitted(example.kknn)
table(example$class, fit)
pcol <- as.character(as.numeric(example$class))
pairs(glass.valid[1:10], pch = pcol, col = c("green3", "red")
      [(glass.valid$class != fit)+1])


library(kknn) #подбираем оптимальное значение k для svmdata4.txt
A_train <- read.table("C:/Users/gdk17/OneDrive/Рабочий стол/универ/machine_learning/Lab2_Knn/svmdata4.txt")
A_test <-  read.table("C:/Users/gdk17/OneDrive/Рабочий стол/универ/machine_learning/Lab2_Knn/svmdata4test.txt")
A_train <- A_train[,-1]
A_test <- A_test[,-1]
A.learn <- A_train
A.valid <- A_test
fit.kknn <- kknn(V4 ~ ., A.learn, A.valid)
fit.train2 <- train.kknn(V4 ~ ., A.learn, kmax = 15, kernel = c("triangular", "rectangular", "epanechnikov", "optimal"), distance = 1)
plot(fit.train2)
plot(A_train$V2, A_train$V3, pch=21, bg=c("red","blue") [unclass(A_train$V4)],  main="My train data")
