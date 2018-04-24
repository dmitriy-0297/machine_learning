#Unit 1
A <- read.table("C:/Users/gdk17/OneDrive/Рабочий стол/универ/machine_learning/Lab_5_Regression/reglab1.txt", header = TRUE, sep = "\t")
data <- lm(z ~ ., data = A)
summary(data)

#Unit 2
A <- read.table("C:/Users/gdk17/OneDrive/Рабочий стол/универ/machine_learning/Lab_5_Regression/reglab2.txt", header = TRUE, sep = "\t")
combination <- combn(A[, 2:5], 2)
for(i in 1:length(combination[1,]))
{
  tmp <- data.frame(A$y, combination[1, i], combination[2, i])
  res <- lm(tmp$A.y ~., data = tmp)
  print(sum((tmp$A.y - res$fitted.values)^2))
}

#Unit 3
A <- read.table("C:/Users/gdk17/OneDrive/Рабочий стол/универ/machine_learning/Lab_5_Regression/cygage.txt", header = TRUE, sep = "\t")
f <- lm(calAge~Depth, data=A, weights = A$Weight)
summary(f)

#Unit 4
library(MASS)
library(datasets)
A<-longley
res <- lm(Employed ~., data = A)
summary(res)
A.1 <- A[,-5]
set.seed(12345)
n <- dim(A.1)[1]
A_rand <- A.1[ order(runif(n)), ]
A_train <- A_rand[1:(n/2),]
A_test <- A_rand[((n/2)+1):n,]
ml = c()
for (i in 1:25){
  ml <- c(ml, 10^(-3 + 0.2 * i))
}
res <- lm.ridge(Employed ~., data = A_train, lambda = ml)
plot(x=res$lambda, y=res$GCV, type="o")
lambda <- res$GCV[which.min(res$GCV)]
res <- lm.ridge(Employed ~., data = A_train, lambda = lambda)
print(res)
for(i in 1:25){
  res.test <- lm(Employed  ~., data = A_train)
  res.error <- (res.test$fitted.values - A_train$Employed) ^ 2
  res <- lm.ridge(Employed ~., data = A_train, lambda = ml[i])
  res.error <- res.error + ml[i] * sum(abs(res$coef))
  print(res.error)
}

#Unit 5
library(datasets)
data("EuStockMarkets")
plot(EuStockMarkets)
for(i in 1:4){
  res <- lm(EuStockMarkets[,i]~., data = EuStockMarkets[,-i])
  print(res)
  print(summary(res))
  png(file = paste(toString(i),'lm.png'))
  plot(res)
  dev.off()
}
plot(0.4982 * EuStockMarkets[, 2] + 0.4957*EuStockMarkets[, 3] - 0.0172 * EuStockMarkets[,4], col = "red") 
lines(7.263e-01* EuStockMarkets[,1] + 9.905e-02 * EuStockMarkets[,3] + 8.451e-01 * EuStockMarkets[, 4], col = "blue")
lines(0.72037 * EuStockMarkets[, 1] + 0.09767 * EuStockMarkets[, 2] -0.40078*EuStockMarkets[,4], col = "green")
lines(-0.02123 * EuStockMarkets[, 1] + 0.70758  * EuStockMarkets[, 2]  -0.34029*EuStockMarkets[,3])

#Unit 6
library(datasets)
A <- JohnsonJohnson
plot(A)
i = 1
A.1 <- cbind(A[i], A[i+1], A[i+2], A[i+3])
i = 5
while(i < 84){
  myTmp <- cbind(A[i], A[i+1], A[i+2], A[i+3])
  A.1 <- rbind(A.1, myTmp)
  i <- i + 4
}
A.1 <- data.frame(A.1, 1960:1980)
res<-lm(X1+X2+X3+X4~X1960.1980, data = A.1)
res1 <- lm(X1~X1960.1980, data = A.1)
res2<-lm(X2~X1960.1980, data = A.1)
res3<-lm(X3~X1960.1980, data = A.1)
res4<-lm(X4~X1960.1980, data = A.1)
plot(res$fitted.values, x = (1960:1980))
lines(res1$fitted.values, col = "black",  x = (1960:1980))
lines(res2$fitted.values, col = "blue",  x = (1960:1980))
lines(res3$fitted.values, col = "green", x = (1960:1980))
lines(res4$fitted.values, col = "red", x = (1960:1980))

new<- data.frame(X1960.1980 = (2016))
pred <- predict(res, newdata = new )
print(pred)

#Unit 7
library(datasets)
A <- sunspot.year
plot(A)  
A <- data.frame(A, Year=1700:1988)
res <- lm(A~Year, data = A)
plot(res$fitted.values)

#Unit 8
library(datasets)
A <- read.csv("C:/Users/gdk17/OneDrive/Рабочий стол/универ/machine_learning/Lab_5_Regression/UKgas.csv")
i = 1
print(A[i,3])
A.1 <- cbind(A[i,3], A[i+1,3], A[i+2,3], A[i+3, 3])
i = 5
while(i < 108){
  Tmp <- cbind(A[i,3], A[i+1,3], A[i+2,3], A[i+3, 3])
  A.1 <- rbind(A.1, Tmp)
  i <- i + 4
}
A.1 <- data.frame(A.1, Year=1960:1986)
res<-lm(X1+X2+X3+X4~Year, data = A.1)
res1<-lm(X1~Year, data = A.1)
res2<-lm(X2~Year, data = A.1)
res3<-lm(X3~Year, data = A.1)
res4<-lm(X4~Year, data = A.1)
plot(res$fitted.values, x = (1960:1986))
lines(res1$fitted.values, col = "yellow",  x = (1960:1986))
lines(res2$fitted.values, col = "blue",  x = (1960:1986))
lines(res3$fitted.values, col = "green", x = (1960:1986))
lines(res4$fitted.values, col = "red", x = (1960:1986))
new<- data.frame(Year = (2016))
pred <- predict(res, newdata = new )
print(pred)
plot(pred, x = (1980:2016))

#Unit 9
library(datasets)
A <- cars
res<-lm(dist~speed, data = A)
plot(res$fitted.values)
new <-data.frame(speed=40)
pred <- predict(res,newdata = new)
print(pred)
