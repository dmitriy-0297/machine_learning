#install.packages("e1071")
library(e1071)
### Naive Bayesian (данные категориальные) #########################
# 1 #############################################################
# импортируем данные в R
# установить параметр stringsAsFactors = TRUE, 
# так как все данные - категориальные
A_raw <- read.table("C:/Users/gdk17/OneDrive/Рабочий стол/универ/machine_learning/Lab1_NaiveBayesian/Tic_tac_toe.txt", sep = ",", stringsAsFactors = TRUE)
# число строк в базе
n <- dim(A_raw)[1]
# Создан фрейм, который можно просмотреть, используя str(A_raw).
# Имеется 9 столбцов признаков V1-V9 и V10 (класс) и 
# все имеют один и тот же тип Factor.
# 2 #############################################################
# Создание обучающей и тестирующей выборки
# Скажем, имеем n примеров в исходной выборке, 
# используем 80% для обучения и оставшиеся - для тестирования. 
# Устанавливаем базу генерации случайных чисел и рандомизируем выборку
set.seed(12345)
A_rand <- A_raw[ order(runif(n)), ]
# разделим данные на обучающие и тестирующие
nt <- as.integer(n*0.9)
A_train <- A_rand[1:nt, ]
A_test <- A_rand[(nt+1):n, ]
# Можно убедиться, какой имеется процент каждого 
# класса V2 в обучающей и тестирующей выборке
prop.table(table(A_train$V10))
prop.table(table(A_test$V10))
# 3 ############################################################
# Используем Наивный Байесовский классификатор из пакета e1071
#	A_classifier <- naiveBayes(A_train[,-10], A_train$V10)
# Другой вариант классификатора
A_classifier <- naiveBayes(V10 ~ ., data = A_train)
# 4 ############################################################
# Теперь оценим полученную модель:
A_predicted <- predict(A_classifier, A_test)
# Используем table для сравнения прогнозируемых значений с тем, что есть
table(A_predicted, A_test$V10)
pairs(A_train, col=A_train$V10)


library(kernlab)
library(e1071)
data(spam)
for(size in seq(20, 4581, by = 500))
{  
  idx <- sample(1:dim(spam)[1], size)
  spamtrain <- spam[-idx, ]
  spamtest <- spam[idx, ]
  model <- naiveBayes(type ~ ., data = spamtrain)
  t <- table(predict(model, spamtest), spamtest$type)
  
  print(t)
}


array1 <- rnorm(50, 10, 4) #признак X1
array2 <- rnorm(50, 14, 4) #признак x2
array3 <- rnorm(50, 20, 3) #признак X1
array4 <- rnorm(50, 18, 3) #признак x2
A_raw = data.frame(c(array1, array3), c(array2, array4), factor(c(rep(-1, 50), rep(1, 50))))
colnames(A_raw) <- c("V1", "V2", "class")
pairs(A_raw[1:2], main="(red = -1, blue = 1)", pch = 21, bg = c("red", "blue")[unclass(A_raw$class)])
A_rand <- A_raw[ order(runif(n)), ]
n <- dim(A_raw)[1]
nt <- as.integer(n*0.1)
A_train <- A_rand[1:nt, ]
A_test <- A_rand[(nt+1):n, ]
prop.table(table(A_train$class))
prop.table(table(A_test$class))
model <- naiveBayes(class ~ ., data = A_train)
print(table(predict(model, A_test), A_test$class))