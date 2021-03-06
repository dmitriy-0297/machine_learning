#install.packages("e1071")
library(e1071)
### Naive Bayesian (������ ��������������) #########################
# 1 #############################################################
# ����������� ������ � R
# ���������� �������� stringsAsFactors = TRUE, 
# ��� ��� ��� ������ - ��������������
A_raw <- read.table("C:/Users/gdk17/OneDrive/������� ����/������/machine_learning/Lab1_NaiveBayesian/Tic_tac_toe.txt", sep = ",", stringsAsFactors = TRUE)
# ����� ����� � ����
n <- dim(A_raw)[1]
# ������ �����, ������� ����� �����������, ��������� str(A_raw).
# ������� 9 �������� ��������� V1-V9 � V10 (�����) � 
# ��� ����� ���� � ��� �� ��� Factor.
# 2 #############################################################
# �������� ��������� � ����������� �������
# ������, ����� n �������� � �������� �������, 
# ���������� 80% ��� �������� � ���������� - ��� ������������. 
# ������������� ���� ��������� ��������� ����� � ������������� �������
set.seed(12345)
A_rand <- A_raw[ order(runif(n)), ]
# �������� ������ �� ��������� � �����������
nt <- as.integer(n*0.9)
A_train <- A_rand[1:nt, ]
A_test <- A_rand[(nt+1):n, ]
# ����� ���������, ����� ������� ������� ������� 
# ������ V2 � ��������� � ����������� �������
prop.table(table(A_train$V10))
prop.table(table(A_test$V10))
# 3 ############################################################
# ���������� ������� ����������� ������������� �� ������ e1071
#	A_classifier <- naiveBayes(A_train[,-10], A_train$V10)
# ������ ������� ��������������
A_classifier <- naiveBayes(V10 ~ ., data = A_train)
# 4 ############################################################
# ������ ������ ���������� ������:
A_predicted <- predict(A_classifier, A_test)
# ���������� table ��� ��������� �������������� �������� � ���, ��� ����
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


array1 <- rnorm(50, 10, 4) #������� X1
array2 <- rnorm(50, 14, 4) #������� x2
array3 <- rnorm(50, 20, 3) #������� X1
array4 <- rnorm(50, 18, 3) #������� x2
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