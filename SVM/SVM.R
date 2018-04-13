#Unit 1
library(e1071)
A_train=read.table("C:/Users/gdk17/OneDrive/Рабочий стол/универ/machine_learning/Lab_6_SVM/svmdata1.txt",header = TRUE, sep="\t")
A_test=read.table("C:/Users/gdk17/OneDrive/Рабочий стол/универ/machine_learning/Lab_6_SVM/svmdata1test.txt",header = TRUE, sep="\t")
symbols.pallete = c("Blue", "Red")
area.pallete = function(n = 2) 
{ 
  cols = rainbow(n)
  cols[1:2] = c("PaleGreen", "Pink") 
  return(cols)
}
plot(X1 ~ X2, A_train, col = Color) 
svmModelLinear = svm(Color ~ ., data = A_train, type = "C-classification", cost = 1, kernel = "linear") 
plot(svmModelLinear, A_train, grid = 250, symbolPalette = symbols.pallete, color.palette = area.pallete) 
predictionsTrain = predict(svmModelLinear, A_train) 
table(A_tran$"Color", predictionsTrain)

#Unit 2
B_train=read.table("C:/Users/gdk17/OneDrive/Рабочий стол/универ/machine_learning/Lab_6_SVM/svmdata2.txt",header = TRUE, sep="\t")
B_test=read.table("C:/Users/gdk17/OneDrive/Рабочий стол/универ/machine_learning/Lab_6_SVM/svmdata2test.txt",header = TRUE, sep="\t")
symbols.pallete = c("Blue", "Red")
area.pallete = function(n = 2) 
{ 
  cols = rainbow(n)
  cols[1:2] = c("PaleGreen", "Pink") 
  return(cols)
}
plot(X1 ~ X2, B_test, col = Colors) 
c = 1
svmModelLinear = svm(Colors ~ ., data = B_test, type = "C-classification", cost = c, kernel = "linear") 
plot(svmModelLinear, B_test, grid = 250, symbolPalette = symbols.pallete, color.palette = area.pallete) 
predictionsTest = predict(svmModelLinear, B_test) 
table(B_test$"Colors", predictionsTest)

#Unit 3
D_test=read.table("C:/Users/gdk17/OneDrive/Рабочий стол/универ/machine_learning/Lab_6_SVM/svmdata3test.txt",header = TRUE, sep="\t")
area.pallete = function(n = 2) 
{ 
  cols = rainbow(n)
  cols[1:2] = c("PaleGreen", "Pink") 
  return(cols)
}
svmModelLinear = svm(Colors ~ ., data = C_test, type = "C-classification", cost = 1, kernel = "radial") 
plot(svmModelLinear, C_test, grid = 250, symbolPalette = symbols.pallete, color.palette = area.pallete)
predictionsTest = predict(svmModelLinear, C_test) 
table(C_test$"Color", predictionsTest)

#Unit 4
D_test=read.table("C:/Users/gdk17/OneDrive/Рабочий стол/универ/machine_learning/Lab_6_SVM/svmdata4test.txt",header = TRUE, sep="\t")
area.pallete = function(n = 2) 
{ 
  cols = rainbow(n)
  cols[1:2] = c("PaleGreen", "Pink") 
  return(cols)
}
svmModelLinear = svm(Colors ~ ., data = D_test, type = "C-classification", cost = 1, kernel = "sigmoid") 
plot(svmModelLinear, D_test, grid = 250, symbolPalette = symbols.pallete, color.palette = area.pallete)
predictionsTest = predict(svmModelLinear, D_test) 
table(D_test$"Color", predictionsTest)

#Unit 5
E_test=read.table("C:/Users/gdk17/OneDrive/Рабочий стол/универ/machine_learning/Lab_6_SVM/svmdata5test.txt",header = TRUE, sep="\t")
area.pallete = function(n = 2) 
{ 
  cols = rainbow(n)
  cols[1:2] = c("PaleGreen", "Pink") 
  return(cols)
}
svmModelLinear = svm(Colors ~ ., data = E_test, type = "C-classification", cost = 1, kernel = "sigmoid") 
plot(svmModelLinear, E_test, grid = 250, symbolPalette = symbols.pallete, color.palette = area.pallete)
predictionsTest = predict(svmModelLinear, E_test) 
table(E_test$"Color", predictionsTest)

#Unit 6
F <-read.table("C:/Users/gdk17/OneDrive/Рабочий стол/универ/machine_learning/Lab_6_SVM/svmdata6.txt",header = TRUE, sep="\t")
set.seed(0)
plot(F$X, F$Y)
svmModel = svm(F$X, F$Y,type = "eps-regression", cost = 1, kernel = "radial", epsilon  = 0.5)
points(F$X[svmModel$index], F$Y[svmModel$index], col = "red") 
predctions = predict(svmModel,F$X) 
lines(F$X, predctions, col = "dodgerblue", lwd = 2) 
lines(F$X, predctions + svmModel$epsilon, col = "cyan") 
lines(F$X, predctions - svmModel$epsilon, col = "cyan")
msquared = c()
for(i in seq(0.05, 1.5, by = 0.05)){
  svmModel = svm(F$X, F$Y, type = "eps-regression", cost = 1, kernel = "radial", epsilon  = i,cross = 1)
  predctions = predict(svmModel, F$X)
  msquared =c(msquared,sum((predctions - F$Y) ^ 2) / length(predctions))
}
plot(msquared, x = seq(0.05, 1.5, by = 0.05))
