library(mlbench)
library(tree)
data(Glass)
m <- dim(Glass)[1]
Glass.tr <- tree(Type ~., Glass)
library(maptree)
draw.tree(Glass.tr, cex=0.7)
Glass.tr
Glass.tr1 <- prune.tree(Glass.tr, k = 56)
draw.tree(Glass.tr1, cex=0.7)

library(DAAG)
library(tree)
data(spam7)
sp.tr <- tree(yesno ~., spam7)
library(maptree)
draw.tree(sp.tr, cex = 0.7)
tr1 <- prune.tree(spam_tree,method = "misclass")
for(i in 2:4)
{
  tr2 <- prune.tree(spam_tree,k=tr1$k[i],method = "misclass")
  png(filename=paste(toString(i),'.jpg'))
  draw.tree(tr2)
  dev.off()
}

library("tree")
library("DAAG")
library("maptree")
library("kernlab")  
data(nsw74psid1)
tr <- tree(re78 ~.,nsw74psid1)
draw.tree(tr, cex = 0.7)
res <- lm(re78 ~ ., data = nsw74psid1)
summary(res)
confint(res)
ksvm(re78 ~ ., data=nsw74psid1)

library("tree")
A_raw <- read.table("C:/Users/gdk17/OneDrive/Рабочий стол/универ/machine_learning/Lab_3_Tree/Lenses.txt", sep = ',', stringsAsFactors = TRUE)
m <- dim(A_raw)[1]
A_raw <- A_raw[,-1]
A_raw.tr <- tree(V6 ~., A_raw)
library(maptree)
draw.tree(A_raw.tr, cex = 0.7)

library(kknn)
data(glass)
gl <- glass[,-1]
tr <- tree(Type~ .,gl)
library(maptree)
draw.tree(tr,cex=0.7)
ex<-data.frame(1.516, 11.7, 1.01,1.19,72.59,0.43,11.44,0.02,0.1)
l <- c("RI", "Na", "Mg", "Al","Si","K","Ca","Ba","Fe")
colnames(ex) <- l