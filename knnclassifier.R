library(class)
library("openxlsx")
options(java.parameters = "-Xmx2048m")
setwd("C:/Users/Naseem Ashraf/Desktop/Fall 16/DM Project sets")

newproductdataset <- read.xlsx("newproductdatasetclassifiedNEW.xlsx", sheet = 1,startRow = 1, colNames = TRUE)
colnames(newproductdataset) <- c("ProductID","Description","TransactionFreq","TotalQuantity","Customers","MeanQuantityPerTransaction","MeanQuantityPerCustomer", "UnitPrice","MeanEarningPerTransaction","SalePriorityClass")

attach(newproductdataset)

#######################
##Data Exploration
#######################
#summary(newproductdataset)
class <- as.factor(newproductdataset$SalePriorityClass)
levels(class)

counts1 <- table(class)
barplot(counts1, col = c("red","orange","green"), ylab = "No. of Products" ,
        names.arg=c("High","Medium","Low"), 
        xlab = "Product Sale Priority")
text(0.70, 150, counts1[1])
text(1.85, 400, counts1[2])
text(3.10, 700, counts1[3])

#######################
##Split Test and Training Data in 1:9 ratio [Note: Using sample seeding of 42]
#######################
newproductdataset <- read.xlsx("newproductdatasetclassifiedNEW.xlsx", sheet = 1,startRow = 1, colNames = TRUE)
colnames(newproductdataset) <- c("ProductID","Description","TransactionFreq","TotalQuantity","Customers","MeanQuantityPerTransaction","MeanQuantityPerCustomer", "UnitPrice","MeanEarningPerTransaction","SalePriorityClass")

data1 <- newproductdataset[order(ProductID),] #Reordering by ProductID in order to shuffle TransactionFreq based pre-sorted data.
set.seed(42);
data1 <- data1[sample(nrow(data1)),]

inx <- c(1:3877)
data1$"index" <- inx
rm(inx)

attach(data1)
#train <- as.integer(3877*0.90) #3489.3 ~ 90%
train = index<3489

#######################
##Cross Validated KNN
#######################
data1 <- newproductdataset[order(ProductID),] #Reordering by ProductID in order to shuffle TransactionFreq based pre-sorted data.
set.seed(42);
data1 <- data1[sample(nrow(data1)),]

inx <- c(1:3877)
data1$"index" <- inx
rm(inx)

attach(data1)
#train <- as.integer(3877*0.90) #3489.3 ~ 90%
train = index<3489

## KNN.CV Model 1 
attach(data1)
train.features <- cbind(TransactionFreq, TotalQuantity, Customers, MeanQuantityPerTransaction, MeanQuantityPerCustomer,
                        UnitPrice, MeanEarningPerTransaction)

indices <- seq(1,20)
cv.predy <- c()
cv.pred.meany <- c()
set.seed(42);
for(ki in seq(1,20)){
  cv.pred <- knn.cv(train.features,SalePriorityClass,k=ki, prob=FALSE, use.all=TRUE)
  cv.predy <- c(cv.predy,cv.pred)
  cv.pred.mean <- 1-mean(cv.pred==SalePriorityClass)
  cv.pred.meany <- c(cv.pred.meany, cv.pred.mean*100)
}

print(cv.pred.meany)
bp <- barplot(cv.pred.meany, ylim = c(8,12), beside=TRUE, xpd = FALSE, names.arg = indices)
text(bp, cv.pred.meany, labels = formatC(cv.pred.meany,digits=3,format="f"), srt = 90) 
## k = 3 & 7

## KNN.CV Model 2
attach(data1)
train.features <- cbind(TransactionFreq, Customers, MeanQuantityPerTransaction, 
                        MeanEarningPerTransaction)
indices <- seq(1,20)
cv.predy <- c()
cv.pred.meany <- c()
set.seed(42);
for(ki in seq(1,20)){
  cv.pred <- knn.cv(train.features,SalePriorityClass,k=ki, prob=FALSE, use.all=TRUE)
  cv.predy <- c(cv.predy,cv.pred)
  cv.pred.mean <- 1-mean(cv.pred==SalePriorityClass)
  cv.pred.meany <- c(cv.pred.meany, cv.pred.mean*100)
}

print(cv.pred.meany)
bp <- barplot(cv.pred.meany, ylim = c(3,7), beside=TRUE, xpd = FALSE, names.arg = indices)
text(bp, cv.pred.meany, labels = formatC(cv.pred.meany,digits=3,format="f"), srt = 90) 
## k = 1, 3, 6

## KNN.CV Model 3
attach(data1)
train.features <- cbind(TransactionFreq, Customers, MeanEarningPerTransaction)
indices <- seq(1,20)
cv.predy <- c()
cv.pred.meany <- c()
set.seed(42);
for(ki in seq(1,20)){
  cv.pred <- knn.cv(train.features,SalePriorityClass,k=ki, prob=FALSE, use.all=TRUE)
  cv.predy <- c(cv.predy,cv.pred)
  cv.pred.mean <- 1-mean(cv.pred==SalePriorityClass)
  cv.pred.meany <- c(cv.pred.meany, cv.pred.mean*100)
}

print(cv.pred.meany)
bp <- barplot(cv.pred.meany, ylim = c(2,6), beside=TRUE, xpd = FALSE, names.arg = indices)
text(bp, cv.pred.meany, labels = formatC(cv.pred.meany,digits=3,format="f"), srt = 90) 
## k = 1, 3, 5

#######################
## KNN Best Fold Case
#######################

## KNN Model 1 (k=3,7) 
attach(data1)
set.seed(42);
ds1 = cbind(TransactionFreq, TotalQuantity, Customers, MeanQuantityPerTransaction, MeanQuantityPerCustomer,
            UnitPrice, MeanEarningPerTransaction)
knn.pred = knn(ds1[train,],ds1[!train,],SalePriorityClass[train],k=3)
#head(knn.pred)
table(knn.pred,SalePriorityClass[!train])
m1 <- mean(knn.pred==SalePriorityClass[!train])
m1
m1a <- m1
# knn.pred   1   2   3
#        1  13   9   0
#        2  21  43   3
#        3   0   2 298
# 0.9100257

attach(data1)
set.seed(42);
ds1 = cbind(TransactionFreq, TotalQuantity, Customers, MeanQuantityPerTransaction, MeanQuantityPerCustomer,
            UnitPrice, MeanEarningPerTransaction)
knn.pred = knn(ds1[train,],ds1[!train,],SalePriorityClass[train],k=7)
#head(knn.pred)
table(knn.pred,SalePriorityClass[!train])
m1 <- mean(knn.pred==SalePriorityClass[!train])
m1
m1b <- m1
# knn.pred   1   2   3
#        1  13   5   0
#        2  21  48   7
#        3   0   1 294
# 0.9125964

errors <- c((1-m1a)*100, (1-m1b)*100)
barplot(errors, ylab = "Error %", ylim = c(0,11), names.arg=c("K = 3","K = 7"), 
        xlab = "KNN Model 1")
text(0.70, 9.7, format((1-m1a)*100))
text(1.85, 9.5, format((1-m1b)*100))
# format(round(x, 2), nsmall = 2)

## KNN Model 2 (k = 1,3,6) 
attach(data1)
set.seed(42);
ds1 = cbind(TransactionFreq, Customers, MeanQuantityPerTransaction, MeanEarningPerTransaction)
knn.pred = knn(ds1[train,],ds1[!train,],SalePriorityClass[train],k=1)
#head(knn.pred)
table(knn.pred,SalePriorityClass[!train])
m2 <- mean(knn.pred==SalePriorityClass[!train])
m2
m2a <- m2
# knn.pred   1   2   3
#        1  24   6   0
#        2  10  48   1
#        3   0   0 300
# 0.9562982

attach(data1)
set.seed(42);
ds1 = cbind(TransactionFreq, Customers, MeanQuantityPerTransaction, MeanEarningPerTransaction)
knn.pred = knn(ds1[train,],ds1[!train,],SalePriorityClass[train],k=3)
#head(knn.pred)
table(knn.pred,SalePriorityClass[!train])
m2 <- mean(knn.pred==SalePriorityClass[!train])
m2
m2b <- m2
# knn.pred   1   2   3
#        1  22   3   0
#        2  12  51   2
#        3   0   0 299
# 0.9562982

attach(data1)
set.seed(42);
ds1 = cbind(TransactionFreq, Customers, MeanQuantityPerTransaction, MeanEarningPerTransaction)
knn.pred = knn(ds1[train,],ds1[!train,],SalePriorityClass[train],k=6)
#head(knn.pred)
table(knn.pred,SalePriorityClass[!train])
m2 <- mean(knn.pred==SalePriorityClass[!train])
m2
m2c <- m2
# knn.pred   1   2   3
#        1  23   2   0
#        2  11  51   1
#        3   0   1 300
# 0.9614396

errors <- c((1-m2a)*100, (1-m2b)*100, (1-m2c)*100)
barplot(errors, ylab = "Error %", ylim = c(0,11), names.arg=c("K = 1","K = 3","K = 6"), 
        xlab = "KNN Model 2")
text(0.70, 5, format((1-m2a)*100))
text(1.85, 5, format((1-m2b)*100))
text(3.00, 5, format((1-m2c)*100))

## KNN Model 3 (k = 1, 3, 5) 
attach(data1)
set.seed(42);
ds1 = cbind(TransactionFreq, Customers, MeanEarningPerTransaction)
knn.pred = knn(ds1[train,],ds1[!train,],SalePriorityClass[train],k=1)
#head(knn.pred)
table(knn.pred,SalePriorityClass[!train])
m3 <- mean(knn.pred==SalePriorityClass[!train])
m3
m3a <- m3
# knn.pred   1   2   3
#        1  26   5   0
#        2   8  49   1
#        3   0   0 300
# 0.9640103

attach(data1)
set.seed(42);
ds1 = cbind(TransactionFreq, Customers, MeanEarningPerTransaction)
knn.pred = knn(ds1[train,],ds1[!train,],SalePriorityClass[train],k=3)
#head(knn.pred)
table(knn.pred,SalePriorityClass[!train])
m3 <- mean(knn.pred==SalePriorityClass[!train])
m3
m3b <- m3
# knn.pred   1   2   3
#        1  24   2   0
#        2  10  52   2
#        3   0   0 299
# 0.9640103

attach(data1)
set.seed(42);
ds1 = cbind(TransactionFreq, Customers, MeanEarningPerTransaction)
knn.pred = knn(ds1[train,],ds1[!train,],SalePriorityClass[train],k=5)
#head(knn.pred)
table(knn.pred,SalePriorityClass[!train])
m3 <- mean(knn.pred==SalePriorityClass[!train])
m3
m3c <- m3
# knn.pred   1   2   3
#        1  23   2   0
#        2  11  52   2
#        3   0   0 299
# 0.9614396

errors <- c((1-m3a)*100, (1-m3b)*100, (1-m3c)*100)
barplot(errors, ylab = "Error %", ylim = c(0,6), names.arg=c("K = 1","K = 3","K = 6"), 
        xlab = "KNN Model 3")
text(0.70, 5, format((1-m3a)*100))
text(1.85, 5, format((1-m3b)*100))
text(3.00, 5, format((1-m3c)*100))


#KNN Model performance
errors <- c((1-m1b)*100, (1-m2b)*100, (1-m3b)*100)
barplot(errors, ylab = "Error %", ylim = c(0,10), names.arg=c(1,2,3), 
        xlab = "KNN Model Number")
text(0.70, 9.5, format((1-m1)*100))
text(1.85, 6, format((1-m2)*100))
text(3.10, 4.5, format((1-m3a)*100))
