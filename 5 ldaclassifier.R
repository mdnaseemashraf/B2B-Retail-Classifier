#LDA
library(MASS)
library(class)
library("openxlsx")
options(java.parameters = "-Xmx2048m")
setwd("C:/Users/Naseem Ashraf/Desktop/Fall 16/DM Project sets")

newproductdataset <- read.xlsx("newproductdatasetclassifiedNEW.xlsx", sheet = 1,startRow = 1, colNames = TRUE)
colnames(newproductdataset) <- c("ProductID","Description","TransactionFreq","TotalQuantity","Customers","MeanQuantityPerTransaction","MeanQuantityPerCustomer", "UnitPrice","MeanEarningPerTransaction","SalePriorityClass")

attach(newproductdataset)
#names(newproductdataset)

data1 <- newproductdataset[order(ProductID),] #Reordering by ProductID in order to shuffle TransactionFreq based pre-sorted data.
set.seed(42);
data1 <- data1[sample(nrow(data1)),]

inx <- c(1:3877)
data1$"index" <- inx
rm(inx)

#train <- as.integer(3877*0.90) #3489.3 ~ 90%
attach(data1)
train = index<3489

# LDA Model 1
# Cross Validation
# lda.fit=lda(SalePriorityClass~TransactionFreq + TotalQuantity + Customers + MeanQuantityPerTransaction + MeanQuantityPerCustomer +
#               UnitPrice + MeanEarningPerTransaction, data=data1, subset=index<3489, CV= TRUE, N = 10 )
# lda.fit
lda.fit=lda(SalePriorityClass~TransactionFreq + TotalQuantity + Customers + MeanQuantityPerTransaction + MeanQuantityPerCustomer +
              UnitPrice + MeanEarningPerTransaction, data=data1, subset=index<3489)
lda.fit

plot(lda.fit)
data1.test=subset(data1,index>=3489)
lda.pred=predict(lda.fit,data1.test)
table(lda.pred$class,data1.test$SalePriorityClass)
ld1 <- mean(lda.pred$class==data1.test$SalePriorityClass)
ld1
#      1    2    3
#  1  11   3   1
#  2  18  36   0
#  3   5  15 300
# 0.8920308

# LDA Model 2
# Cross Validation
attach(data1)
# lda.fit=lda(SalePriorityClass~TransactionFreq + Customers + MeanQuantityPerTransaction + MeanEarningPerTransaction, data=data1, subset=index<3489,
#             CV = TRUE, N = 10)
# lda.fit

lda.fit=lda(SalePriorityClass~TransactionFreq + Customers + MeanQuantityPerTransaction + MeanEarningPerTransaction, data=data1, subset=index<3489)
lda.fit
plot(lda.fit)
data1.test=subset(data1,index>=3489)
lda.pred=predict(lda.fit,data1.test)
#lda.pred[1:5,]
#class(lda.pred)
#data.frame(lda.pred)[1:5,]
table(lda.pred$class,data1.test$SalePriorityClass)
ld2 <- mean(lda.pred$class==data1.test$SalePriorityClass)
ld2
#      1    2    3
#  1  11    4    0
#  2  18   35    0
#  3   5   15  301
# 0.8920308

# LDA Model 3
attach(data1)
# Cross Validation
# lda.fit=lda(SalePriorityClass~TransactionFreq + Customers + MeanEarningPerTransaction, data=data1, subset=index<3489, CV=TRUE, N=10)
# lda.fit
lda.fit=lda(SalePriorityClass~TransactionFreq + Customers + MeanEarningPerTransaction, data=data1, subset=index<3489)
lda.fit
plot(lda.fit)
data1.test=subset(data1,index>=3489)
lda.pred=predict(lda.fit,data1.test)
#lda.pred[1:5,]
#class(lda.pred)
#data.frame(lda.pred)[1:5,]
table(lda.pred$class,data1.test$SalePriorityClass)
ld3 <- mean(lda.pred$class==data1.test$SalePriorityClass)
ld3
#     1   2   3
# 1  11   4   0
# 2  18  35   0
# 3   5  15 301
# 0.8920308

#LDA Model performance
errors <- c((1-ld1)*100, (1-ld2)*100, (1-ld3)*100)
barplot(errors, ylab = "Error %", names.arg=c(1,2,3), 
        xlab = "LDA Model Number")
