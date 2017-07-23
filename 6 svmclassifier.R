library( 'e1071' )
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
attach(data1)
#train <- as.integer(1170*0.90) #3489 ~ 90%
train = index<3489

##############################
##SVM Tuned Model 1
##############################
library(caret)
data2 <- data1
data2$"index" <- NULL
data2$"ProductID" <- NULL
data2$"Description" <- NULL
data2$"SalePriorityClass" <- as.factor(data2$"SalePriorityClass")

data2.train <- data1[data1$index<3489,]
data2.test <- data1[data1$index>=3489,]

drops <- c("SalePriorityClass","Description", "index", "ProductID")
data2.train.attributes <- data2.train[,!(names(data2.train) %in% drops)]
data2.test.attributes <- data2.test[,!(names(data2.test) %in% drops)]
# str(data2.train.attributes)
# str(data2.test.attributes)
data2class.train <- as.factor(data2.train$SalePriorityClass)
data2class.test <- as.factor(data2.test$SalePriorityClass)

svm_model1 <- svm(data2.train.attributes, data2class.train)
svm.original.pred1 <- predict(svm_model1, data2.test.attributes)

tab <- table(pred = svm.original.pred1, true = data2class.test)
tab
#         true
# pred   1   2   3
#    1   5   0   1
#    2  29  52   1
#    3   0   2 299
classification_error_original1 <- 1- sum(svm.original.pred1 == data2class.test)/length(svm.original.pred1)
classification_error_original1
# 0.0848329

svm_tune1 <- tune(svm, train.x = data2.train.attributes, train.y = data2class.train,
                 kernel="radial", ranges = list(cost=10^-1:2), gamma=c(.5,1,2))

svm_tune1$best.model
# Call:
#   best.tune(method = svm, train.x = data2.train.attributes, train.y = data2class.train, ranges = list(cost = 10^-1:2), 
#             kernel = "radial", gamma = c(0.5, 1, 2))
# 
# 
# Parameters:
#   SVM-Type:  C-classification 
# SVM-Kernel:  radial 
# cost:  1.1 
# gamma:  0.5 1 2 
# 
# Number of Support Vectors:  659

svm_model1_best <- svm(data2.train.attributes, data2class.train, kernel="radial", cost=1.1, gamma=c(0.5,1,2))

svm.pred1 <- predict(svm_model1_best, data2.test.attributes)

tab <- table(pred = svm.pred1, true = data2class.test)
tab
#         true
# pred   1   2   3
#    1   9   1   1
#    2  25  52   1
#    3   0   1 299
classification_error1 <- 1- sum(svm.pred1 == data2class.test)/length(svm.pred1)
classification_error1
# 0.07455013

##############################
##SVM Tuned Model 2
##############################
library(caret)
## SVM Model 2
data2 <- data1
data2$"index" <- NULL
data2$"ProductID" <- NULL
data2$"Description" <- NULL
data2$"SalePriorityClass" <- as.factor(data2$"SalePriorityClass")

data2.train <- data1[data1$index<3489,]
data2.test <- data1[data1$index>=3489,]

# cbind(TransactionFreq, Customers, MeanQuantityPerTransaction, 
#       MeanEarningPerTransaction)
drops <- c("ProductID","SalePriorityClass","Description", "index", "TotalQuantity", "MeanQuantityPerCustomer",
           "UnitPrice")
data2.train.attributes <- data2.train[,!(names(data2.train) %in% drops)]
data2.test.attributes <- data2.test[,!(names(data2.test) %in% drops)]
names(data2.train.attributes)

str(data2.train.attributes)
str(data2.test.attributes)

data2class.train <- as.factor(data2.train$SalePriorityClass)
data2class.test <- as.factor(data2.test$SalePriorityClass)

svm_model2 <- svm(data2.train.attributes, data2class.train)
svm.original.pred2 <- predict(svm_model2, data2.test.attributes)

tab <- table(pred = svm.original.pred2, true = data2class.test)
tab
#          true
# pred   1   2   3
#    1   7   1   0
#    2  27  52   1
#    3   0   1 300
classification_error_original2 <- 1- sum(svm.original.pred2 == data2class.test)/length(svm.original.pred2)
classification_error_original2
# 0.07712082

svm_tune2 <- tune(svm, train.x = data2.train.attributes, train.y = data2class.train,
                  kernel="radial", ranges = list(cost=10^-1:2), gamma=c(.5,1,2))

svm_tune2$best.model
# Call:
#   best.tune(method = svm, train.x = data2.train.attributes, train.y = data2class.train, ranges = list(cost = 10^-1:2), 
#             kernel = "radial", gamma = c(0.5, 1, 2))
# 
# 
# Parameters:
#   SVM-Type:  C-classification 
# SVM-Kernel:  radial 
# cost:  1.1 
# gamma:  0.5 1 2 
# 
# Number of Support Vectors:  718

svm_model2_best <- svm(data2.train.attributes, data2class.train, kernel="radial", cost=1.1, gamma=c(0.5,1,2))

svm.pred2 <- predict(svm_model2_best, data2.test.attributes)

tab <- table(pred = svm.pred2, true = data2class.test)
tab
#          true
# pred   1   2   3
#    1   6   1   0
#    2  28  53   1
#    3   0   0 300
classification_error2 <- 1- sum(svm.pred2 == data2class.test)/length(svm.pred2)
classification_error2
# 0.07712082

##############################
##SVM Tuned Model 3
##############################
library(caret)
## SVM Model 3
data2 <- data1
data2$"index" <- NULL
data2$"ProductID" <- NULL
data2$"Description" <- NULL
data2$"SalePriorityClass" <- as.factor(data2$"SalePriorityClass")

data2.train <- data1[data1$index<3489,]
data2.test <- data1[data1$index>=3489,]

# cbind(TransactionFreq, Customers, MeanEarningPerTransaction)

drops <- c("SalePriorityClass","Description", "index", "TotalQuantity", "MeanQuantityPerCustomer",
           "UnitPrice", "MeanQuantityPerTransaction", "ProductID")
data2.train.attributes <- data2.train[,!(names(data2.train) %in% drops)]
data2.test.attributes <- data2.test[,!(names(data2.test) %in% drops)]
names(data2.train.attributes)
str(data2.train.attributes)
str(data2.test.attributes)

data2class.train <- as.factor(data2.train$SalePriorityClass)
data2class.test <- as.factor(data2.test$SalePriorityClass)

svm_model3 <- svm(data2.train.attributes, data2class.train)
svm.original.pred3 <- predict(svm_model3, data2.test.attributes)

tab <- table(pred = svm.original.pred3, true = data2class.test)
tab
#         true
# pred   1   2   3
#    1   7   1   0
#    2  27  52   1
#    3   0   1 300
classification_error_original3 <- 1- sum(svm.original.pred3 == data2class.test)/length(svm.original.pred3)
classification_error_original3
# 0.07712082

svm_tune3 <- tune(svm, train.x = data2.train.attributes, train.y = data2class.train,
                  kernel="radial", ranges = list(cost=10^-1:2), gamma=c(.5,1,2))

svm_tune3$best.model

# Call:
#   best.tune(method = svm, train.x = data2.train.attributes, train.y = data2class.train, ranges = list(cost = 10^-1:2), 
#             kernel = "radial", gamma = c(0.5, 1, 2))
# 
# 
# Parameters:
#   SVM-Type:  C-classification 
# SVM-Kernel:  radial 
# cost:  1.1 
# gamma:  0.5 1 2 

svm_model3_best <- svm(data2.train.attributes, data2class.train, kernel="radial", cost=1.1, gamma=c(0.5,1,2))

svm.pred3 <- predict(svm_model3_best, data2.test.attributes)

tab <- table(pred = svm.pred3, true = data2class.test)
tab
#           true
# pred   1   2   3
#    1   6   1   0
#    2  28  53   1
#    3   0   0 300
classification_error3 <- 1- sum(svm.pred3 == data2class.test)/length(svm.pred3)
classification_error3
# 0.07712082

#SVM Model performance
errors <- c(classification_error1*100, classification_error2*100, classification_error3*100)
barplot(errors, ylim = c(6,11), beside=TRUE, xpd = FALSE, ylab = "Error %", names.arg=c(1,2,3), 
        xlab = "SVM Model Number")
text(0.70, 8, format(classification_error1*100))
text(1.85, 8, format(classification_error2*100))
text(3.00, 8, format(classification_error3*100))
