#
# STEP 2.5: Data Exploration - MD Naseem Ashraf
#

#library(class)
library("openxlsx")
options(java.parameters = "-Xmx2048m")
setwd("C:/Users/Naseem Ashraf/Desktop/Fall 16/DM Project sets")

newproductdataset <- read.xlsx("newproductdatasetclassifiedNEW.xlsx", sheet = 1,startRow = 1, colNames = TRUE)
colnames(newproductdataset) <- c("ProductID","Description","TransactionFreq","TotalQuantity","Customers","MeanQuantityPerTransaction","MeanQuantityPerCustomer", "UnitPrice","MeanEarningPerTransaction","SalePriorityClass")

attach(newproductdataset)
names(newproductdataset)
summary(newproductdataset)

## Target Analysis
class <- as.factor(newproductdataset$SalePriorityClass)
levels(class)

counts1 <- table(class)
barplot(counts1, col = c("red","orange","green"), ylab = "No. of Products" ,
                        names.arg=c("High [1]","Medium [2]","Low [3]"), 
                        xlab = "Product Sale Priority",  ylim = c(0,3500), beside=TRUE, xpd = FALSE)
text(0.70, 500, counts1[1])
text(1.85, 850, counts1[2])
text(3.10, 3075, counts1[3])

k <- newproductdataset
k$Description <- NULL
k$SalePriorityClass <- as.factor(k$SalePriorityClass)

## Attribute Correlation Visualization
#install.packages("corrplot")
library(corrplot)
correlations <- cor(k[,1:8])
corrplot(correlations, method="circle")

## Attribute Pairwise Corellation Plots
pairs(SalePriorityClass~., data= k, col=k$SalePriorityClass)

## Additional Information About Target Relation to Features
d1 <- subset(newproductdataset, SalePriorityClass<2, select = c( "Description","TransactionFreq",
                                                                 "TotalQuantity","Customers",
                                                                 "MeanQuantityPerTransaction",
                                                                 "MeanQuantityPerCustomer","UnitPrice",
                                                                 "MeanEarningPerTransaction",
                                                                 "SalePriorityClass" ))
d1$SalePriorityClass <- as.factor(d1$SalePriorityClass)
d1$Description <- NULL
d2 <- subset(newproductdataset, (SalePriorityClass==2), select = c( "Description","TransactionFreq",
                                                                 "TotalQuantity","Customers",
                                                                 "MeanQuantityPerTransaction",
                                                                 "MeanQuantityPerCustomer","UnitPrice",
                                                                 "MeanEarningPerTransaction",
                                                                 "SalePriorityClass" ))
d2$SalePriorityClass <- as.factor(d2$SalePriorityClass)
d2$Description <- NULL
summary(d2$SalePriorityClass)
d3 <- subset(newproductdataset, (SalePriorityClass==3), select = c( "Description","TransactionFreq",
                                                                 "TotalQuantity","Customers",
                                                                 "MeanQuantityPerTransaction",
                                                                 "MeanQuantityPerCustomer","UnitPrice",
                                                                 "MeanEarningPerTransaction",
                                                                 "SalePriorityClass" ))
d3$SalePriorityClass <- as.factor(d3$SalePriorityClass)
d3$Description <- NULL
summary(d3$SalePriorityClass)

## IMPORTANT: Parallel-Plot of Class Attributes
library(lattice)
parallelplot(~k[2:8] | SalePriorityClass, k)

## Attribute by Class Density Plots
## IMPORTANT: Attribute by Class Box and Whisker Plots
#install.packages("caret")
library(caret)
x <- k[,c(2,3,4,7)]
y <- k[,9]
scales <- list(x=list(relation="free"), y=list(relation="free"))
#featurePlot(x=x, y=y, plot="density", scales=scales, col=c("Red","Orange","Green"))
featurePlot(x=x, y=y, plot="box", scales=scales, col=c("Red","Orange","Green"))

#MeanEarningPerTransaction
range(d1$MeanEarningPerTransaction)
range(d2$MeanEarningPerTransaction)
range(d3$MeanEarningPerTransaction)
#UnitPrice
range(d1$UnitPrice)
range(d2$UnitPrice)
range(d3$UnitPrice)
#MeanQuantityPerCustomer
range(d1$MeanQuantityPerCustomer)
range(d2$MeanQuantityPerCustomer)
range(d3$MeanQuantityPerCustomer)
#MeanQuantityPerTransaction
range(d1$MeanQuantityPerTransaction)
range(d2$MeanQuantityPerTransaction)
range(d3$MeanQuantityPerTransaction)
#Customers
range(d1$Customers)
range(d2$Customers)
range(d3$Customers)
#TotalQuantity
range(d1$TotalQuantity)
range(d2$TotalQuantity)
range(d3$TotalQuantity)
#Transaction Frequencies
range(d1$TransactionFreq)
range(d2$TransactionFreq)
range(d3$TransactionFreq)

## Outlier Notes
d3[d3$MeanEarningPerTransaction==max(d3$MeanEarningPerTransaction),]
# TransactionFreq 1
# TotalQuantity 80995
# Customers 1
# MeanQuantityPerTransaction 80995
# MeanQuantityPerCustomer 80995
# UnitPrice 2.08
# MeanEarningPerTransaction 168469.6
# SalePriorityClass 3

## Potential Model Exploration
## Feature/Predictor Importance
set.seed(42)
library(mlbench)
library(caret)

kll <- k
kll$ProductID <- NULL
#Set Control
control <- trainControl(method="repeatedcv", number=10, repeats=3)
## Full Model Check
model <- train(SalePriorityClass~., data=kll, method="lvq", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)

# Recursive Feature Elimination (RFE)
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(kll[,1:7], kll[,8], sizes=c(1:7), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))

# Recursive feature selection
# 
# Outer resampling method: Cross-Validated (10 fold) 
# 
# Resampling performance over subset size:
#   
#   Variables Accuracy  Kappa AccuracySD  KappaSD Selected
# 1   0.6570 0.1555   0.019225 0.047001         
# 2   0.9969 0.9924   0.003168 0.007728         
# 3   0.9987 0.9969   0.001358 0.003314         
# 4   0.9990 0.9975   0.001330 0.003251        *
# 5   0.9985 0.9962   0.001332 0.003257         
# 6   0.9982 0.9956   0.001740 0.004248         
# 7   0.9979 0.9950   0.002031 0.004957         
# 
# The top 4 variables (out of 4):
#   MeanEarningPerTransaction, Customers, TransactionFreq, TotalQuantity


# ## Attributes/Predictors/Features Analysis
# hist(ProductID)
# hist(TransactionFreq)
# hist(TotalQuantity)
# hist(Customers)
# hist(MeanQuantityPerTransaction)
# hist(MeanQuantityPerCustomer)
# hist(UnitPrice)
# hist(MeanEarningPerTransaction)
# 
# ## Some important Corellations
# cor(TransactionFreq, MeanQuantityPerTransaction) #0.04369588
# cor(TransactionFreq, MeanQuantityPerCustomer) #0.1437518
# cor(TransactionFreq, MeanEarningPerTransaction) #0.1525453
# cor(MeanQuantityPerTransaction, MeanEarningPerTransaction) #0.5211551
# cor(MeanQuantityPerCustomer, MeanEarningPerTransaction) #0.525997
