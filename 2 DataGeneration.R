#
# STEP 2: DataGeneration - MD Naseem Ashraf
# UPDATED: Using Entire DS from New DS Files.
#
rm(list=setdiff(ls(), "finaldataset"))
library("openxlsx")
options(java.parameters = "-Xmx2048m")
setwd("C:/Users/Naseem Ashraf/Desktop/Fall 16/DM Project sets")
library(plyr)

orderedallproducts <- read.xlsx("orderedallproductsNEW.xlsx", sheet = 1,startRow = 1, colNames = TRUE)
colnames(orderedallproducts) <- c("ProductID","Description","TransactionFreq","TotalQuantity","Customers","MeanQuantityPerTransaction","MeanQuantityPerCustomer")

transfreqsummary <- summary(orderedallproducts$TransactionFreq)

finaldataset <- read.xlsx("finaldatasetNEW.xlsx", sheet = 1,startRow = 1, colNames = TRUE)
colnames(finaldataset) <- c("ProductID","Description","TransactionFreq","TotalQuantity","Customers","MeanQuantityPerTransaction","MeanQuantityPerCustomer", "UnitPrice", "MeanEarningPerTransaction")

cutomerssummary <- summary(finaldataset$Customers)
#summary(finaldataset$Customers)
#UPDATED
## Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 1.0     9.0    36.0    69.2    94.0   881.0 

meanQuantityPerTransactionsummary <- summary(finaldataset$MeanQuantityPerTransaction)
#summary(finaldataset$MeanQuantityPerTransaction)
#UPDATED
## Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 1.00     3.79     7.43    32.69    13.34 81000.00 

meanEarningPerTransactionsummary <- summary(finaldataset$MeanEarningPerTransaction)
#summary(finaldataset$MeanEarningPerTransaction)
#UPDATED
## Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.00      9.47     15.01     71.72     24.03 168500.00 

classifyPrioritySale <- function(currentid){ #MAIN CLASSIFICATION LOGIC
  k = 0
  
  #TransactionFreq
  tfreq <- finaldataset$TransactionFreq[finaldataset$ProductID==currentid]
  #Customers
  tcust <- finaldataset$Customers[finaldataset$ProductID==currentid]
  #Earnings
  meanEarningPerTransaction <- finaldataset$UnitPrice[finaldataset$ProductID==currentid] * finaldataset$MeanQuantityPerTransaction[finaldataset$ProductID==currentid]
  
  if((tfreq >= transfreqsummary[4]) & (tfreq < transfreqsummary[5])) #Medium TransactionFreq
  {
    if((tcust >= cutomerssummary[5]) & (tcust < cutomerssummary[6])) #High Number of Customers
    {
      #High Earning Per Transaction
      if((meanEarningPerTransaction >= meanEarningPerTransactionsummary[5]) &
         (meanEarningPerTransaction < meanEarningPerTransactionsummary[6]))
      {
        k = 1
      }
      else #Less Earning Per Transaction
      {
        k = 2
      }
    }
    else #Less Number of Customers
    {
      #High Earning Per Transaction
      if((meanEarningPerTransaction >= meanEarningPerTransactionsummary[5]) &
         (meanEarningPerTransaction < meanEarningPerTransactionsummary[6]))
      {
        k = 3
      }
      else #Less Earning Per Transaction
      {
        k = 3
      } 
    }
  }
  
  if(tfreq >= transfreqsummary[5]) #High TransactionFreq
  {
    if((tcust >= cutomerssummary[5]) & (tcust < cutomerssummary[6])) #High Number of Customers
    {
      #High Earning Per Transaction
      if((meanEarningPerTransaction >= meanEarningPerTransactionsummary[5]) &
         (meanEarningPerTransaction < meanEarningPerTransactionsummary[6]))
      {
        k = 1
      }
      else #Less Earning Per Transaction
      {
        k = 2
      }
    }
    else #Less Number of Customers
    {
      #High Earning Per Transaction
      if((meanEarningPerTransaction >= meanEarningPerTransactionsummary[5]) &
         (meanEarningPerTransaction < meanEarningPerTransactionsummary[6]))
      {
        k = 2
      }
      else #Less Earning Per Transaction
      {
        k = 3
      } 
    }
  }
  
  if(k==0)
  {
    k = 3
  }
  
  return(k)
}

clsin <- c()
index <- 0
dim(finaldataset) #3877    9
range(finaldataset$ProductID) #1 3877

for (pid in finaldataset$ProductID) #3877 Total Products
{
  index = index + 1
  if(classifyPrioritySale(pid)==1) #198-- High Priority products
  {
    clsin[index] <- 1
  }
  if(classifyPrioritySale(pid)==2) #972-- Medium Priority products
  {
    clsin[index] <- 2
  }
  if(classifyPrioritySale(pid)==3) #972-- Low Priority products
  {
    clsin[index] <- 3
  }
}
rm(index)
clsin <- as.factor(clsin)
levels(clsin)
summary(clsin)
# 1    2    3 
# 327  665 2885

finaldataset$"SalePriorityClass" <- clsin
rm(clsin)

newproductdataset <- finaldataset
write.xlsx(newproductdataset, "newproductdatasetclassifiedNEW.xlsx") ##Final Dataset with Class Attribute

summary(newproductdataset$SalePriorityClass)
