#
# STEP 1: Preprocessing - MD Naseem Ashraf
# Updated: Also covers low/very-low transaction frequencies
#

#Setup Environment & Workspace
rm(list=ls())
#install.packages('openxlsx')
#install.packages('plyr')
library("openxlsx")
options(java.parameters = "-Xmx2048m")
setwd("C:/Users/Naseem Ashraf/Desktop/Fall 16/DM Project sets")

##Reading Input Data
mydf <- read.xlsx("OnlineRetail.xlsx", sheet = 1,startRow = 2, colNames = TRUE)
colnames(mydf) <- c("InvoiceNo","StockCode","Description","Quantity","InvoiceDate","UnitPrice","CustomerID","Country")

initialStatistics <- function(mydf){
  nrow(mydf) #541,908 Total Transactions
  head(mydf[!complete.cases(mydf[, c("InvoiceNo","StockCode","Description","Quantity","InvoiceDate","UnitPrice","CustomerID","Country")]), ])
  missingDataCount <- nrow(mydf[!complete.cases(mydf[, c("InvoiceNo","StockCode","Description","Quantity","InvoiceDate","UnitPrice","CustomerID","Country")]), ])
  missingInvoiceNoCount <- nrow(mydf[!complete.cases(mydf[, c("InvoiceNo")]), ]) #0 missing InvoiceNo
  missingStockCodeCount <- nrow(mydf[!complete.cases(mydf[, c("StockCode")]), ]) #0 missing StockCode
  missingNameCount <- nrow(mydf[!complete.cases(mydf[, c("Description")]), ]) #1454 missing Description
  missingQuantityCount <- nrow(mydf[!complete.cases(mydf[, c("Quantity")]), ]) #0 missing Quantity
  missingInvoiceDateCount <- nrow(mydf[!complete.cases(mydf[, c("InvoiceDate")]), ]) #0 missing InvoiceDate
  missingUnitPriceCount <- nrow(mydf[!complete.cases(mydf[, c("UnitPrice")]), ]) #0 missing UnitPrice
  missingCustomerIDCount <- nrow(mydf[!complete.cases(mydf[, c("CustomerID")]), ]) #135080 missing CustomerID
  missingCountryCount <- nrow(mydf[!complete.cases(mydf[, c("Country")]), ]) #0 missing Country
  # NA/Missing Counts = 13,5080
}

initialStatistics(mydf)
nrow(mydf) #541,908 Transactions
dim(mydf)

## Filtering Transactions of Missing Data and Redundancies
# COMPLETE-CASES
mydf <- mydf[complete.cases(mydf),]
nrow(mydf) #406,828 Complete Case Transactions
# UNIQUE-CASES
mydf <- unique(mydf) #
nrow(mydf) #401,603 Unique Case Transactions
# LOST
dim(mydf)
mydf$'tindex'<- seq.int(nrow(mydf))
dim(mydf)
lost <- subset(mydf[with(mydf, order(Quantity)),], Quantity <0, select = c(tindex, InvoiceNo, StockCode, Description, Quantity, InvoiceDate, UnitPrice, CustomerID, Country))
dim(lost) #8872 Cancelled or Lost Product Transactions
# FILTERING
mydf <- mydf[ ! mydf$tindex %in% lost$tindex, ]
dim(mydf) #392731  9
mydf$tindex <- NULL
dim(mydf) #392731  9
nrow(mydf) ##UPDATED: 392,731 Transactions

###PRODUCTS TABLE GENERATION [ACTUAL DATASET]
###Generating ProductID for Products
newdf <- transform(mydf, ProductID = match(mydf$'Description', unique(mydf$'Description')))
all.equal(mydf,newdf)  ## Only first 8 Column Names are left uncoded.
rm(mydf)
#head(newdf[with(newdf,order(ProductID)),])
#dim(newdf)
#nrow(unique(newdf))

###TRANSACTIONS SUMMARY TABLE
# ProductStat[PID, Description, TotalQuantity, TransactionFreq, UnitPrice]
library(plyr)

getTotalQuantity <- function(pid, npar = FALSE, print = FALSE)
{
  totalQuantity <- sum(subset(newdf, ProductID == pid, select = c(Quantity)))
  return(totalQuantity)
}

getTotalCustomers <- function(pid, npar = FALSE, print = FALSE)
{
  return(length(unique(newdf$CustomerID[newdf$ProductID==pid])))
}

getMeanQuantityPerTransaction <- function(pid, npar = FALSE, print = FALSE)
{
  meanQuantity <- mean(subset(newdf, ProductID == pid, select = c(Quantity))$Quantity)
  return(meanQuantity)
}

range(newdf$ProductID) #1-3877
##UPDATED
productstatdf <- count(subset(newdf, ProductID = 1:3877, select = c(ProductID, Description, UnitPrice)), c('ProductID', 'Description'))
colnames(productstatdf)[3] <- "TransactionFreq"
#head(productstatdf)

totQuantity <- c()
totCustomers <- c()
meanQuantityPerTransaction <- c()
for (netproductcount in 1:3877)
{
  totQuantity[netproductcount] <- getTotalQuantity(netproductcount)
  totCustomers[netproductcount] <- getTotalCustomers(netproductcount)
  meanQuantityPerTransaction[netproductcount] <- getMeanQuantityPerTransaction(netproductcount)
}

productstatdf$"TotalQuantity" <- totQuantity
productstatdf$"Customers" <- totCustomers
productstatdf$"MeanQuantityPerTransaction" <- meanQuantityPerTransaction

meanQuantityPerCustomer <- c()
for (netproductcount in 1:3877)
{
  meanQuantityPerCustomer[netproductcount] <- productstatdf$"TotalQuantity"[productstatdf$ProductID==netproductcount]/productstatdf$"Customers"[productstatdf$ProductID==netproductcount]
}
productstatdf$"MeanQuantityPerCustomer" <- meanQuantityPerCustomer
rm(totCustomers, totQuantity, lost, meanQuantityPerCustomer, meanQuantityPerTransaction)

plotProductsInitial <- function(){ 
  
  head(productstatdf)
  ##Histogram of productstatdf attributes
  for (n in c(1,3,4,5,6,7))
  {
    hist(productstatdf[,n], xlab = colnames(productstatdf)[n], xlim= c(100,500), breaks = 100)
    hist(productstatdf[,n], xlab = colnames(productstatdf)[n])
  }
  
  hist(productstatdf[,3], xlab = colnames(productstatdf)[3], breaks=100, xlim = c(0,2015), ylab = "No. of Different Products")
  max(productstatdf[,3])
  
  hist(productstatdf[,4], xlab = colnames(productstatdf)[4], breaks=100, xlim = c(0,81000), ylab = "No. of Different Products")
  max(productstatdf[,4])
  
}

###Splitting Dataset into High Frequency, Medium Frequency and Low Frequency Transactions
orderedallproducts <- productstatdf[with(productstatdf, order(-TransactionFreq, TotalQuantity, ProductID)),]
orderdProductsStats(orderedallproducts)

orderdProductsStats <- function(orderedallproducts){
  head(orderedallproducts)
  tail(orderedallproducts)
  max(orderedallproducts$ProductID)
  summary(orderedallproducts$TransactionFreq)
}

transfreqsummary <- summary(orderedallproducts$TransactionFreq)
#summary(orderedallproducts$TransactionFreq)
## UPDATED
## Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 1.0    10.0    43.0   101.3   127.0  2015.0 

lowfreqproducts <- subset(orderedallproducts, (ProductID = 1:3877)&(TransactionFreq < transfreqsummary[4]), select = c(ProductID, Description, TransactionFreq, TotalQuantity, Customers, MeanQuantityPerTransaction, MeanQuantityPerCustomer))
#range(lowfreqproducts$TransactionFreq)
#hist(lowfreqproducts$TransactionFreq)
#dim(lowfreqproducts) ##2707 Products <101.3

midfreqproducts <- subset(orderedallproducts, (ProductID = 1:3877)&(TransactionFreq > transfreqsummary[4])&(TransactionFreq < transfreqsummary[5]), select = c(ProductID, Description, TransactionFreq, TotalQuantity, Customers, MeanQuantityPerTransaction, MeanQuantityPerCustomer))
#range(midfreqproducts$TransactionFreq) #102-126
#hist(midfreqproducts$TransactionFreq)
#dim(midfreqproducts) ##198 Products ~ 774

highfreqproducts <- subset(orderedallproducts, (ProductID = 1:3877)&(TransactionFreq >= transfreqsummary[5]), select = c(ProductID, Description, TransactionFreq, TotalQuantity, Customers, MeanQuantityPerTransaction, MeanQuantityPerCustomer))
#range(highfreqproducts$TransactionFreq) #127-2015
#hist(highfreqproducts$TransactionFreq)
#dim(highfreqproducts) ##972 Products

#UPDATED
finaldataset <- orderedallproducts
#finaldataset <- orderedallproducts[ ! orderedallproducts$ProductID %in% lowfreqproducts$ProductID, ]
##UPDATED
##dim(finaldataset) #3877 products and 7 Attributes
#dim(finaldataset) #1170 Products [198 + 972 + ]

#Reattach UnitPrice of each product
upricein <- c()
earningpertransactions <- c()
index <- 0
for (currentid in finaldataset$ProductID) #3877 Total Products
{
  index = index + 1
  upricein[index] <- newdf$UnitPrice[newdf$ProductID==currentid]
  earningpertransactions[index] <- newdf$UnitPrice[newdf$ProductID==currentid] * finaldataset$MeanQuantityPerTransaction[finaldataset$ProductID==currentid]
}
rm(index)
finaldataset$"UnitPrice" <- upricein
finaldataset$"MeanEarningPerTransaction" <- earningpertransactions
rm(upricein, earningpertransactions)

write.xlsx(orderedallproducts, "orderedallproductsNEW.xlsx")
write.xlsx(finaldataset, "finaldatasetNEW.xlsx")
