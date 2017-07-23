# B2B-Retail-Classifier

Data Mining project made in R for CS-686 in the Fall of 2016 for University of San Francisco, Data Mining class. 

This project deals with a UK-based and registered non-store online retail company that mainly sells unique all-occasion gifts, internationally. 

## More about the Data Set
This data set is a transnational data set which contains all the transactions occurring between 01/12/2010 and 09/12/2011 for a UK-based and registered non-store online retail.The company mainly sells unique all-occasion gifts. Many customers of the company are wholesalers.  

Raw Data [Source](http://archive.ics.uci.edu/ml/datasets/Online+Retail) | Source: [UCI Machine Learning Repository](http://archive.ics.uci.edu/ml/index.php) | Format: .xlsx | Size: 22.6 MB

## More about the Project
The data set is cleaned, organized and labelled to create the required sale priority classes. This is then used to train and build a "product sale priority" classifier. 

### The steps are:

1. Preprocessing 
2. Data Generation - To create the initial class label for sale priority for all dataset.
3. Data Exploration - To understand the attributes of data set and gauge possible data mining models.
4. kNN Classifier
5. LDA Classifier
6. SVM Classifier

## About the Classifiers
This classifer can then be used to make several decisions like deciding which products to discontinue manufacture depending on their sales or which products to put on sale or offers during which months etc. The primary aim of the classifier is to take into consideration several factors regarding the product including sale quantity, price, number of customers etc to determine their sale priority.

For example; A high demand product which is only ordered by a single or very few retailers might not be a good product to keep manufacturing on the long term, as the manufacturer begins to depend on those specific retailers to sell their specific product and might want to focus on products sold through several retailers. 

This classifier can also be used on new products to understand their priority in market and how well are they doing in the market.  Furthermore, the classifier project uses several classification models and multi-classification methods (kNN, LDA and SVM) to compare them as per their accuracy. 

Also, included is a Project Report on the Classifier and it's performance on the dataset.
