# R-code-Classifiers
The aim of this repository is to select important features of a dataset based on accuracy of the classifiers. The R code compares the performance metrics between logistic regression, SVM, Naive Bayes, Knn and random forest classifers in a 10 fold cross validation loop. The features are iteratively selected in a forward selection manner and everytime a new feature(next best feature selected based on regsubsets)  is added the accuracy of the classifers are calculated. Finally a graph is plotted at the end depicting the accuracy for every classifier at every feature count. This will help us understand the important features as well as the tradeoff between accuracy and the number of features. 

