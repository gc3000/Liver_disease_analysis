# Logistic regression model for Liver Disease

This paper builds a logistic regression model and decision tree to predict liver disease in patients. The dataset was obtained from the
UCI machine learning repository. The dataset was cleaned and transformed into an 80/20 training test split. A correlation matrix was
used to determine if there was collinearity among variables and then particular variables were selected to be a part of the candidate
models. The overall accuracy of the final logistic regression model was 71%. The final accuracy of the classification tree was 68%. To
improve future model building on the same dataset or on similar future datasets, principal component analysis could be used as a way
to do feature selection. Using L1 or L2 regularization is also suggested to reduce variance in the model and improve overall
generalization of the model. Taking an algorithmic approach to selecting the decision threshold for logistic regression such as finding
the optimal threshold using a ROC or precision-recall curve can be used for future analysis. Implementing an ensemble technique like
random forests is also suggested instead of a classification tree to improve overall accuracy of the models.
