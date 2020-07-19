# 🏀 Forecasting Outcomes of NCAA Basketball Games 🏀

I’ve always been a pretty avid professional/college basketball fan, while also being interested in the application of data analytics in the sports world. 

With the March Madness Tournament coming up this year, I wanted to apply the concepts we learned in STAT 301-2 Data Science II to try to predict the March Madness Tournament. Predicting the tournament comes down to predicting outcomes of college basketball games given historical performance statistics. Because the outcome of a basketball game is binary, i.e. win or lose, this is a classification problem. 

Overall, this final report includes a practical application and assessment of the model methodologies I learned in class. I have also include a brief Exploratory Data Analysis (EDA) in the Appendix.

The model methodologies I used are as the following:

- Forward Stepwise Section
- Backward Stepwise Selection
- Logistic Regression
- Linear Discriminant Analysis
- Quadratic Disciminant Analysis
- K-Nearest Neighbors
- Ridge Regression
- Lasso
- PCR Regression
- PLS Regression

Out of all the models I ran, the PCR model with 12 principal components was the best model with a test error of 0.1674. The ridge and lasso models came in at a close second and third, respectively. On the other end, the KNN models I ran, in general, performed the worst (i.e. had the highest test errors).

Link to Final Report: https://rpubs.com/nimondong/march-madness-predictions

This project was coded with ❤️ and R