# Portfolio

## FacialRecognition.ipynb
The point of this small exercise was to familiarize myself with how to do facial recoginition in Python. Throughout the below notebook, I built a face recognition system with the help of the face recognition library from the dlib package.

## Predicting Earthquake Damage.ipynb
I used Support Vector Machine (SVM), Random Forest, and k-Nearest Neighbors (kNN) models to predict building damage ratings from the 2015 Gorkha earthquake. This major earthquake was near the Gorkha district of Gandaki Pradesh, Nepal that reached a magnitude of 7.6 and impacted almost one-third of the population. Overall, the SVM model performed the best as a whole. The random forest model was better than kNN in terms of accuracy, but not in terms of precision. kNN seemed to have a lot more mis-classifications of damage grade than my random forest model did when I looked at the confusion matrices.

## LinReg.R
A linear regression model I made using some AirBnB data. Worth noting: I had to log the data here as the data was not normal based on the histograms and QQ plots that were created to check the distribution. Data that doesn't follow a normal distribution causes problems for linear regression models. In order to work around this, transforming the data to make it normal helps. I also included a lot of code centered around checking the 5 assumptions for linear regression models.

## GradDesc.R
I coded my own gradient descent algorithm. If you go through the whole thing and compare the coefficients with the output from the lm() function, you'll see that they match!

## LMER MPLS.R
Longitudinal data are a special case of repeated measures data, with time (or duration) representing the dimension over which the measurements are collected. Analyzing changes over time must be approached with more advanced statistical methods since the data are not independent, and often involve dropout and missing assessments. Thus, I compared 6 different Linear Mixed Effects Regression models to see which one was the most accurate in predicting reading score over time for each child as they progress through grades 5-8. I used AICc as my criterion to choose the most performant model.

## EDA.R
This is some exploratory data analysis I performed on AirBnB data. I created several bar graphs as well as scatterplots.
