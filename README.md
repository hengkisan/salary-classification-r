# Exploring Salary Determinants: A Machine Learning Analysis of Demographic and Work-Related Factors

## 📚 Introduction
The study focuses on understanding the factors that influence individual salaries using machine learning techniques. Various algorithms such as Random Forest, Logistic Regression, and Support Vector Machine were implemented and evaluated using the "Adult" dataset.

## 💻 Tools and Analysis
The "Adult" dataset contains demographic and work-related information such as age, gender, education, occupation, and native country. The dataset was pre-processed to handle missing values, inconsistencies, duplicates, and categorical encoding. Additionaly, it was balanced using SMOTE and one-hot encoding was performed for Logistic Regression. 

![image](https://github.com/hengkisan/salary-classification-r/assets/122197570/ea85c1c2-9ebd-4d5c-8e66-c9f5209a57bc)

The study employed Random Forest, Logistic Regression, and Support Vector Machine (SVM) machine learning models to classify salary levels based on demographic and work-related attributes. Random Forest was chosen for its non-linearity handling, Logistic Regression for binary classification, and SVM for capturing non-linear patterns. Evaluation involved metrics like accuracy, recall, precision, and F1-Score, along with ROC curves and AUC values for overall performance assessment. These models were trained and tested using an 80%-20% train-test split, ensuring reliable predictions on unseen data with diverse feature sets.

![image](https://github.com/hengkisan/salary-classification-r/assets/122197570/ce0f1aa7-5bd1-438e-af03-3f6e6da94238)

## 💡 Conclusion
The findings of the study underscored the superior performance of the Random Forest algorithm compared to Logistic Regression and Support Vector Machine in classifying individuals into distinct salary categories.

Random Forest highlighted the relative importance of various features based on Gini Importance. Logistic Regression, on the other hand, quantified the impact of each feature through feature coefficients. The variables considered in the study for predicting salary levels include demographic attributes like age, gender, education level, marital status, native country, occupation type, and hours worked per week. These variables provide a comprehensive view of an individual's background, qualifications, and professional status, which are crucial factors influencing salary levels in the dataset.
