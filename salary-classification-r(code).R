library(ggplot2)
library(DataExplorer)
library(mice)
library(corrplot)

set.seed(123)

df <- read.csv('/salary-classification-r(dataset).csv', sep = ';')
df[] <- lapply(df, trimws)

num.var <- c('age', 'capitalgain', 'capitalloss', 'hoursperweek')
cat.var <- c('workclass', 'education', 'education.num', 'marital.status',
             'occupation', 'relationship', 'race', 'sex', 'native.country', 'class')
corr.var<-c('age', 'capitalloss', 'hoursperweek','class_num')

for (i in num.var){
  df[[i]] <- as.numeric(df[[i]])
  print(class(df[[i]]))
}

for (i in cat.var){
  print(class(df[[i]]))
}

#data preparation 
#drop fnlwgt
df2 <- subset(df, select = -fnlwgt)

str(df2)
summary(df2)
library(ggplot2)

# distribution of age vs class
ggplot(df, aes(x = age, fill = class)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribution of Age by Class",
       x = "Age",
       y = "Density") +
  scale_fill_manual(values = c("<=50K" = "blue", ">50K" = "red"))
  
ggplot(df, aes(x = age)) +
  geom_density(alpha=0.5,fill = "green", color = "black") +
  labs(title = "Distribution of Age",
       x = "Age",
       y = "Density")

#log transform age
df2$age <- log(df2$ag)

ggplot(df2, aes(x = age)) +
  geom_density(alpha=0.5,fill = "green", color = "black") +
  labs(title = "Distribution of Age",
       x = "Age",
       y = "Density")

ggplot(df, aes(x = capitalgain)) +
  geom_histogram(binwidth = 500, fill = "green", color = "black") +
  labs(title = "Histogram of Capital Gain",
       x = "Capital Gain",
       y = "Frequency")

ggplot(df, aes(x = capitalloss)) +
  geom_histogram(binwidth = 100, fill = "red", color = "black") +
  labs(title = "Histogram of Capital Loss",
       x = "Capital Loss",
       y = "Frequency")

ggplot(df, aes(x = hoursperweek)) +
  geom_histogram(binwidth = 5, fill = "green", color = "black") +
  labs(title = "Histogram of Hours per Week",
       x = "Hours per Week",
       y = "Frequency")

ggplot(df, aes(x = hoursperweek, fill = class)) +
  geom_histogram(binwidth = 5,alpha = 0.5) +
  labs(title = "Distribution of Hour per week by Class",
       x = "Hours per week",
       y = "Frequency") +
  scale_fill_manual(values = c("<=50K" = "blue", ">50K" = "red"))

correlation_matrix <- cor(df6[corr.var])
corrplot(correlation_matrix, method = "color")


#CATEGORICAL VARIABLES
as.data.frame(table(df2$workclass))
as.data.frame(table(df2$education))
as.data.frame(table(df2$marital.status))
as.data.frame(table(df2$occupation))
as.data.frame(table(df2$relationship))
as.data.frame(table(df2$race))
as.data.frame(table(df2$sex))
as.data.frame(table(df2$native.country))
as.data.frame(table(df2$class))

#change "?" to become missing values
df3 <- df2

df3[df3 == "?"] <- NA
plot_missing(df3)
#md.pattern(df3)

#1) workclass
as.data.frame(table(df3$workclass))
nrow(df3[is.na(df3$workclass),]) #sum of missing values

ggplot(df3, aes(x = workclass, fill = class)) +
  geom_bar(position = "stack") +
  labs(title = "Stacked Bar Chart of <=50K and >50K by Workclass Level",
       x = "Workclass", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")

#2)education
as.data.frame(table(df3$education))
nrow(df3[is.na(df3$education),])

#mapping
df3$education <- ifelse(df3$education %in% c("10th", "11th", "12th"), "some-HS",
                 ifelse(df3$education %in% c("1st-4th", "5th-6th"), "some-Primary",
                 ifelse(df3$education %in% c("7th-8th", "9th"), "some-JH",
                 ifelse(df3$education %in% c("Assoc-acdm", "Assoc-voc"), "Assoc-acdm/voc",
                 df3$education))))

ggplot(df3, aes(x = education, fill = class)) +
  geom_bar(position = "stack") +
  labs(title = "Stacked Bar Chart of <=50K and >50K by Education Level",
       x = "Education Level", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")

#3)marital.status
as.data.frame(table(df3$marital.status))
nrow(df3[is.na(df3$marital.status),])

df3$marital.status <- ifelse(df3$marital.status %in% c("Married-AF-spouse", "Married-civ-spouse", "Married-spouse-absent"), "Married", df3$marital.status)

ggplot(df3, aes(x = marital.status, fill = class)) +
  geom_bar(position = "stack") +
  labs(title = "Stacked Bar Chart of <=50K and >50K by Marital Status Level",
       x = "Status", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")

#4) occupation
as.data.frame(table(df3$occupation))
nrow(df3[is.na(df3$occupation),]) #sum of missing values

ggplot(df3, aes(x = occupation, fill = class)) +
  geom_bar(position = "stack") +
  labs(title = "Stacked Bar Chart of <=50K and >50K by Occupation Level",
       x = "Occupation", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")

#5) relationship
as.data.frame(table(df3$relationship))
nrow(df3[is.na(df3$relationship),]) #sum of missing values

ggplot(df3, aes(x = relationship, fill = class)) +
  geom_bar(position = "stack") +
  labs(title = "Stacked Bar Chart of <=50K and >50K by Relationship Level",
       x = "Education Level", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")

#6) race
as.data.frame(table(df3$race))
nrow(df3[is.na(df3$race),]) #sum of missing values

ggplot(df3, aes(x = race, fill = class)) +
  geom_bar(position = "stack") +
  labs(title = "Stacked Bar Chart of <=50K and >50K by Race Level",
       x = "Education Level", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")

#7) Sex
as.data.frame(table(df3$sex))
nrow(df3[is.na(df3$sex),]) #sum of missing values

df3$sex <- ifelse(df3$sex == "F", "Female",
           ifelse(df3$sex == "M", "Male", df3$sex))

ggplot(df3, aes(x = sex, fill = class)) +
  geom_bar(position = "stack") +
  labs(title = "Stacked Bar Chart of <=50K and >50K by Sex Level",
       x = "Sex", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")

#8) native.country
as.data.frame(table(df3$native.country))
nrow(df3[is.na(df3$native.country),]) #sum of missing values

#map into 2 categories
df3$native.country <- ifelse(df3$native.country == "United-States", "United-States", "Non-United-States")

ggplot(df3, aes(x = native.country, fill = class)) +
  geom_bar(position = "stack") +
  labs(title = "Stacked Bar Chart of <=50K and >50K by Native Country Level",
       x = "Country", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")

#9) class
as.data.frame(table(df3$class))
nrow(df3[is.na(df3$class),]) #sum of missing values

ggplot(df3, aes(x = class, fill = class)) +
  geom_bar(position = "stack") +
  labs(title = "Stacked Bar Chart of <=50K and >50K",
       x = "Class", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")

#treat capitalgain with value 99999 as NaN
as.data.frame(table(df2$capitalgain[df2$capitalgain>=20000]))

df3$capitalgain <- ifelse(df3$capitalgain == 99999, NA, df3$capitalgain)

#remove duplicates
df4 <- df3[!duplicated(df3),]

df5 <- df4
plot_missing(df5)

#change into numerical representation
as.data.frame(table(df5$workclass))
category_mapping_workclass <- c(
  "Federal-gov" = 1,
  "Local-gov" = 2,
  "Never-worked" = 3,
  "Private" = 4,
  "Self-emp-inc" = 5,
  "Self-emp-not-inc" = 6,
  "State-gov" = 7,
  "Without-pay" = 8
)
df5$workclass_num <- category_mapping_workclass[df5$workclass]
as.data.frame(table(df5$workclass_num))

as.data.frame(table(df5$education))
category_mapping_education <- c(
  "Preschool" = 1,
  "some-Primary" = 2,
  "some-JH" = 3,
  "some-HS" = 4,
  "HS-grad" = 5,
  "Some-college" = 6,
  "Assoc-acdm/voc" = 7,
  "Bachelors" = 8,
  "Prof-school" = 9,
  "Masters" = 10,
  "Doctorate" = 11
)
df5$education_num <- category_mapping_education[df5$education]
as.data.frame(table(df5$education_num))

as.data.frame(table(df5$marital.status))
category_mapping_marital <- c(
  "Never-married" = 1,
  "Married" = 2,
  "Separated" = 3,
  "Divorced" = 4,
  "Widowed" = 5
)
df5$marital.status_num <- category_mapping_marital[df5$marital.status]
as.data.frame(table(df5$marital.status_num))

as.data.frame(table(df5$occupation))
category_mapping_occupation <- c(
  "Adm-clerical" = 1,
  "Armed-Forces" = 2,
  "Craft-repair" = 3,
  "Exec-managerial" = 4,
  "Farming-fishing" = 5,
  "Handlers-cleaners" = 6,
  "Machine-op-inspct" = 7,
  "Other-service" = 8,
  "Priv-house-serv" = 9,
  "Prof-specialty" = 10,
  "Protective-serv" = 11,
  "Sales" = 12,
  "Tech-support" = 13,
  "Transport-moving" = 14
)
df5$occupation_num <- category_mapping_occupation[df5$occupation]
as.data.frame(table(df5$occupation_num))

as.data.frame(table(df5$relationship))
category_mapping_relationship <- c(
  "Husband" = 1,
  "Not-in-family" = 2,
  "Other-relative" = 3,
  "Own-child" = 4,
  "Unmarried" = 5,
  "Wife" = 6
)
df5$relationship_num <- category_mapping_relationship[df5$relationship]
as.data.frame(table(df5$relationship_num))

as.data.frame(table(df5$race))
category_mapping_race <- c(
  "Amer-Indian-Eskimo" = 1,
  "Asian-Pac-Islander" = 2,
  "Black" = 3,
  "Other" = 4,
  "White" = 5
)
df5$race_num <- category_mapping_race[df5$race]
as.data.frame(table(df5$race_num))

as.data.frame(table(df5$sex))
category_mapping_sex <- c(
  "Female" = 0,
  "Male" = 1
)
df5$sex_num <- category_mapping_sex[df5$sex]
as.data.frame(table(df5$sex_num))

as.data.frame(table(df5$native.country))
category_mapping_native <- c(
  "Non-United-States" = 0,
  "United-States" = 1
)
df5$native.country_num <- category_mapping_native[df5$native.country]
as.data.frame(table(df5$native.country_num))

as.data.frame(table(df5$class))

category_mapping_class <- c(
  "<=50K" = 0,
  ">50K" = 1
)
df5$class_num <- category_mapping_class[df5$class]

as.data.frame(table(df5$class_num))


df6 <- df5[c("age","capitalgain","capitalloss", "hoursperweek", "workclass_num",
  "education_num", "marital.status_num", "occupation_num", "relationship_num",
  "race_num", "sex_num", "native.country_num", "class_num")]

plot_missing(df6)

#imputation
imputation_model <- mice(df6, m = 5)
imputed_df <- complete(imputation_model)

plot_missing(imputed_df)

#data balancing
feature.var <- c(
    "age", "capitalgain", "capitalloss", "hoursperweek",
    "workclass_num", "education_num", "marital.status_num",
    "occupation_num", "relationship_num", "race_num",
    "sex_num", "native.country_num"
    )
X <- imputed_df[feature.var]
y <- imputed_df$class_num


library(smotefamily)
smote_obj <- SMOTE(X,y)
imputed_df_bal <- smote_obj$data

imputed_df$class <- imputed_df$class_num
imputed_df <- imputed_df[, !names(imputed_df) %in% "class_num"]

imputed_df$class <- as.factor(imputed_df$class)
imputed_df_bal$class <- as.factor(imputed_df_bal$class)

as.data.frame(table(imputed_df$class))
as.data.frame(table(imputed_df_bal$class))

#________________________________________________________________
#TRAIN TEST SPLIT
#install.packages("randomForest")
library(randomForest)
library(caret)
library(caTools)
#install.packages('rsample')
library(rsample)
library(dplyr)



# rf_model <- randomForest(class ~ ., data = imputed_df, ntree = 100)
# ctrl <- trainControl(method = "cv", number = 1, savePredictions = TRUE)
# model_spec <- train(class ~ ., data = imputed_df, method = "rf",
#                    trControl = ctrl)


X_unbal <- imputed_df[feature.var]
y_unbal <- imputed_df$class

data_unbal_df <- cbind(X_unbal, class = y_unbal)

data_unbal_split <- initial_split(data_unbal_df, prop = 0.8, strata = "class")

train_data_unbal <- training(data_unbal_split)
test_data_unbal <- testing(data_unbal_split)

X_unbal_train <- train_data_unbal %>% select(-class)
y_unbal_train <- train_data_unbal$class
X_unbal_test <- test_data_unbal %>% select(-class)
y_unbal_test <- test_data_unbal$class


X_bal <- imputed_df_bal[feature.var]
y_bal <- imputed_df_bal$class

data_bal_df <- cbind(X_bal, class = y_bal)

data_bal_split <- initial_split(data_bal_df, prop = 0.8, strata = "class")

train_data_bal <- training(data_bal_split)
test_data_bal <- testing(data_bal_split)

X_bal_train <- train_data_bal %>% select(-class)
y_bal_train <- train_data_bal$class
X_bal_test <- test_data_bal %>% select(-class)
y_bal_test <- test_data_bal$class


#________________________________________________________________
#RANDOM FOREST MODEL
rf_model_unbal <- randomForest(y_unbal_train ~ ., data = cbind(y_unbal_train, X_unbal_train))
predictions_rf_unbal <- predict(rf_model_unbal, newdata = X_unbal_test)

confusion_matrix_rf_unbal <- table(Actual = y_unbal_test, Predicted = predictions_rf_unbal)
accuracy_rf_unbal <- sum(diag(confusion_matrix_rf_unbal)) / sum(confusion_matrix_rf_unbal)

print(confusion_matrix_rf_unbal)
print(paste("Accuracy:", accuracy_rf_unbal))
getTree(rf_model_unbal, k = 1)

rf_model_bal <- randomForest(y_bal_train ~ ., data = cbind(y_bal_train, X_bal_train))
predictions_rf_bal <- predict(rf_model_bal, newdata = X_bal_test)

confusion_matrix_rf_bal <- table(Actual = y_bal_test, Predicted = predictions_rf_bal)
accuracy_rf_bal <- sum(diag(confusion_matrix_rf_bal)) / sum(confusion_matrix_rf_bal)
print(confusion_matrix_rf_bal)
print(paste("Accuracy:", accuracy_rf_bal))

importance <- rf_model_bal$importance

#evaluation on training data
predictions_rf_unbal_tr <- predict(rf_model_unbal, newdata = X_unbal_train)
confusion_matrix_rf_unbal_tr <- table(Actual = y_unbal_train, Predicted = predictions_rf_unbal_tr)
accuracy_rf_unbal_tr <- sum(diag(confusion_matrix_rf_unbal_tr)) / sum(confusion_matrix_rf_unbal_tr)
print(confusion_matrix_rf_unbal_tr)
print(paste("Accuracy:", accuracy_rf_unbal_tr))

predictions_rf_bal_tr <- predict(rf_model_bal, newdata = X_bal_train)
confusion_matrix_rf_bal_tr <- table(Actual = y_bal_train, Predicted = predictions_rf_bal_tr)
accuracy_rf_bal_tr <- sum(diag(confusion_matrix_rf_bal_tr)) / sum(confusion_matrix_rf_bal_tr)
print(confusion_matrix_rf_bal_tr)
print(paste("Accuracy:", accuracy_rf_bal_tr))
#########
#HYPERPARAMETER
#########

cv_folds <- createFolds(y_bal_train, k = 3, returnTrain = TRUE)

tuneGrid <- expand.grid(.mtry = c(1 : 10))
ctrl <- trainControl(method = "cv",
                     number=3,
                     search = 'grid',
                     classProbs = TRUE,
                     savePredictions = "final",
                     index=cv_folds,
                     summaryFunction = twoClassSummary)


ntrees <- c(500, 1000)    
nodesize <- c(1, 5)

params <- expand.grid(ntrees = ntrees,
                      nodesize = nodesize)

store_maxnode <- vector("list", nrow(params))

levels(y_bal_train)
new_levels <- make.names(levels(y_bal_train))
levels(y_bal_train) <- new_levels

data_bal_comb = cbind(y_bal_train, X_bal_train)
for(i in 1:nrow(params)){
  nodesize <- params[i,2]
  ntree <- params[i,1]
  set.seed(65)
  rf_model <- train(y_bal_train~.,
                    data =data_bal_comb ,
                    method = "rf",
                    importance=TRUE,
                    metric = "ROC",
                    tuneGrid = tuneGrid,
                    trControl = ctrl,
                    ntree = ntree,
                    nodesize = nodesize)
  store_maxnode[[i]] <- rf_model
}
#########



#hyperparameter
library(caret)

# Define the hyperparameter grid
param_grid <- expand.grid(
  #min_samples_leaf = c(1, 2, 4)
  mtry = c(2, 4, 6, 8)
  )


# Create a train control object for cross-validation
ctrl <- trainControl(
  method = "cv",  # Cross-validation method
  number = 5,     # Number of folds
  verboseIter = TRUE
)

rf_tuning_data = cbind(y_bal_test, X_bal_test)
colnames(rf_tuning_data)[1] <- "class"

class(rf_tuning_data$class)

# Tune the Random Forest model using grid search
rf_tune <- train(
  class ~ .,  # Your formula here
  data = rf_tuning_data,  # Training data
  method = "rf",      # Random Forest method
  trControl = ctrl,  # Train control object
  tuneGrid = param_grid
  )

# Print the best hyperparameters
print(rf_tune)

# Fit the final model using the best hyperparameters
final_rf_model <- randomForest(
  class ~ .,  # Your formula here
  data = train_data,  # Training data
  ntree = rf_tune$bestTune$n_estimators,
  mtry = rf_tune$bestTune$max_features,
  maxdepth = rf_tune$bestTune$max_depth,
  nodesize = rf_tune$bestTune$min_samples_split
)

# Evaluate the final model on the test data
predictions <- predict(final_rf_model, newdata = test_data)

#________________________________________________________________
#LOGISTIC REGRESSION
#one hot encoding
columns_to_encode <- c("workclass_num", "marital.status_num", "occupation_num",
                       "relationship_num", "race_num")

imputed_df_encoded <- data.frame(imputed_df)
for (col in columns_to_encode) {
  encoded_col <- model.matrix(~ 0 + factor(imputed_df[, col]))
  
  colnames(encoded_col) <- gsub(" ", "_", colnames(encoded_col))  
  colnames(encoded_col) <- gsub("[^[:alnum:]_]", "", colnames(encoded_col))
  colnames(encoded_col) <- paste(col, colnames(encoded_col), sep = "_")
  imputed_df_encoded <- cbind(imputed_df_encoded, encoded_col)
}
imputed_df_encoded <- imputed_df_encoded[, !(names(imputed_df_encoded) %in% columns_to_encode)]

#encoded df balancing
X_encoded <- imputed_df_encoded[, !names(imputed_df_encoded) %in% "class"]
y_encoded <- imputed_df_encoded$class

smote_obj_encoded <- SMOTE(X_encoded,y_encoded)
imputed_df_bal_encoded <- smote_obj_encoded$data

imputed_df_encoded$class <- as.factor(imputed_df$class)
imputed_df_bal_encoded$class <- as.factor(imputed_df_bal$class)

as.data.frame(table(imputed_df_encoded$class))
as.data.frame(table(imputed_df_bal_encoded$class))

#train-test split encoded
X_unbal_encoded <- imputed_df_encoded[, !names(imputed_df_encoded) %in% "class"]
y_unbal_encoded <- imputed_df_encoded$class

data_unbal_df_encoded <- cbind(X_unbal_encoded, class = y_unbal_encoded)

data_unbal_split_encoded <- initial_split(data_unbal_df_encoded, prop = 0.8, strata = "class")

train_data_unbal_encoded <- training(data_unbal_split_encoded)
test_data_unbal_encoded <- testing(data_unbal_split_encoded)

X_unbal_train_encoded <- train_data_unbal_encoded %>% select(-class)
y_unbal_train_encoded <- train_data_unbal_encoded$class
X_unbal_test_encoded <- test_data_unbal_encoded %>% select(-class)
y_unbal_test_encoded <- test_data_unbal_encoded$class


X_bal_encoded <- imputed_df_bal_encoded[, !names(imputed_df_bal_encoded) %in% "class"]
y_bal_encoded <- imputed_df_bal_encoded$class

data_bal_df_encoded <- cbind(X_bal_encoded, class = y_bal_encoded)

data_bal_split_encoded <- initial_split(data_bal_df_encoded, prop = 0.8, strata = "class")

train_data_bal_encoded <- training(data_bal_split_encoded)
test_data_bal_encoded <- testing(data_bal_split_encoded)

X_bal_train_encoded <- train_data_bal_encoded %>% select(-class)
y_bal_train_encoded <- train_data_bal_encoded$class
X_bal_test_encoded <- test_data_bal_encoded %>% select(-class)
y_bal_test_encoded <- test_data_bal_encoded$class

#LOGISTIC REGRESSION
logistic_model_unbal <- glm(y_unbal_train_encoded ~ ., data = cbind(y_unbal_train_encoded,X_unbal_train_encoded), family = binomial,control = glm.control(maxit = 2000))
predictions_logistic_unbal <- predict(logistic_model_unbal, newdata = X_unbal_test_encoded)

threshold <- 0.5
predicted_classes_logistic_unbal <- ifelse(predictions_logistic_unbal > threshold, 1, 0)

confusion_matrix_logistic_unbal <- table(Actual = y_unbal_test_encoded, Predicted = predicted_classes_logistic_unbal)
accuracy_logistic_unbal <- sum(diag(confusion_matrix_logistic_unbal)) / sum(confusion_matrix_logistic_unbal)
print(confusion_matrix_logistic_unbal)
print(paste("Accuracy:", accuracy_logistic_unbal))


logistic_model_bal <- glm(y_bal_train_encoded ~ ., data = cbind(y_bal_train_encoded, X_bal_train_encoded),family = binomial, control = glm.control(maxit = 100000))
predictions_logistic_bal <- predict(logistic_model_bal, newdata = X_bal_test_encoded)

threshold <- 0.5
predicted_classes_logistic_bal <- ifelse(predictions_logistic_bal > threshold, 1, 0)

confusion_matrix_logistic_bal <- table(Actual = y_bal_test_encoded, Predicted = predicted_classes_logistic_bal)
accuracy_logistic_bal <- sum(diag(confusion_matrix_logistic_bal)) / sum(confusion_matrix_logistic_bal)
print(confusion_matrix_logistic_bal)
print(paste("Accuracy:", accuracy_logistic_bal))

coeff_logistic_bal <- coef(logistic_model_bal)
coeff_df <- data.frame(Variable = names(coeff_logistic_bal), Coefficient = coeff_logistic_bal)


#evaluation on training data
predictions_logistic_unbal_tr <- predict(logistic_model_unbal, newdata = X_unbal_train_encoded)
predicted_classes_logistic_unbal_tr <- ifelse(predictions_logistic_unbal_tr > threshold, 1, 0)
confusion_matrix_logistic_unbal_tr <- table(Actual = y_unbal_train_encoded, Predicted = predicted_classes_logistic_unbal_tr)
accuracy_logistic_unbal_tr <- sum(diag(confusion_matrix_logistic_unbal_tr)) / sum(confusion_matrix_logistic_unbal_tr)
print(confusion_matrix_logistic_unbal_tr)
print(paste("Accuracy:", accuracy_logistic_unbal_tr))

predictions_logistic_bal_tr <- predict(logistic_model_bal, newdata = X_bal_train_encoded)
predicted_classes_logistic_bal_tr <- ifelse(predictions_logistic_bal_tr > threshold, 1, 0)
confusion_matrix_logistic_bal_tr <- table(Actual = y_bal_train_encoded, Predicted = predicted_classes_logistic_bal_tr)
accuracy_logistic_bal_tr <- sum(diag(confusion_matrix_logistic_bal_tr)) / sum(confusion_matrix_logistic_bal_tr)
print(confusion_matrix_logistic_bal_tr)
print(paste("Accuracy:", accuracy_logistic_bal_tr))

# Display the counts
print(counts)


#SVM
library(e1071)

svm_model_unbal <- svm(y_unbal_train ~ ., data = cbind(y_unbal_train,X_unbal_train), kernel = "radial",  probability = TRUE)
predictions_svm_unbal <- predict(svm_model_unbal, newdata = X_unbal_test)

confusion_matrix_svm_unbal <- table(Actual = y_unbal_test, Predicted = predictions_svm_unbal)
accuracy_svm_unbal <- sum(diag(confusion_matrix_svm_unbal)) / sum(confusion_matrix_svm_unbal)
print(confusion_matrix_svm_unbal)
print(paste("Accuracy:", accuracy_svm_unbal))


svm_model_bal <- svm(y_bal_train ~ ., data = cbind(y_bal_train, X_bal_train), kernel = "radial",probability = TRUE)
predictions_svm_bal <- predict(svm_model_bal, newdata = X_bal_test)

confusion_matrix_svm_bal <- table(Actual = y_bal_test, Predicted = predictions_svm_bal)
accuracy_svm_bal <- sum(diag(confusion_matrix_svm_bal)) / sum(confusion_matrix_svm_bal)

print(confusion_matrix_svm_bal)
print(paste("Accuracy:", accuracy_svm_bal))

#evaluation on training data
predictions_svm_unbal_tr <- predict(svm_model_unbal, newdata = X_unbal_train)
confusion_matrix_svm_unbal_tr <- table(Actual = y_unbal_train, Predicted = predictions_svm_unbal_tr)
accuracy_svm_unbal_tr <- sum(diag(confusion_matrix_svm_unbal_tr)) / sum(confusion_matrix_svm_unbal_tr)
print(confusion_matrix_svm_unbal_tr)
print(paste("Accuracy:", accuracy_svm_unbal_tr))

predictions_svm_bal_tr <- predict(svm_model_bal, newdata = X_bal_train)
confusion_matrix_svm_bal_tr <- table(Actual = y_bal_train, Predicted = predictions_svm_bal_tr)
accuracy_svm_bal_tr <- sum(diag(confusion_matrix_svm_bal_tr)) / sum(confusion_matrix_svm_bal_tr)
print(confusion_matrix_svm_bal_tr)
print(paste("Accuracy:", accuracy_svm_bal_tr))

#############
#hyper parameter tuning
#############

#sampling 10%
data_bal_comb <- cbind(y_bal_train, X_bal_train)

as.data.frame(table(y_bal_train))
nsample <- 0.1 * length(y_bal_train)
rand_index <- sample(1:length(y_bal_train),nsample)

y_tune <- y_bal_train[rand_index]
X_tune <- X_bal_train[rand_index, ]
as.data.frame(table(y_tune))


rf_tune <- randomForest(y_tune ~ ., data = cbind(y_tune, X_tune))
predictions_rf_tune <- predict(rf_tune, newdata = X_bal_test)

confusion_matrix_rf_tune <- table(Actual = y_bal_test, Predicted = predictions_rf_tune)
accuracy_rf_tune <- sum(diag(confusion_matrix_rf_tune)) / sum(confusion_matrix_rf_tune)
print(confusion_matrix_rf_tune)
print(paste("Accuracy:", accuracy_rf_tune))

#HYPERPARAMATER TUNING
cv_folds <- createFolds(y_tune, k = 5, returnTrain = TRUE)
tuneGrid <- expand.grid(.mtry = c(1 : 10))
ctrl <- trainControl(method = "cv",
                     number=5,
                     search = 'grid',
                     classProbs = TRUE,
                     savePredictions = "final",
                     index=cv_folds,
                     summaryFunction = twoClassSummary)
ntrees <- c(750, 1000,1250)    
nodesize <- c(1, 3, 5)
params <- expand.grid(ntrees = ntrees,
                      nodesize = nodesize)

store_maxnode <- vector("list", nrow(params))
y_tune <- make.names(y_tune)
data_bal_comb = cbind(y_tune, X_tune)

for(i in 1:nrow(params)){
  nodesize <- params[i,2]
  ntree <- params[i,1]
  set.seed(65)
  rf_model <- train(y_tune~.,
                    data =data_bal_comb ,
                    method = "rf",
                    importance=TRUE,
                    metric = "ROC",
                    tuneGrid = tuneGrid,
                    trControl = ctrl,
                    ntree = ntree,
                    nodesize = nodesize)
  store_maxnode[[i]] <- rf_model
}

names(store_maxnode) <- paste("ntrees:", params$ntrees,
                              "nodesize:", params$nodesize)
results_mtry <- resamples(store_maxnode)

summary(results_mtry)
lapply(store_maxnode, function(x) x$results[x$results$ROC == max(x$results$ROC),])

rf_model_bal_tune <- randomForest(y_bal_train ~ ., data = cbind(y_bal_train, X_bal_train))
predictions_rf_bal_tune <- predict(rf_model_bal_tune, newdata = X_bal_test)

confusion_matrix_rf_bal_tune <- table(Actual = y_bal_test, Predicted = predictions_rf_bal_tune)
accuracy_rf_bal_tune <- sum(diag(confusion_matrix_rf_bal_tune)) / sum(confusion_matrix_rf_bal_tune)
print(confusion_matrix_rf_bal_tune)
print(paste("Accuracy:", accuracy_rf_bal_tune))

#ROC Curve
library(pROC)

rf_model_unbal_roc <- randomForest(y_unbal_train ~ ., data = cbind(y_unbal_train, X_unbal_train), probability = TRUE)
predictions_rf_unbal_roc <- predict(rf_model_unbal_roc, newdata = X_unbal_test, type = "vote")[,2]
predictions_svm_unbal_roc <- predict(svm_model_unbal, newdata = X_unbal_test, probability = TRUE)
predictions_svm_unbal_roc<-attributes(predictions_svm_unbal_roc)$probabilities[,2]

rf_model_bal_roc <- randomForest(y_bal_train ~ ., data = cbind(y_bal_train, X_bal_train), probability = TRUE)
predictions_rf_bal_roc <- predict(rf_model_bal_roc, newdata = X_bal_test, type = "vote")[,2]
predictions_svm_bal_roc <- predict(svm_model_bal, newdata = X_bal_test, probability = TRUE)
predictions_svm_bal_roc<-attributes(predictions_svm_bal_roc)$probabilities[,2]


roc_obj <- roc(response = y_unbal_test, predictor = predictions_rf_unbal_roc)
roc_obj_logistic <- roc(response = y_unbal_train_encoded, predictor = predictions_logistic_unbal_tr)
roc_obj_svm <- roc(response = y_unbal_test, predictor = predictions_svm_unbal_roc)
roc_obj2 <- roc(response = y_bal_test, predictor = predictions_rf_bal_roc)
roc_obj_logistic2 <- roc(response = y_bal_train_encoded, predictor = predictions_logistic_bal_tr)
roc_obj_svm2 <- roc(response = y_bal_test, predictor = predictions_svm_bal_roc)


plot(roc_obj, col = "blue", main = "ROC Curves", print.auc = F, auc.polygon = F, grid = TRUE)
lines(roc_obj_logistic, col = "red")
lines(roc_obj_svm, col = "green")
lines(roc_obj2, col = "black")
lines(roc_obj_logistic2, col = "brown")
lines(roc_obj_svm2, col = "purple")
