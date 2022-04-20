bdd_k <- predict(bdd_dummy, newdata = bdd_k)
bdd_k <- data.frame(bdd_k)

#Re-adding target variable (BDD_Score) to dataset bc dummyVars dropped it
score_vals <- cp_bdd_survey_data %>%
  select(BDD_Categories)
bdd_k <- cbind(bdd_k, score_vals)

#Training data
bdd_k <- bdd_k %>%
  mutate(BDD_Categories = factor(BDD_Categories))
bdd_split <- createDataPartition(bdd_k$BDD_Categories, p = 0.8, list = FALSE)
features_train <- bdd_k[bdd_split, !(names(bdd_k) %in% c('BDD_Categories'))]
features_test <- bdd_k[-bdd_split, !(names(bdd_k) %in% c('BDD_Categories'))]
target_train <- bdd_k[bdd_split, "BDD_Categories"]
target_test <- bdd_k[-bdd_split, "BDD_Categories"]
preprocess_object <- preProcess(features_train, 
                                method = c('center', 'scale', 'knnImpute'))
#>>>>>>> 4b4a23376f977ff2ec8692cc93344c67b6fb3229

features_train <- predict(preprocess_object, newdata = features_train)
features_test <- predict(preprocess_object, newdata = features_test)

#<<<<<<< HEAD
#=======
#Fitting kNN model 
knn_fit <- knn3(features_train, target_train, k = 5)
knn_pred <- predict(knn_fit, features_test, type = 'class' )
predictions <- cbind(data.frame(target_test, knn_pred))
predictions
#Creating error graph
k_seq <- seq(from = 1, to = 30, by = 2)

error_df <- data.frame(matrix(ncol = 2, nrow= 0))
x <- c('error', 'k')
colnames(error_df) <- x

for(i in k_seq) {
  knn_fit <- knn3(features_train, target_train, k = i)
  knn_pred <- predict(knn_fit, features_test, type = 'class')
  error <- mean(ifelse(predictions$target_test != knn_pred, 1, 0))
  error_df[i,'error'] <- error
  error_df[i, 'k'] <- i
}
error_df <- drop_na(error_df)
ggplot(error_df,
       aes( x = k, y = error)) +
  geom_line() +
  geom_point() +
  labs(x = 'k', y = 'Error')

#Creating confusion matrix and plotting it
knn_conf <- confusionMatrix(predictions$knn_pred, predictions$target_test)
conf_df <- as.data.frame(knn_conf$table)
final_plot <- ggplot(conf_df, aes(x = Prediction, y = Reference, fill=Freq)) + geom_tile() + 
  scale_fill_distiller(palette="Greens", direction=1) + geom_text(aes(label = Freq)) +
  labs(title = "KNN Confusion Matrix", x = "Predicted Values", y = "Actual Values") +
  theme(plot.title = element_text(hjust = 0.5))

