require(xgboost)
require(readr)
require(stringr)
require(caret)
require(car)
require(dplyr)
require(tidyr)
require(Ckmeans.1d.dp)
require(DMwR)

set.seed(12345)

params <- list( max.depth = 9, eta = 0.1,
                subsample = 0.5, colsample_bytree = 0.5, max_delta_step = 1,
                objective = "multi:softprob")

params2 <- list( max.depth = 9, eta = 0.1,
                subsample = 0.5, colsample_bytree = 0.5,
                objective = "rank:pairwise")


boost.cv <- xgb.cv(data=data.matrix(train_boost1[,-1]), params=params, label=outcome, num_class=12, nrounds=5000, seed=12345, eval_metric="merror", nfold=5, missing = -1, early_stopping_rounds = 20)

boost2.cv <- xgb.cv(data=data.matrix(train_boost2[,-1]), params=params, label=outcome2, num_class=12, nrounds=5000, seed=12345, eval_metric="merror", nfold=5, missing = -1, early_stopping_rounds = 50)

#Training the model 

boost1 <- xgboost(data=data.matrix(train_boost1[,-1]), label=outcome, params = params, nrounds=80, seed=12345, num_class=12, eval_metric="merror", nthread=6, missing= -1) 

boost2 <- xgboost(data=data.matrix(train_boost2[,-1]), label=outcome2, params = params, nrounds=85, seed=12345, num_class=12, eval_metric="merror", nthread=3, missing= -1) 

#Predictions


boost_names <- dimnames(train_boost1)[[2]]
boost_names <- boost_names[-1]
importance_matrix <- xgb.importance(boost_names, model=boost1)
xgb.ggplot.importance(importance_matrix[1:20,])

prob2 <- matrix(predict(boost2, newdata=data.matrix(test_boost2[,-1])), ncol = 12, byrow = T)
prob2_df <- as.data.frame(prob2)
test_user_id2 <- as.data.frame(test_boost2$user_id)
colnames(prob2_df) <- outcome2_labels
prob2_df <- bind_cols(test_user_id2, prob2_df)

boost_names2 <- dimnames(train_boost2)[[2]]
boost_names2 <- boost_names2[-1]
importance_matrix2 <- xgb.importance(boost_names2, model=boost2)
xgb.ggplot.importance(importance_matrix2[1:20,])

#Confusion matrix
confusion_df <- prob1_df

confusion_df <- mutate(confusion_df, first_prediction = max.col(prob1))

confusion_df$first_prediction <- factor(confusion_df$first_prediction, levels = c(1,2,3,4,5,6,7,8,9,10,11,12), labels=outcome_labels)


