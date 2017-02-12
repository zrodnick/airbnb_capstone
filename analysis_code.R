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

params <- list( max.depth = 6, 
                eta = 0.1,
                subsample = 0.5, 
                colsample_bytree = 0.5, 
                max_delta_step = 1, 
                scale_pos_weight = -20,
                objective = "multi:softprob")

params2 <- list( max.depth = 9, eta = 0.1,
                subsample = 0.5, colsample_bytree = 0.5,
                objective = "rank:pairwise")

#Cross Validation

boost.cv <- xgb.cv(data=data.matrix(train_boost1[,-1]), params=params, label=outcome, num_class=12, nrounds=5000, seed=12345, eval_metric="mlogloss", nfold=5, early_stopping_rounds = 20, missing=-1)

boost2.cv <- xgb.cv(data=data.matrix(train_boost2[,-1]), params=params, label=outcome2, num_class=12, nrounds=5000, seed=12345, eval_metric="mlogloss", nfold=5, missing = -1, early_stopping_rounds = 20)

#Training the model 

boost1 <- xgboost(data=data.matrix(train_boost1[,-1]), label=outcome, params = params, nrounds=120, seed=12345, num_class=12, eval_metric="mlogloss", nthread=6, missing=-1) 

boost2 <- xgboost(data=data.matrix(train_boost2[,-1]), label=outcome2, params = params, nrounds=110, seed=12345, num_class=12, eval_metric="merror", nthread=3, missing= -1) 

#Predictions

boost_names <- dimnames(train_boost1)[[2]]
boost_names <- boost_names[-1]
importance_matrix <- xgb.importance(boost_names, model=boost1)
xgb.ggplot.importance(importance_matrix[1:20,])

prob1 <- predict(boost1, newdata=data.matrix(test_boost1[,-1]))
prob1_df <- as.data.frame(matrix(prob1, nrow=12))
test_user_id <- as.data.frame(test_boost1$user_id)
rownames(prob1_df) <- outcome_labels
predictions_top5 <- as.vector(apply(prob1_df, 2, function(x) names(sort(x)[12:8])))


ids <- NULL
for (i in 1:NROW(test_boost1)) {
  idx <- test_boost1$user_id[i]
  ids <- append(ids, rep(idx,5))
} 

submission <- NULL
submission$id <- ids
submission$country <- predictions_top5

submission <- as.data.frame(submission)
write.csv(submission, "submission.csv", quote=FALSE, row.names = FALSE)

prob2 <- predict(boost2, newdata=data.matrix(test_boost2[,-1]))
prob2_df <- as.data.frame(matrix(prob2, nrow=12))
test_user_id2 <- as.data.frame(test_boost2$user_id)
rownames(prob2_df) <- outcome2_labels
predictions2_top5 <- as.vector(apply(prob2_df, 2, function(x) names(sort(x)[12:8])))

boost_names2 <- dimnames(train_boost2)[[2]]
boost_names2 <- boost_names2[-1]
importance_matrix2 <- xgb.importance(boost_names2, model=boost2)
xgb.ggplot.importance(importance_matrix2[1:20,])

submission2 <- NULL
submission2$id <- ids
submission2$country <- predictions2_top5

submission2 <- as.data.frame(submission2)
write.csv(submission2, "submission2.csv", quote=FALSE, row.names = FALSE)


