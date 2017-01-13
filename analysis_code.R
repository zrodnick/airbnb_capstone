require(xgboost)
require(readr)
require(stringr)
require(caret)
require(car)
require(dplyr)
require(tidyr)

set.seed(12345)

params <- list( max.depth = 10, eta = 0.005, booster = "gbtree",
                subsample = 0.5, colsample_bytree = 0.5,
                objective = "binary:logistic", min_child_weight=1)


boost1.cv <- xgb.cv(data=data.matrix(train_boost1[,-1]), params = params, label=outcome1, nrounds=2000, seed=12345, eval_metric="logloss", nthread=6, early_stopping_rounds = 50, nfold=5, missing=-1)

boost1_names <- dimnames(train_boost1)[[2]]
boost1_names <- boost1_names[-1]
importance_matrix.cv <- xgb.importance(boost1_names, model=boost1.cv)
xgb.plot.importance(importance_matrix.cv[1:50,])


boost2.cv <- xgb.cv(data=data.matrix(train_boost2[,-1]), label=outcome2,objective = "multi:softprob", num_class=12, nrounds=60, eta = .1, seed=12345, nthread=6, eval_metric="mlogloss", max_depth=10, nfold=5, missing = -1)


boosth.cv <- xgb.cv(data=data.matrix(train_boost1[,-1]), label=outcomeh, objective = "multi:softprob", num_class=12, nrounds=1000, eta = .1, seed=12345, nthread=6, eval_metric="mlogloss", max_depth=5, nfold=5, missing = -1, early_stopping_rounds = 5, max_delta_step = 3)

boostl.cv <- xgb.cv(data=data.matrix(train_boost_large[,-1]), label=outcomel, objective = "multi:softprob", num_class=12, nrounds=1000, eta = .1, seed=12345, nthread=6, eval_metric="mlogloss", max_depth=10, nfold=5, early_stopping_rounds = 5, max_delta_step=3)

boostlb.cv <- xgb.cv(data=data.matrix(train_boost_large[,-1]), label=outcomelb, params=params, nthread=6, eval_metric="logloss", nfold=5, early_stopping_rounds = 50, nrounds=2000, max_delta_step=3)

#Training the model 

boost1 <- xgboost(data=data.matrix(train_boost1[,-1]), label=outcomelb, params = params, nrounds=1015, seed=12345, eval_metric="logloss", nthread=6, missing= -1) 

prob1 <- matrix(predict(boost1, newdata=data.matrix(test_boost1[,-1])), ncol = 1, byrow = T)
prob1_df <- as.data.frame(prob1)
prob1_df <- bind_cols(test_user_id, prob1_df)  


boost2 <- xgboost(data=data.matrix(train_boost2[,-1]), label=outcome2, objective = "multi:softprob", num_class=12, nrounds=620, eta = .01, seed=12345, eval_metric="mlogloss", nthread=6, max_depth=10, missing= -1)

boost3 <- xgboost(data=data.matrix(train_boost_large[,-1]), label=outcomel, objective = "multi:softprob", num_class=12, nrounds=90, eta = .1, seed=12345, nthread=6, eval_metric="mlogloss", max_depth=10)

boosth <- xgboost(data=data.matrix(train_boost1[,-1]), label=outcomeh, objective = "multi:softprob", num_class=12, nrounds=110, eta = .1, seed=12345, nthread=6, eval_metric="mlogloss", max_depth=5)
                  


test_user_id <- as.data.frame(test_boost1$user_id)
colnames(prob2_df) <- outcome_labels

test_prob1 <- bind_cols(test_user_id, prob_df)



prob3 <- matrix(predict(boost3, newdata=data.matrix(test_boost1[,-1])), ncol = 12, byrow = T)
prob3_df <- as.data.frame(prob3)
test_user_id <- as.data.frame(test_boost1$user_id)
colnames(prob3_df) <- outcomel_labels
submission3 <- bind_cols(test_user_id, prob3_df)


submission <- bind_cols(test_user_id, prob_df)

submission2 <- bind_cols(test_boost1[1], prob2_df)

boost2_names <- dimnames(train_boost1)[[2]]
boost2_names <- boost2_names[-1]
importance_matrix <- xgb.importance(boost2_names, model=boost1)
xgb.plot.importance(importance_matrix[1:50,])


