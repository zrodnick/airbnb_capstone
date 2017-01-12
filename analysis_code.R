require(xgboost)
require(readr)
require(stringr)
require(caret)
require(car)
require(dplyr)
require(tidyr)

set.seed(12345)

#Logistic Regression

boost1.cv <- xgb.cv(data=data.matrix(train_boost1[,-1]), label=outcome1, objective = "binary:logistic", nrounds=10000, eta = .001, seed=12345, eval_metric="logloss", nthread=6, max_depth=10, early_stopping_rounds = 5, nfold=5, missing=-1)



boost1_names <- dimnames(train_boost1)[[2]]
boost1_names <- boost1_names[-1]
importance_matrix.cv <- xgb.importance(boost1_names, model=boost1.cv)
xgb.plot.importance(importance_matrix.cv[1:50,])


boost2.cv <- xgb.cv(data=data.matrix(train_boost2[,-1]), label=outcome2, objective = "multi:softprob", num_class=12, nrounds=10000, eta = .001, seed=12345, nthread=6, eval_metric="mlogloss", max_depth=10, nfold=5, missing = -1, early_stopping_rounds = 5)

boosth.cv <- xgb.cv(data=data.matrix(train_boost1[,-1]), label=outcomeh, objective = "multi:softprob", num_class=12, nrounds=1000, eta = .1, seed=12345, nthread=6, eval_metric="mlogloss", max_depth=10, nfold=5, missing = -1, early_stopping_rounds = 5)



#using multiclass log loss as metric

boost1 <- xgboost(data=data.matrix(train_boost1[,-1]), label=outcome1, objective = "binary:logistic", nrounds=50, eta = .1, seed=12345, eval_metric="logloss", nthread=6, max_depth=10, missing= -1) 
  
boost2 <- xgboost(data=data.matrix(train_boost2[,-1]), label=outcome2, objective = "multi:softprob", num_class=12, nrounds=620, eta = .01, seed=12345, eval_metric="mlogloss", nthread=6, max_depth=10, missing= -1, )



prob1 <- matrix(predict(boost1, newdata=data.matrix(test_boost1[,-1])), ncol = 1, byrow = T)
prob1_df <- as.data.frame(prob1)

test_user_id <- as.data.frame(test_boost1$user_id)
colnames(prob_df) <- outcome_labels

test_prob1 <- bind_cols(test_user_id, prob_df)

prob2 <- matrix(predict(boost, newdata=data.matrix(test_boost[,-1])), ncol = 12, byrow = T)
prob2_df <- as.data.frame(prob)





submission <- bind_cols(test_user_id, prob_df)


boost2_names <- dimnames(train_boost1)[[2]]
boost2_names <- boost2_names[-1]
importance_matrix <- xgb.importance(boost2_names, model=boost1)
xgb.plot.importance(importance_matrix[1:50,])


