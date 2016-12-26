require(xgboost)
require(readr)
require(stringr)
require(caret)
require(car)
require(dplyr)
require(tidyr)

set.seed(12345)


boost.cv <- xgb.cv(data=data.matrix(train_boost[,-1]), label=outcome, objective = "multi:softprob", num_class=12, nrounds=60, eta = .1, seed=12345, nthread=4, eval_metric="mlogloss", max_depth=15, nfold=4)


#using multiclass log loss as metric
boost <- xgboost(data=data.matrix(train_boost[,-1]), label=outcome, objective = "multi:softprob", num_class=12, nrounds=60, eta = .1, seed=12345, eval_metric="mlogloss", nthread=4, max_depth=15)

boost_names <- dimnames(train_boost)[[2]]
boost_names <- boost_names[-1]

importance_matrix <- xgb.importance(boost_names, model=boost)


xgb.plot.importance(importance_matrix[1:50,])


prob <- matrix(predict(boost, newdata=data.matrix(test_boost[,-1])), ncol = 12, byrow = T)

prob_df <- as.data.frame(prob)
test_user_id <- as.data.frame(test_boost$user_id)

colnames(prob_df) <- outcome_labels

submission <- bind_cols(user_id, prob_df)

#Check out SMOTE algorithm