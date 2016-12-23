require(xgboost)
require(readr)
require(stringr)
require(caret)
require(car)

set.seed(12345)

#cross-validation
boost.cv <- xgb.cv(data=data.matrix(train_boost[,-1]), label=outcome, objective = "multi:softprob", num_class=12, nrounds=10, eta = .1, seed=12345, nthread=4, max_depth=15, nfold=4)

boost2.cv <- xgb.cv(data=data.matrix(train_boost[,-1]), label=outcome, objective = "multi:softprob", num_class=12, nrounds=10, eta = .1, seed=12345, nthread=4, eval_metric="mlogloss", max_depth=15, nfold=4)

#using multiclass error rate as metric (algorithm's choice)
boost <- xgboost(data=data.matrix(train_boost[,-1]), label=outcome, objective = "multi:softprob", num_class=12, nrounds=50, eta = .1, seed=12345, nthread=4, max_depth=15)

#using multiclass log loss as metric
boost2 <- xgboost(data=data.matrix(train_boost[,-1]), label=outcome, objective = "multi:softprob", num_class=12, nrounds=50, eta = .1, seed=12345, eval_metric="mlogloss", nthread=4, max_depth=15)

boost_names <- dimnames(train_boost)[[2]]
boost_names <- boost_names[-1]

importance_matrix <- xgb.importance(boost_names, model=boost)
importance_matrix2 <- xgb.importance(boost_names, model=boost2)

xgb.plot.importance(importance_matrix[1:50,])

prob.cv <- matrix(predict(boost, newdata=data.matrix(train_boost[,-1])), ncol=12, byrow=T)

prob <- matrix(predict(boost, newdata=data.matrix(test_boost[,-1])), ncol = 12, byrow = T)
prob2 <- matrix(predict(boost2, newdata=data.matrix(test_boost[,-1])), ncol=12, byrow=T)

