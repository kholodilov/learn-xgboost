# https://github.com/dmlc/xgboost/blob/master/R-package/vignettes/xgboostPresentation.Rmd

require(xgboost)

data(agaricus.train, package='xgboost')
data(agaricus.test, package='xgboost')
train <- agaricus.train
test <- agaricus.test

dtrain <- xgb.DMatrix(data = train$data, label = train$label)
dtest <- xgb.DMatrix(data = test$data, label=test$label)
watchlist <- list(train=dtrain, test=dtest)
bst <- xgb.train(data = dtrain, max_depth = 2, eta = 1, nthread = 2, nrounds = 2, watchlist=watchlist, objective = "binary:logistic", verbose = 2)

bstLinear <- xgb.train(data=dtrain, booster = "gblinear", max_depth=2, nthread = 2, nrounds=2, watchlist=watchlist, eval_metric = "error", eval_metric = "logloss", objective = "binary:logistic")

pred <- predict(bst, test$data)
err <- mean(as.numeric(pred > 0.5) != test$label)
print(paste("test-error=", err))

importance_matrix <- xgb.importance(model = bst)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)

xgb.dump(bst, with_stats = T)
xgb.plot.tree(model = bst)
