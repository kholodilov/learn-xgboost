require(e1071)
require(xgboost)
require(caret)

read_libsvm_to_xgb <- function(file_path) {
  libsvm <- read.matrix.csr(file = file_path, fac = FALSE)
  xgb.DMatrix(as(libsvm$x, "dgCMatrix"), label = libsvm$y)
}

dtrain <- read_libsvm_to_xgb("blocked_train.libsvm")
dtest <- read_libsvm_to_xgb("blocked_test.libsvm")

nround <- 10
param <- list(max_depth=3, eta=1, silent=1, nthread=8, objective='binary:logistic', eval_metric = "auc", eval_metric = "error")
xgb_cv = xgb.cv(param, dtrain, nround, nfold=5)

model <- xgb.train(data = dtrain, nrounds = nround, watchlis = list(train=dtrain, test=dtest), verbose = 1, eval_metric = "auc", eval_metric = "error")

pred <- predict(model, dtest)
err <- mean(as.numeric(pred > 0.5) != getinfo(dtest, 'label'))
print(paste("test-error=", err))

importance_matrix <- xgb.importance(model = model)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)

xgb.dump(model, with_stats = T)
