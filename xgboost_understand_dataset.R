# http://xgboost.readthedocs.io/en/latest/R-package/discoverYourData.html

require(xgboost)
require(Matrix)
require(data.table)
require(vcd)
require(r2pmml)

data(Arthritis)
df <- data.table(Arthritis, keep.rownames = F)
head(df[,AgeDiscret := as.factor(round(Age/10,0))])
head(df[,AgeCat:= as.factor(ifelse(Age > 30, "Old", "Young"))])
df[,ID:=NULL]
sparse_matrix <- sparse.model.matrix(Improved~.-1, data = df)
output_vector = df[,Improved] == "Marked"

bst <- xgboost(data = sparse_matrix, label = output_vector, max.depth = 4, eta = 1, nthread = 2, nround = 4,objective = "binary:logistic")

importance <- xgb.importance(feature_names = sparse_matrix@Dimnames[[2]], model = bst)
head(importance)

importanceRaw <- xgb.importance(feature_names = sparse_matrix@Dimnames[[2]], model = bst, data = sparse_matrix, label = output_vector)
importanceClean <- importanceRaw[,`:=`(Cover=NULL, Frequency=NULL)]
head(importanceClean)

xgb.plot.importance(importance_matrix = importanceRaw)

c2 <- chisq.test(df$Age, output_vector)
print(c2)

c2 <- chisq.test(df$AgeDiscret, output_vector)
print(c2)

c2 <- chisq.test(df$AgeCat, output_vector)
print(c2)

# https://github.com/jpmml/jpmml-xgboost#the-xgboost-side-of-operations
# https://github.com/jpmml/r2pmml#package-xgboost

fmap = r2pmml::genFMap(as.data.frame(as.matrix(sparse_matrix)))
r2pmml::writeFMap(fmap, "xgboost.fmap")

xgb.dump(bst, "xgboost.model.txt", fmap = "xgboost.fmap")
r2pmml(bst, fmap = fmap, missing = NULL, "xgboost.pmml")
