library(glmnet)
data <- read.csv(file="house-prices-advanced-regression-techniques/AmesHousing.csv")

# Process training set, remove NAs, log-transform response variable
processed <- preprocess(data, remove.na = FALSE)
data <- processed$data

# Training/Testing split (80/20 split)
set.seed(20794587)
trainInd <- sample(1:nrow(data), round(nrow(data)*0.8), replace=F)
train <- data[trainInd,]
test <- data[-trainInd,]

X <- as.matrix(train, ncol=20)[,-20]
Y <- as.matrix(train, ncol=20)[,20]

testX = as.matrix(test, ncol=20)[,-20]
testY = as.matrix(test, ncol=20)[,20]

# Create a single LASSO and RIDGE model
lasso <- glmnet(X, Y, type.measure = "mse", alpha=1)
lasso.preds = predict(lasso, newx = testX, type = "response")
mean((testY - lasso.preds)^2)

ridge <- glmnet(X, Y, type.measure = "mse", alpha=0)
ridge.preds = predict(ridge, newx = testX, type = "response")
mean((testY - ridge.preds)^2)

# Create LASSO and RIDGE models using CV
lasso <- cv.glmnet(X, Y, type.measure = "mse", nfolds = 20, alpha=1)
print(lasso)

ridge <- cv.glmnet(X, Y, type.measure = "mse", nfolds = 20, alpha=0)
print(ridge)

lasso.preds = predict(lasso, newx = testX, type = "response")
mean((testY - lasso.preds)^2)

ridge.preds = predict(ridge, newx = testX, type = "response")
mean((testY - ridge.preds)^2)

# Exponentiate the sale price to restore the sale price
# The average absolute difference in sale price is $22035.80!
mean(abs(exp(preds) - exp(testY)))

