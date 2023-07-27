# Read the data
data <- read.csv('house-prices-advanced-regression-techniques/AmesHousing.csv')
housing_raw = read.csv('house-prices-advanced-regression-techniques/AmesHousing.csv')

# Set the seed for any random ops
set.seed(69420)

################################################################################
# nn_val: Helper function to get nearest neighbor value for a query variable.
# Uses a random sample of the data frame to fill in the NA value at 
# data[query_row_idx, query_var].

# Inputs:
# -> data: A data frame.
# -> query_row_idx: The index of the row for which we want to fill in an NA.
# -> query_var: The variable in the row that contains the NA.
# -> cat_var: A list of the categorical covariates in data.
# -> neighborhood: The size of the random sample to use to help fill in the NA.

# Returns:
# -> Predicted value for data[query_row_idx, query_var] according to nearest
# neighbor.

nn_val <- function(data, query_row_idx, query_var, cat_var, neighborhood=10) {
  # In case the nearest neighbors algorithm returns NA.
  backup_val <- mode(data[,query_var])
  
  # Select a small random sample of data to act as neighbors
  data <- data[sample(nrow(data), neighborhood),]
  
  # Extract query row and remove it from the dataset
  query_row <- data[query_row_idx,]
  other_data <- data[-query_row_idx,]
  
  # Compute distance scores between the query and each neighbor
  distances <- sapply(seq(nrow(other_data)), function(idx) {
    other <- other_data[idx,]
    # Let a denote the row for which we want to fill in an NA value.
    # Let b denote another row in the given dataframe.
    a <- query_row[!(names(data) %in% c(query_var, "SalePrice"))]
    b <- other[!(names(data) %in% c(query_var, "SalePrice"))]
    score <- sum(sapply(names(a), function(name) {
      # If the covariate is NA for either query or other
      if (is.na(a[[name]]) || is.na(b[[name]])) {
        return(1)
      }
      # If the covariate is categorical, increment the distance score if the
      # values are different.
      else if (name %in% cat_var) {
        if (a[[name]] != b[[name]]) {
          return(1)
        }
        else {
          return(0)
        }
      }
      # If the covariate is numeric
      else {
        return((a[[name]] - b[[name]]) ** 2)
      }
    }))
    return(score)
  })
  
  # The nearest neighbor is the one with the lowest distance score.
  nearest_neighbor <- sample(which(distances==min(distances)), 1)
  pred <- data[nearest_neighbor, query_var]
  return(ifelse(is.na(pred), backup_val, pred))
}

################################################################################
# Utility functions:
is_sparse <- function(covariate) {
  return(sum(covariate==0)/length(covariate) > 0.5)
}

has_outliers <- function(covariate) {
  covar_std <- sd(covariate)
  within3std <- (-3*covar_std <= covariate) * (covariate <= 3*covar_std)
  return(sum(within3std) < length(covariate))
}

apply_util_to_df <- function(df, util_func) {
  attrs <- c()
  for (name in colnames(df)) {
    if (util_func(df[,name])) {
      attrs <- append(attrs, name)
    }
  }
  return(attrs)
}

################################################################################
# preprocess: A function that preprocess data according to the pipeline:
# 1. Handle missing values. Specify whether to remove using remove.nas=TRUE.
# 2. Dimensionality reduction.
# 3. Categorical processing (here, we just use as.numeric(as.factor(...))).

# Inputs:
# -> data: A data frame.
# -> remove.nas: A boolean specifying whether to remove NAs. Defaults to TRUE.
# Note: If FALSE, categorical NAs will be filled in using nearest neighbors with
# probability 2/3, or using the covariate mode with probability 1/3.
# -> min_var: A number specifying the cutoff for low variance filter (ignore for
# now).

# Imports
library(CORElearn)

preprocess <- function(data, remove.nas=TRUE, ig_cap=10) {
  # Removing some covariates by inspection:
  # -> "Order"
  # -> "PID"
  data <- subset(data, select=-c(Order, PID))
  # data <- subset(data, select=-c(Id))
  
  # PRELIMINARY FEATURE ENGINEERING
  # -> Scale down the year built to start at 0.
  # -> Transform Year.Remod.Add <- Year.Remod.Add - Year.Built.
  # -> Transform Garage.Yr.Blt <- Garage.Yr.Blt - Year.Built.
  data$Year.Remod.Add <- data$Year.Remod.Add - data$Year.Built
  data$Garage.Yr.Blt <- data$Garage.Yr.Blt - data$Year.Built
  data$Year.Built <- data$Year.Built - min(data$Year.Built, na.rm=TRUE)
  
  ## MISSING VALUES
  # Take out any covariates that are missing a lot of values, as it is not
  # feasible to use them for analysis.
  to_drop_1 <- c()
  for (name in colnames(data)) {
    # If more than half of the observations for the covariate are NA, drop it
    if (sum(is.na(data[name])) > 0.5 * nrow(data)) {
      to_drop_1 <- append(to_drop_1, name)
    }
  }
  data <- data[, !(names(data) %in% to_drop_1)]
  
  # Get list of numerical and categorical covariates
  cat_var <- c()
  num_var <- c()
  for (name in names(data)) {
    if (class(data[[name]]) == 'character') {
      cat_var <- append(cat_var, name)
    }
    else {
      num_var <- append(num_var, name)
    }
  }
  
  # Handle the remaining NAs
  if (remove.nas) {
    data <- na.omit(data)
  }
  else {  # In the case where we don't remove the NAs
    na_idx <- unique(which(is.na(data), arr.ind=TRUE)[,1])
    for (idx in na_idx) {
      # If covariate is categorical, use nearest neighbors.
      # If covariate is numerical, use mean value.
      for (name in names(data)) {
        if (is.na(data[idx,name])) {
          if (name %in% cat_var) {
            # 2/3 chance that we will use nearest neighbors for categorical
            # covariates. 1/3 chance that we take the mode. This is to save
            # time computationally, as nn_val takes a while...
            data[idx,name] <- ifelse(rbinom(1, 1, 2/3), 
                                     nn_val(data, idx, name, cat_var), 
                                     mode(na.omit(data[,name])))
          }
          else {
            data[idx,name] <- mean(data[,name], na.rm=TRUE)
          }
        }
      }
    }
  }
  
  ## DIMENSIONALITY REDUCTION
  
  # CATEGORICAL PROCESSING
  # Idea: Use the categorical covariates with the highest information gain.
  ig <- attrEval(SalePrice~., data=subset(data, select=c(cat_var, "SalePrice")), 
                 estimator='InfGain')
  ig <- sort(ig, decreasing=TRUE)
  low_ig_attrs <- names(ig[ig_cap+1:length(ig)])
  data <- data[, !(names(data) %in% low_ig_attrs)]
  
  cat_var <- cat_var[!(cat_var %in% low_ig_attrs)]
  for (name in cat_var) {
    data[,name] <- as.numeric(as.factor(data[,name]))
  }
  
  # NUMERIC COLUMNS PROCESSING
  # Extract numerical columns from the dataset
  num_cols <- data[ , num_var]
  
  # Compute the correlation matrix
  cor_matrix <- cor(num_cols, method='pearson', use='pairwise.complete.obs')
  diag(cor_matrix) <- 0
  
  cor_threshold <- 0.8
  
  # Create a vector to keep track of the selected features
  features_to_remove <- character(0)
  
  #  one representative from each group of highly correlated features
  for (i in 1:(ncol(cor_matrix) - 1)) {
    if (!(colnames(cor_matrix)[i] %in% features_to_remove)) {
      highly_correlated <- colnames(cor_matrix)[cor_matrix[, i] >= 
                                                  cor_threshold]
      features_to_remove <- c(features_to_remove, highly_correlated[1])
    }
  }
  to_drop_2 <- features_to_remove[!is.na(features_to_remove)]
  
  cat("Columns dropped:", paste(to_drop_2, collapse = ", "), "\n")
  data <- data[, !(names(data) %in% to_drop_2)]
  
  # Calculate correlation between each column and the target
  correlations <- cor(num_cols[, -which(names(num_cols) == "SalePrice")], 
                      num_cols$SalePrice)
  
  # Find covariates with correlation below 0.4
  below_indices <- which(abs(correlations) < 0.4, arr.ind = TRUE)

  to_drop_3 <- rownames(correlations)[below_indices[, 1]]
  
  cat("Columns dropped:", paste(to_drop_3, collapse = ", "), "\n")
  data <- data[, !(names(data) %in% to_drop_3)]
  
  data$SalePrice <- log(data$SalePrice)
  
  # The artifacts to return
  return_list <- list(
    data=data,
    too_many_nas=to_drop_1,
    too_high_corr=to_drop_2,
    too_low_relation=to_drop_3,
    cat_var=cat_var
  )
  
  return(return_list)
}

################################################################################
# cross_val: A general-purpose k-fold cross-validation function.

# Inputs:
# -> data: A data frame.
# -> k: Number of folds to use for cross-validation. Defaults to 10.
# -> test_prop: Proportion of dataset to use as a final testing set. The
# remaining data points will be used as train/val.
# -> linear_model: Boolean specifying whether the cross-validated model is
# linear. Defaults to TRUE (i.e. linear assumption by default).

# Returns:
# -> best_model: The best model resulting from k-fold cross-validation.
# -> metric: Value of performance metric used on best_model and the test set.

# Imports
library(ICglm)
library(splines)
library(gam)
library(glmnet)

get_s <- function(data, attr) {
  return(s(data[,attr], k=length(unique(data[,attr]))))
}

data <- preprocess(data, remove.na=FALSE)$data

test_idx <- sample(1:nrow(data), round(nrow(data)*0.2))
test_set <- data[test_idx, ]
train_set <- data[-test_idx, ]

cross_val <- function(test_set, data, k, linear_model=TRUE) {
  
  loss.mlr <- 0
  loss.gam <- 0
  loss.lasso <- 0
  loss.ridge <- 0

  # Compute standard cross-validation for sanity check
  fold_len <- round(nrow(data) / k)
  for (fold in 1:k) {
    val_start <- (fold-1)*fold_len + 1
    val_end <- min(val_start + fold_len - 1, nrow(data))
    val_fold <- data[val_start:val_end,]
    train_folds <- data[-(val_start:val_end),]
    
    # Naive MLR Model
    model.mlr <- lm(SalePrice~., data=train_folds)
    pred.mlr <- predict(model.mlr, newdata=val_fold)
    loss.mlr <- loss.mlr + sum((pred.mlr-val_fold$SalePrice)**2)
    
    # GAM Model
    formula <- SalePrice ~
      s(Neighborhood) +
      s(House.Style) +
      s(Overall.Qual) +
      s(Year.Built) +
      s(Exterior.1st) +
      s(Exterior.2nd) +
      s(Mas.Vnr.Area) +
      s(Foundation) +
      s(Bsmt.Qual) +
      s(BsmtFin.Type.1) +
      s(BsmtFin.SF.1) +
      s(Total.Bsmt.SF) +
      s(Gr.Liv.Area) +
      s(Full.Bath) +
      s(Kitchen.Qual) +
      s(Fireplaces) +
      s(Fireplace.Qu) +
      s(Garage.Finish) +
      s(Garage.Cars)
    
    model.gam = gam(formula, data=data)
    pred.gam <- predict(model.gam, newdata=val_fold)
    loss.gam <- loss.gam + sum((pred.gam-val_fold$SalePrice)**2)
    
    # Structure data for linear regression models
    X <- as.matrix(train_folds, ncol=20)[,-20]
    Y <- as.matrix(train_folds, ncol=20)[,20]
    
    testX = as.matrix(val_fold, ncol=20)[,-20]
    testY = as.matrix(val_fold, ncol=20)[,20]
    
    # Lasso Model
    model.lasso <- cv.glmnet(X, Y, type.measure = "mse", alpha=1)
    pred.lasso = predict(model.lasso, newx = testX, type = "response")
    loss.lasso = loss.lasso + sum((testY - pred.lasso)^2)
    
    # Ridge Model
    model.ridge <- cv.glmnet(X, Y, type.measure = "mse", alpha=0)
    pred.ridge = predict(model.ridge, newx = testX, type = "response")
    loss.ridge = loss.ridge + sum((testY - pred.ridge)^2)
  }

  loss.mlr <- loss.mlr / nrow(data)
  loss.gam = loss.gam / nrow(data)
  loss.lasso = loss.lasso / nrow(data)
  loss.ridge = loss.ridge / nrow(data)

  # Calculate final test errors for the models
  mlr_model <- lm(SalePrice~., data=data)
  pred.mlr <- predict(model.mlr, newdata=test_set)
  testerr.mlr <- mean((pred.mlr-test_set$SalePrice)^2)
  
  formula <- SalePrice ~
    s(Neighborhood) +
    s(House.Style) +
    s(Overall.Qual) +
    s(Year.Built) +
    s(Exterior.1st) +
    s(Exterior.2nd) +
    s(Mas.Vnr.Area) +
    s(Foundation) +
    s(Bsmt.Qual) +
    s(BsmtFin.Type.1) +
    s(BsmtFin.SF.1) +
    s(Total.Bsmt.SF) +
    s(Gr.Liv.Area) +
    s(Full.Bath) +
    s(Kitchen.Qual) +
    s(Fireplaces) +
    s(Fireplace.Qu) +
    s(Garage.Finish) +
    s(Garage.Cars)
  
  gam_model = gam(formula, data=data)
  
  # Create matrices for linear regression functions
  X <- as.matrix(data, ncol=20)[,-20]
  Y <- as.matrix(data, ncol=20)[,20]
  
  testX = as.matrix(test_set, ncol=20)[,-20]
  testY = as.matrix(test_set, ncol=20)[,20]
  
  # Final Lasso Model
  model.lasso <- cv.glmnet(X, Y, type.measure = "mse", alpha=1)
  pred.lasso = predict(model.lasso, newx = testX, type = "response")
  testerr.lasso = mean((pred.lasso - testY)^2)
  
  # Ridge Model
  model.ridge <- cv.glmnet(X, Y, type.measure = "mse", alpha=0)
  pred.ridge = predict(model.ridge, newx = testX, type = "response")
  testerr.ridge = mean((pred.ridge - testY)^2)
  
  # Compute predictions of model on test set
  pred <- predict(gam_model, test_set, type="response")
  testerr.gam <- mean((pred-test_set$SalePrice)**2)
  
  return(list(
    loss.mlr = loss.mlr,
    loss.gam = loss.gam,
    loss.lasso = loss.lasso,
    loss.ridge = loss.ridge,
    testerr.mlr = testerr.mlr,
    testerr.gam = testerr.gam,
    testerr.lasso = testerr.lasso,
    testerr.ridge = testerr.ridge
  ))
}

cross_val(test_set, train_set, k=20)

################################################################################

# Plot the GAM smooths
formula <- SalePrice ~
  s(Neighborhood) +
  s(House.Style) +
  s(Overall.Qual) +
  s(Year.Built) +
  s(Exterior.1st) +
  s(Exterior.2nd) +
  s(Mas.Vnr.Area) +
  s(Foundation) +
  s(Bsmt.Qual) +
  s(BsmtFin.Type.1) +
  s(BsmtFin.SF.1) +
  s(Total.Bsmt.SF) +
  s(Gr.Liv.Area) +
  s(Full.Bath) +
  s(Kitchen.Qual) +
  s(Fireplaces) +
  s(Fireplace.Qu) +
  s(Garage.Finish) +
  s(Garage.Cars)

gam_model = gam(formula, data=train_set)
preds = predict(gam_model, newdata=test_set)
mean(sqrt((exp(preds) - exp(test_set$SalePrice))^2))

par(mfrow=c(3, 7))
plot(gam_model, residuals=TRUE, col='grey')