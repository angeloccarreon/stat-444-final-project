# Read the data
data <- read.csv('C:/Users/hsmle/Downloads/AmesHousing.csv')

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

nn_val <- function(data, query_row_idx, query_var, cat_var, neighborhood=10) {
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
      if (is.na(a[[name]]) || is.na(b[[name]])) {
        return(1)
      }
      else if (name %in% cat_var) {
        if (a[[name]] != b[[name]]) {
          return(1)
        }
        else {
          return(0)
        }
      }
      else {
        return((a[[name]] - b[[name]]) ** 2)
      }
    }))
    return(score)
  })
  
  # The nearest neighbor is the one with the lowest distance score.
  nearest_neighbor <- sample(which(distances==min(distances)), 1)
  return(data[nearest_neighbor, query_var])
}
################################################################################
# preprocess: A function that preprocess data according to the pipeline:
# 1. Handle missing values. Specify whether to remove using remove.nas=TRUE.
# 2. Hand outliers. (Implement this!)
# 3. Dimensionality reduction.
# 4. Categorical processing (here, we just use as.numeric(as.factor(...))).

# Inputs:
# -> data: A data frame.
# -> remove.nas: A boolean specifying whether to remove NAs. Defaults to TRUE.
# Note: If FALSE, categorical NAs will be filled in using nearest neighbors with
# probability 2/3, or using the covariate mode with probability 1/3.
# -> min_var: A number specifying the cutoff for low variance filter (ignore for
# now).

preprocess <- function(data, remove.nas=TRUE, min_var=1) {
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
        if (name %in% cat_var) {
          # 2/3 chance that we will use nearest neighbors for categorical
          # covariates. 1/3 chance that we take the mode. This is to save
          # time computationally, as nn_val takes a while...
          data[idx,name] <- ifelse(rbinom(1, 1, 2/3), 
                                nn_val(data, idx, name, cat_var), 
                                mode(data[,name]))
        }
        else {
          data[idx,name] <- mean(data[,name], na.rm=TRUE)
        }
      }
    }
  }
  # Outliers (mainly applies for numerical covariates)
  
  ## DIMENSIONALITY REDUCTION
  # Low Variance Filter
  to_drop_2 <- c()
  # Not sure about this one...
  # for (name in colnames(data)) {
  #   if (!(name %in% cat_var) && var(data[,name]) < min_var) {
  #     to_drop_2 <- append(to_drop_2, name)
  #   }
  #   data <- data[, !(names(data) %in% to_drop_2)]
  # }
  
  # Categorical processing
  for (name in cat_var) {
    data[,name] <- as.numeric(as.factor(data[,name]))
  }
  
  # The artifacts to return
  return_list <- list(
    data=data,
    too_many_nas=to_drop_1,
    too_low_var=to_drop_2
  )
  
  return(return_list)
}

################################################################################
# cross_val: A function that performs cross-validation for linear regression on
# data using k folds.

# Inputs:
# -> data: A data frame.
# -> k: Number of folds to use for cross-validation. Defaults to 10.
cross_val <- function(data, k=10) {
  # Step 1: Divide data into folds
  data <- data[sample(1:nrow(data)), ]  # shuffle the data
  fold_len <- round(nrow(data) / k)  # size of each fold
  
  # Track the cross-validation error
  running_loss <- rep(0, k)
  
  # Step 2: Cross-validation loop
  for (fold in 1:k) {
    # Val fold indices
    val_start <- (fold - 1) * fold_len + 1
    val_end <- min(val_start + fold_len - 1, nrow(data))
    
    # Train and val folds
    val_fold = data[val_start:val_end, ]
    train_folds = data[-(val_start:val_end), ]
    
    # Fit the model on the training set
    model <- lm(y~x, data=train_folds)
    
    # Use the model to predict on validation fold; evaluate error on fold
    pred <- predict(model, newdata=val_fold)
    running_loss[fold] <- sum((pred - val_fold$y)^2)
  }
  
  # Step 3: Return the average of the running loss as the cross-validation error
  return(sum(running_loss) / nrow(data))
}

################################################################################

# Test out the cross-val function

set.seed(432798)
n <- 100
x <- runif(n,-1,1)
y <- rnorm(n,1 + 2*x,1)
plot(y~x)
abline(lm(y~x))
