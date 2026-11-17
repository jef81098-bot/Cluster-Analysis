
# ============================================================
# HW: Classification of Dietary Habits and Obesity Status
# NHANES August 2021 - August 2023
# Models: kNN and Classification Tree
# ============================================================


# 1. Load packages
# ------------------------------------------------------------

library(haven)
library(tree)

set.seed(123)

# Set working directory
setwd("C:/Users/jef81/Desktop/Health_Big_Data/Cluster_Analysis")

# Create output folder
if (!dir.exists("output")) {
  dir.create("output")
}


# 2. Read NHANES data
# ------------------------------------------------------------

diet <- read_xpt("DR1TOT_L.xpt")
demo <- read_xpt("DEMO_L.xpt")
bmx  <- read_xpt("BMX_L.xpt")


# 3. Select variables
# ------------------------------------------------------------


diet_vars <- c(
  "SEQN", "DR1DRSTZ",
  
  # Energy
  "DR1TKCAL",
  
  # Macronutrients
  "DR1TPROT",
  "DR1TCARB",
  "DR1TSUGR",
  "DR1TFIBE",
  "DR1TTFAT",
  "DR1TCHOL",
  
  # Vitamins
  "DR1TATOC",
  "DR1TVARA",
  "DR1TVB1",
  "DR1TVB2",
  "DR1TVB6",
  "DR1TVB12",
  "DR1TVC",
  "DR1TVD",
  
  # Other dietary components
  "DR1TCAFF",
  "DR1TALCO",
  "DR1_320Z"
)

# Check variable names
stopifnot(all(diet_vars %in% names(diet)))


demo_vars <- c("SEQN", "RIDAGEYR")
bmx_vars  <- c("SEQN", "BMXBMI")

# Check whether all required variables exist
#stopifnot(all(diet_vars %in% names(diet)))
stopifnot(all(demo_vars %in% names(demo)))
stopifnot(all(bmx_vars %in% names(bmx)))

diet <- diet[, diet_vars]
demo <- demo[, demo_vars]
bmx  <- bmx[, bmx_vars]


# 4. Merge datasets
# ------------------------------------------------------------

data <- merge(diet, demo, by = "SEQN")
data <- merge(data, bmx, by = "SEQN")

# Keep adolescents aged 12-18
# Keep reliable Day 1 dietary recall
# Exclude missing BMI and zero-energy records

data <- data[
  data$RIDAGEYR >= 12 &
    data$RIDAGEYR <= 18 &
    data$DR1DRSTZ == 1 &
    !is.na(data$BMXBMI) &
    !is.na(data$DR1TKCAL) &
    data$DR1TKCAL > 0,
]

# Reset row names
rownames(data) <- NULL


# 5. Create BMI categories
# ------------------------------------------------------------

data$BMI_category <- cut(
  data$BMXBMI,
  breaks = c(-Inf, 18.5, 24, 27, Inf),
  labels = c(
    "Underweight",
    "Normal",
    "Overweight",
    "Obesity"
  ),
  right = FALSE
)

data$BMI_category <- factor(
  data$BMI_category,
  levels = c(
    "Underweight",
    "Normal",
    "Overweight",
    "Obesity"
  )
)

# Remove observations without a BMI category
data <- data[!is.na(data$BMI_category), ]

cat("Number of observations:", nrow(data), "\n")

# Check BMI category distribution
print(table(data$BMI_category))

write.csv(
  as.data.frame(table(data$BMI_category)),
  "output/BMI_distribution.csv",
  row.names = FALSE
)


# ============================================================
# 6. Dietary feature engineering
# ============================================================

# Energy
x <- data.frame(
  Energy = log1p(data$DR1TKCAL)
)

# Nutrients converted into density per 1000 kcal
nutrient_vars <- c(
  "DR1TPROT",
  "DR1TCARB",
  "DR1TSUGR",
  "DR1TFIBE",
  "DR1TTFAT",
  "DR1TCHOL",
  "DR1TATOC",
  "DR1TVARA",
  "DR1TVB1",
  "DR1TVB2",
  "DR1TVB6",
  "DR1TVB12",
  "DR1TVC",
  "DR1TVD"
)

for (v in nutrient_vars) {
  
  density <- data[[v]] /
    (data$DR1TKCAL / 1000)
  
  x[[paste0(v, "_density")]] <- log1p(density)
}

# Other dietary components: absolute intake
x$Caffeine <- log1p(data$DR1TCAFF)
x$Alcohol <- log1p(data$DR1TALCO)
x$Water <- log1p(data$DR1_320Z)

# Outcome remains unchanged
y <- data$BMI_category

summary(x)


# 7. Stratified train-test split
# ------------------------------------------------------------

# Split each BMI category separately to preserve
# approximately the same class proportions.

set.seed(123)

train_index <- unlist(
  lapply(levels(y), function(class_name) {
    
    idx <- which(y == class_name)
    
    n_train <- floor(length(idx) * 0.7)
    
    sample(idx, size = n_train)
    
  })
)

train_index <- sort(train_index)
test_index <- setdiff(seq_len(nrow(x)), train_index)

x_train <- x[train_index, , drop = FALSE]
x_test  <- x[test_index, , drop = FALSE]

y_train <- droplevels(y[train_index])
y_test  <- factor(
  y[test_index],
  levels = levels(y)
)

cat("\nTraining set:\n")
print(table(y_train))

cat("\nTesting set:\n")
print(table(y_test))


# 8. Missing value imputation function
# ------------------------------------------------------------

# Missing values are replaced by the median
# calculated from the training data only.

impute_median <- function(train_x, new_x) {
  
  medians <- sapply(
    train_x,
    median,
    na.rm = TRUE
  )
  
  medians[!is.finite(medians)] <- 0
  
  for (v in names(train_x)) {
    
    train_x[[v]][is.na(train_x[[v]])] <- medians[v]
    
    new_x[[v]][is.na(new_x[[v]])] <- medians[v]
    
  }
  
  list(
    train = train_x,
    new = new_x
  )
}



# ============================================================
# 9. Feature Selection
# ============================================================

cat("\n===== Feature Selection =====\n")

# 9.1 Impute missing values using training data only

imp_fs <- impute_median(x_train, x_test)

x_train <- imp_fs$train
x_test <- imp_fs$new


# 9.2 Variance filter

feature_var <- sapply(x_train, var)

variance_table <- data.frame(
  Variable = names(feature_var),
  Variance = as.numeric(feature_var)
)

# Remove zero or near-zero variance variables
keep_vars <- names(feature_var)[
  is.finite(feature_var) &
    feature_var > 1e-8
]

cat("\nVariables after variance filtering:",
    length(keep_vars), "\n")


# 9.3 Kruskal-Wallis test

kw_results <- data.frame(
  Variable = keep_vars,
  P_value = NA_real_
)

for (i in seq_along(keep_vars)) {
  
  v <- keep_vars[i]
  
  test <- kruskal.test(
    x_train[[v]] ~ y_train
  )
  
  kw_results$P_value[i] <- test$p.value
}

# Multiple testing correction
kw_results$FDR <- p.adjust(
  kw_results$P_value,
  method = "BH"
)

# Sort by statistical significance
kw_results <- kw_results[
  order(kw_results$P_value),
]

cat("\n===== Kruskal-Wallis Results =====\n")
print(kw_results)


# 9.4 Correlation filtering

cor_matrix <- cor(
  x_train[, keep_vars, drop = FALSE],
  use = "complete.obs"
)

# Rank variables by Kruskal-Wallis p-value
ranked_vars <- kw_results$Variable

selected_vars <- character(0)

for (v in ranked_vars) {
  
  if (length(selected_vars) == 0) {
    
    selected_vars <- c(selected_vars, v)
    
  } else {
    
    correlations <- abs(
      cor_matrix[v, selected_vars]
    )
    
    if (all(correlations < 0.85)) {
      selected_vars <- c(selected_vars, v)
    }
  }
}

cat("\nVariables after correlation filtering:",
    length(selected_vars), "\n")


# 9.5 Final feature selection

# Prefer variables with FDR < 0.10
final_vars <- selected_vars[
  kw_results$FDR[
    match(selected_vars, kw_results$Variable)
  ] < 0.10
]

# If fewer than 5 variables remain,
# retain the top 5 ranked variables
if (length(final_vars) < 5) {
  
  final_vars <- head(selected_vars, 5)
  
}

# Limit the number of selected variables to 10
if (length(final_vars) > 10) {
  
  final_vars <- head(final_vars, 10)
  
}

cat("\n===== Selected Variables =====\n")
print(final_vars)

cat("\nNumber of selected variables:",
    length(final_vars), "\n")


# 9.6 Apply selected variables

x_train <- x_train[
  , final_vars, drop = FALSE
]

x_test <- x_test[
  , final_vars, drop = FALSE
]


# 9.7 Save feature selection results

write.csv(
  variance_table,
  "output/feature_variance.csv",
  row.names = FALSE
)

write.csv(
  kw_results,
  "output/feature_selection_kruskal.csv",
  row.names = FALSE
)

write.csv(
  data.frame(Selected_Variable = final_vars),
  "output/selected_features.csv",
  row.names = FALSE
)

write.csv(
  cor_matrix,
  "output/feature_selection_correlation.csv"
)

cat("\nFeature selection completed.\n")


# 9. Evaluation function
# ------------------------------------------------------------

evaluate_model <- function(pred, actual) {
  
  class_names <- levels(actual)
  
  pred <- factor(pred, levels = class_names)
  actual <- factor(actual, levels = class_names)
  
  cm <- table(
    Predicted = pred,
    Actual = actual
  )
  
  accuracy <- sum(diag(cm)) / sum(cm)
  
  precision <- numeric(length(class_names))
  recall <- numeric(length(class_names))
  f1 <- numeric(length(class_names))
  
  for (i in seq_along(class_names)) {
    
    TP <- cm[i, i]
    FP <- sum(cm[i, ]) - TP
    FN <- sum(cm[, i]) - TP
    
    precision[i] <- ifelse(
      TP + FP == 0,
      0,
      TP / (TP + FP)
    )
    
    recall[i] <- ifelse(
      TP + FN == 0,
      0,
      TP / (TP + FN)
    )
    
    f1[i] <- ifelse(
      precision[i] + recall[i] == 0,
      0,
      2 * precision[i] * recall[i] /
        (precision[i] + recall[i])
    )
    
  }
  
  metrics <- data.frame(
    Accuracy = accuracy,
    Macro_Precision = mean(precision),
    Macro_Recall = mean(recall),
    Macro_F1 = mean(f1),
    Balanced_Accuracy = mean(recall)
  )
  
  list(
    confusion_matrix = cm,
    metrics = metrics
  )
}


# ============================================================
# 10. k-Nearest Neighbors (kNN)
# ============================================================

# 10.1 Create stratified cross-validation folds

make_folds <- function(y, v = 5) {
  
  folds <- vector("list", v)
  
  for (class_name in levels(y)) {
    
    idx <- sample(which(y == class_name))
    
    if (length(idx) < v) {
      stop("At least 5 observations per class are needed for 5-fold CV.")
    }
    
    split_idx <- split(
      idx,
      rep(seq_len(v), length.out = length(idx))
    )
    
    for (j in seq_len(v)) {
      folds[[j]] <- c(folds[[j]], split_idx[[j]])
    }
    
  }
  
  folds
}


# 10.2 Tune k using 5-fold cross-validation

set.seed(123)

folds <- make_folds(y_train, v = 5)

k_candidates <- c(3, 5, 7, 9, 11, 15, 21)

# Only consider k values smaller than the
# training sample size in each CV fold.

min_fold_train <- min(
  sapply(folds, function(valid_idx) {
    nrow(x_train) - length(valid_idx)
  })
)

k_candidates <- k_candidates[
  k_candidates < min_fold_train
]

cv_results <- data.frame()

for (k in k_candidates) {
  
  fold_f1 <- numeric(length(folds))
  
  for (i in seq_along(folds)) {
    
    valid_idx <- folds[[i]]
    train_idx <- setdiff(
      seq_len(nrow(x_train)),
      valid_idx
    )
    
    x_tr <- x_train[train_idx, , drop = FALSE]
    x_val <- x_train[valid_idx, , drop = FALSE]
    
    y_tr <- y_train[train_idx]
    y_val <- y_train[valid_idx]
    
    # Imputation based on training fold
    imp <- impute_median(x_tr, x_val)
    
    x_tr <- imp$train
    x_val <- imp$new
    
    # Standardization based on training fold
    train_mean <- sapply(x_tr, mean)
    train_sd <- sapply(x_tr, sd)
    
    train_sd[!is.finite(train_sd) | train_sd == 0] <- 1
    
    x_tr_scaled <- scale(
      x_tr,
      center = train_mean,
      scale = train_sd
    )
    
    x_val_scaled <- scale(
      x_val,
      center = train_mean,
      scale = train_sd
    )
    
    pred <- class::knn(
      train = x_tr_scaled,
      test = x_val_scaled,
      cl = y_tr,
      k = k
    )
    
    result <- evaluate_model(pred, y_val)
    
    fold_f1[i] <- result$metrics$Macro_F1
    
  }
  
  cv_results <- rbind(
    cv_results,
    data.frame(
      k = k,
      Mean_Macro_F1 = mean(fold_f1)
    )
  )
  
}

print(cv_results)

write.csv(
  cv_results,
  "output/knn_tuning.csv",
  row.names = FALSE
)


# 10.3 Select the best k

best_k <- cv_results$k[
  which.max(cv_results$Mean_Macro_F1)
]

cat("\nBest k:", best_k, "\n")


# 10.4 Fit final kNN model and predict test set

imp_knn <- impute_median(x_train, x_test)

x_train_knn <- imp_knn$train
x_test_knn <- imp_knn$new

train_mean <- sapply(x_train_knn, mean)
train_sd <- sapply(x_train_knn, sd)

train_sd[!is.finite(train_sd) | train_sd == 0] <- 1

x_train_scaled <- scale(
  x_train_knn,
  center = train_mean,
  scale = train_sd
)

x_test_scaled <- scale(
  x_test_knn,
  center = train_mean,
  scale = train_sd
)

set.seed(123)

knn_pred <- class::knn(
  train = x_train_scaled,
  test = x_test_scaled,
  cl = y_train,
  k = best_k
)

knn_result <- evaluate_model(knn_pred, y_test)

cat("\n===== kNN Confusion Matrix =====\n")
print(knn_result$confusion_matrix)

cat("\n===== kNN Performance =====\n")
print(knn_result$metrics)

write.csv(
  as.data.frame.matrix(knn_result$confusion_matrix),
  "output/knn_confusion_matrix.csv"
)


# ============================================================
# 11. Classification Tree
# ============================================================

# 11.1 Impute missing values

imp_tree <- impute_median(x_train, x_test)

x_train_tree <- imp_tree$train
x_test_tree <- imp_tree$new

tree_train <- data.frame(
  BMI_category = y_train,
  x_train_tree
)

tree_test <- x_test_tree


# 11.2 Fit classification tree

set.seed(123)

tree_model <- tree::tree(
  BMI_category ~ .,
  data = tree_train
)

cat("\n===== Original Tree Summary =====\n")
summary(tree_model)


# 11.3 Plot original tree

png(
  "output/classification_tree_original.png",
  width = 1200,
  height = 900,
  res = 120
)

plot(tree_model)
text(tree_model, pretty = 0, cex = 0.7)

dev.off()


# 11.4 Cross-validation for tree pruning

set.seed(123)

cv_tree <- tree::cv.tree(
  tree_model,
  FUN = prune.tree,
  K = 10,
  method = "misclass"
)

print(cv_tree)


# 11.5 Select the optimal tree and prune

best_size <- cv_tree$size[which.min(cv_tree$dev)]

cat("Best tree size:", best_size, "\n")

tree_pruned <- tree::prune.tree(
  tree_model,
  best = best_size,
  method = "misclass"
)

cat("\n===== Pruned Tree Summary =====\n")
print(summary(tree_pruned))

# 11.6 Compare original and pruned trees

# Original tree prediction
tree_pred_original <- predict(
  tree_model,
  newdata = tree_test,
  type = "class"
)

tree_original_result <- evaluate_model(
  tree_pred_original,
  y_test
)

# Check whether pruning produced a single terminal node.
is_single_node <- nrow(tree_pruned$frame) == 1L

if (is_single_node) {
  # A single-node tree predicts the majority class for every observation.
  majority_class <- names(which.max(table(y_train)))

  tree_pred_pruned <- factor(
    rep(majority_class, nrow(tree_test)),
    levels = levels(y_train)
  )
} else {
  # If the pruned tree still has splits, use the regular predict() method.
  tree_pred_pruned <- predict(
    tree_pruned,
    newdata = tree_test,
    type = "class"
  )

  tree_pred_pruned <- factor(
    tree_pred_pruned,
    levels = levels(y_train)
  )
}

# Evaluate pruned tree
tree_pruned_result <- evaluate_model(
  tree_pred_pruned,
  y_test
)

cat("\n===== Pruned Tree Performance =====\n")
print(tree_pruned_result$metrics)

cat("\n===== Pruned Tree Confusion Matrix =====\n")
print(tree_pruned_result$confusion_matrix)

tree_pruned_result <- evaluate_model(
  tree_pred_pruned,
  y_test
)

cat("\n===== Original Tree Performance =====\n")
print(tree_original_result$metrics)

cat("\n===== Pruned Tree Performance =====\n")
print(tree_pruned_result$metrics)

cat("\n===== Original Tree Confusion Matrix =====\n")
print(tree_original_result$confusion_matrix)

cat("\n===== Pruned Tree Confusion Matrix =====\n")
print(tree_pruned_result$confusion_matrix)

# Compare both models
tree_comparison <- rbind(
  Original_Tree = tree_original_result$metrics,
  Pruned_Tree = tree_pruned_result$metrics
)

print(tree_comparison)


# 11.6 Plot pruned tree

png(
  "output/classification_tree_pruned.png",
  width = 1200,
  height = 900,
  res = 120
)

if (is_single_node) {
  # plot.tree() may fail for a single-node tree.
  # Draw a simple explanatory figure instead.
  plot.new()
  title(main = "Pruned Classification Tree")
  text(
    x = 0.5,
    y = 0.58,
    labels = paste0(
      "Single terminal node\n",
      "Predicted class: ", majority_class
    ),
    cex = 1.4
  )
} else {
  plot(tree_pruned)
  text(tree_pruned, pretty = 0, cex = 0.7)
}

dev.off()


# 11.7 Predict test set

# Reuse the predictions already generated above.
# This avoids calling predict() again on a single-node tree.
tree_pred <- factor(
  tree_pred_pruned,
  levels = levels(y_test)
)

tree_result <- evaluate_model(tree_pred, y_test)

cat("\n===== Classification Tree Confusion Matrix =====\n")
print(tree_result$confusion_matrix)

cat("\n===== Classification Tree Performance =====\n")
print(tree_result$metrics)

write.csv(
  as.data.frame.matrix(tree_result$confusion_matrix),
  "output/tree_confusion_matrix.csv"
)


# ============================================================
# 12. Compare model performance
# ============================================================

model_comparison <- rbind(
  
  data.frame(
    Model = "kNN",
    knn_result$metrics
  ),
  
  data.frame(
    Model = "Classification Tree",
    tree_result$metrics
  )
  
)

print(model_comparison)

write.csv(
  model_comparison,
  "output/model_comparison.csv",
  row.names = FALSE
)


# ============================================================
# 13. Save final results
# ============================================================

cat("\n====================================\n")
cat("Analysis completed successfully!\n")
cat("====================================\n")

cat("\nOutput files:\n")
print(list.files("output"))
