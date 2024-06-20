library(mlr3verse)
library(mlr3)
library(dplyr)
library(dbplyr)
library(tidyr)
library(ggplot2)
library(mlr3learners)
library(xgboost)
library(e1071)

# dataset
cohort <- read.csv("final_cohort.csv")

# Cohort: Overall survival >= two years
cohort_os <- cohort %>% select(-c(X4156363,  #EFSSTAT, RFSSTAT
                                  X37208123, #EFSTM
                                  X40483363, #RFSTM
                                  X40482950, #OSTM
                                  X4014046,  #CR1
                                  person_id))

# target column as factor
#cohort_os$X44804077 = as.factor(cohort_os$X44804077)
#----------------------------------------------------------------- classification models
# tasks
task_os = as_task_classif(cohort_os, target = 'X44804077')

# train/test split
train_set_os = sample(task_os$row_ids, 0.67 * task_os$nrow)
test_set_os = setdiff(task_os$row_ids, train_set_os)

# metrics: Accuracy
measures = msrs(c('classif.acc','classif.auc', "classif.prauc", "classif.bbrier", "classif.fbeta"))

terminator = trm("evals", n_evals = 5)
inner_cv3 = rsmp("cv", folds = 3)

fselector = fs("random_search")
measure = msr("classif.prauc")


### MODEL: Random Forest

# hyperparameter
RF_lrn = lrn("classif.ranger", 
             predict_type = "prob",
             importance = "impurity",
             max.depth = 17, 
             seed = 12345 
             #mtry = as.integer(sqrt(length(task_os$feature_names)))
)


RF_tuner = auto_tuner(
  tuner = tnr("random_search"),
  learner = RF_lrn,
  resampling = inner_cv3,
  measure = measure,
  search_space = ps(num.trees = p_int(500,2000)),
  terminator = terminator
)

RF_class = AutoFSelector$new(
  learner = RF_tuner,
  resampling = rsmp("holdout"),
  measure = msr("classif.prauc"),
  terminator = terminator,
  fselector = fselector
)

#lrForest = lrn("classif.ranger", importance = "impurity")
RF_class$train(task_os, row_ids = train_set_os)
RF_pred_train = RF_class$predict(task_os, row_ids = train_set_os)
RF_pred_test = RF_class$predict(task_os, row_ids=test_set_os)

RF_pred_train$confusion
RF_pred_test$confusion
RF_pred_train$score(measures)
RF_pred_test$score(measures)
RF_pred_test$prob
RF_pred_train$response

# imporant features 
variab_filter = flt("importance", learner = RF_class)
variab_filter$calculate(task_os)
Important_features_OS = head(as.data.table(variab_filter), 15)


mapping_sheet <- read_excel("20240122_Mappings_sal.xlsx")

Important_features_OS$Concept_name <- sapply(Important_features_OS$feature, function(feature) {
  cleaned_feature <- gsub("^X", "", feature)
  match_idx <- which(mapping_sheet$Concept_id == cleaned_feature)
  
  if (length(match_idx) > 0) {
    return(mapping_sheet$Concept_name[match_idx])
  } else {
    return(NA)
  }
})


### MODEL:Single classification tree from package rpart.
#learner2 = lrn("classif.rpart")

tree_lrn = lrn("classif.rpart", 
               predict_type = "prob",
               keep_model = TRUE,
               maxcompete = 10, # between 0 and ∞
               maxdepth = 20 # between 1 and 30
)

tree_tuner = auto_tuner(
  tuner = tnr("random_search"),
  learner = tree_lrn,
  resampling = inner_cv3,
  measure = msr("classif.prauc"),
  terminator = terminator
)

tree_class = AutoFSelector$new(
  learner = tree_tuner,
  resampling = rsmp("holdout"),
  measure = msr("classif.prauc"),
  terminator = terminator,
  fselector = fselector
)

tree_class$train(task_os, row_ids = train_set_os)
tree_pred_train = tree_class$predict(task_os, row_ids=train_set_os)
tree_pred_test = tree_class$predict(task_os, row_ids=test_set_os)

tree_pred_train$confusion
tree_pred_test$confusion
tree_pred_train$score(measures)
tree_pred_test$score(measures)

### MODEL: Gradient Boosting
gboost_lrn = lrn("classif.xgboost", predict_type = "prob")
gboost_lrn$train(task_os, row_ids = train_set_os)
gboost_pred_train = gboost_lrn$predict(task_os, row_ids=train_set_os)
gboost_pred_test = gboost_lrn$predict(task_os, row_ids=test_set_os)

gboost_pred_train$confusion
gboost_pred_test$confusion
gboost_pred_train$score(measures)
gboost_pred_test$score(measures)

### MODEL: Support-Vector machines
svm_lrn = lrn("classif.svm", predict_type = "prob")
svm_lrn$train(task_os, row_ids = train_set_os)
svm_pred_train = svm_lrn$predict(task_os, row_ids=train_set_os)
svm_pred_test = svm_lrn$predict(task_os, row_ids=test_set_os)

svm_pred_train$confusion
svm_pred_test$confusion
svm_pred_train$score(measures)
svm_pred_test$score(measures)

# Define a custom print function with comments
print_model_performance <- function(model_name, pred_train, pred_test, measures) {
  cat("Model:", model_name, "\n")
  
  cat("\nTrain Set Metrics:\n")
  print(pred_train$score(measures))
  
  cat("\nTest Set Metrics:\n")
  print(pred_test$score(measures))
  cat("\n------------------------\n")
}

# Apply the function to each model with comments
print_model_performance("Random Forest", RF_pred_train, RF_pred_test, measures)
print_model_performance("Decision Tree", tree_pred_train, tree_pred_test, measures)
print_model_performance("Gradient Boosting", gboost_pred_train, gboost_pred_test, measures)
print_model_performance("Support-Vector Machines", svm_pred_train, svm_pred_test, measures)
print(Important_features_OS)
















