library(mlr3)
library(mlr3verse)
library(dplyr)
library(dbplyr)
library(tidyr)
library(ggplot2)
library(mlr3learners)
library(xgboost)
library(readxl)
library(tidyr)

# dataset
cohort <- read.csv("final_cohort.csv")

#colnames(cohort)

# Cohort: Complete remission
cohort_cr <- cohort %>% select(-c(X4156363,  #EFSSTAT, RFSSTAT
                                  X37208123, #EFSTM
                                  X40483363, #RFSTM
                                  X44804077, #OSSTAT2Years
                                  X40482950, #OSTM
                                  person_id))

# target column as factor
cohort_cr$X4014046 = as.factor(cohort_cr$X4014046)
#----------------------------------------------------------------- classification models

# definition of the classification task 
task_cr = as_task_classif(cohort_cr, target = 'X4014046')

# train/test split definition
train_set_cr = sample(task_cr$row_ids, 0.67 * task_cr$nrow)
test_set_cr = setdiff(task_cr$row_ids, train_set_cr)

# metrics: Accuracy
measures = msrs(c('classif.acc','classif.auc', "classif.prauc", "classif.bbrier", "classif.fbeta"))

terminator = trm("evals", n_evals = 5)
inner_cv3 = rsmp("cv", folds = 3)


### MODEL: Random Forest

fselector = fs("random_search")
measure = msr("classif.prauc")

# hyperparameter
RF_lrn = lrn("classif.ranger", 
             importance = "impurity",
             predict_type = "prob", 
             max.depth = 17, 
             seed = 12345 #,
             #mtry = as.integer(sqrt(length(task_cr$feature_names)))
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

RF_class$train(task_cr, row_ids = train_set_cr)
RF_pred_train = RF_class$predict(task_cr, row_ids = train_set_cr)

# ------------------------------------------------------------------RF model test START
RF_pred_test = RF_class$predict(task_cr, row_ids=test_set_cr)
# ------------------------------------------------------------------RF model test END

#RF_pred_train$confusion
#RF_pred_test$confusion
RF_pred_train$score(measures)
RF_pred_test$score(measures)
#RF_pred_test$prob
#RF_pred_train$response

#--------------------------------------------------------------- important features START
variab_filter = flt("importance", learner = RF_lrn)
variab_filter$calculate(task_cr)
Important_features_CR = head(as.data.table(variab_filter), 15)

mapping_sheet <- read_excel("20240122_Mappings_sal.xlsx", sheet = "Mappings")

Important_features_CR$Concept_name <- sapply(Important_features_CR$feature, function(feature) {
  cleaned_feature <- gsub("^X", "", feature)
  match_idx <- which(mapping_sheet$Concept_id == cleaned_feature)
  
    if (length(match_idx) > 0) {
    return(mapping_sheet$Concept_name[match_idx])
  } else {
    return(NA)
  }
})

#--------------------------------------------------------------- important features END

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

tree_class$train(task_cr, row_ids = train_set_cr)
tree_pred_train = tree_class$predict(task_cr, row_ids=train_set_cr)

# ------------------------------------------------------------------CT model test START
tree_pred_test = tree_class$predict(task_cr, row_ids=test_set_cr)
# ------------------------------------------------------------------CT model test END

tree_pred_train$confusion
tree_pred_test$confusion
tree_pred_train$score(measures)
tree_pred_test$score(measures)

### MODEL: Gradient Boosting
gboost_lrn = lrn("classif.xgboost", predict_type = "prob")
gboost_lrn$train(task_cr, row_ids = train_set_cr)
gboost_pred_train = gboost_lrn$predict(task_cr, row_ids=train_set_cr)

# ------------------------------------------------------------------GBM model test START
gboost_pred_test = gboost_lrn$predict(task_cr, row_ids=test_set_cr)
# ------------------------------------------------------------------GBM model test END

gboost_pred_train$confusion
gboost_pred_test$confusion
gboost_pred_train$score(measures)
gboost_pred_test$score(measures)

### MODEL: Support-Vector machines
svm_lrn = lrn("classif.svm", predict_type = "prob")
svm_lrn$train(task_cr, row_ids = train_set_cr)
svm_pred_train = svm_lrn$predict(task_cr, row_ids=train_set_cr)

# ------------------------------------------------------------------SVM model test START
svm_pred_test = svm_lrn$predict(task_cr, row_ids=test_set_cr)
# ------------------------------------------------------------------SVM model test END

svm_pred_train$confusion
svm_pred_test$confusion
svm_pred_train$score(measures)
svm_pred_test$score(measures)

# Print the feature importance
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
print(Important_features_CR)

save.image(file = "AML study.RData")









