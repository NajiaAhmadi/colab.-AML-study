library(mlr3verse)
library(mlr3)
library(dplyr)
library(dbplyr)
library(tidyr)
library(ggplot2)
library(mlr3learners)
library(xgboost)
library(e1071)
library(mlr3learners)
#install.packages("FSelector")
library("FSelector")
#install.packages("mlr3fselect")
library(mlr3fselect)

library(readxl)

# dataset
cohort <- read.csv("final_cohort.csv")


# Cohort: Overall survival >= two years

cohort_os <- cohort %>% select(-c(X4156363,  #EFSSTAT, RFSSTAT
                                  X37208123, #EFSTM
                                  X40483363, #RFSTM
                                  X40482950, #OSTM
                                  X4014046,  #CR1
                                  person_id,
                                  # the following columns does not exist in the US data: remove from the train set as well. 
                                  X37312067, #CEBPA
                                  X40483363, #RFSTM
                                  X35944932, #BCORL1
                                  X35945674, #MYD88
                                  X35948718, #SF3B1
                                  X35950932, #FBXW7
                                  X35954356, #CUX1
                                  X35954738, #GATA1
                                  X35954887, #ATRX
                                  X35955105, #CBL
                                  X35955125, #PTEN
                                  X35955661, #KDM6A
                                  X35955904, #IKZF1
                                  X35956153, #FLT3 I/T/R
                                  X35958935, #GATA
                                  X35960677, #HRAS
                                  X35963472, #CBLB
                                  X35963556, #PDGFRA 
                                  X35963775  #GNAS
                                  ))

# target column as factor
#cohort_os$X44804077 = as.factor(cohort_os$X44804077)
#----------------------------------------------------------------- classification models

# definition of the classification task 
task_os = as_task_classif(cohort_os, target = 'X44804077')

# train/test split definition
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

# ------------------------------------------------------------------RF model test START
RF_pred_test = RF_class$predict(task_os, row_ids=test_set_os)
# ------------------------------------------------------------------RF model test END 

RF_pred_train$confusion
RF_pred_test$confusion
RF_pred_train$score(measures)
RF_pred_test$score(measures)
RF_pred_test$prob
RF_pred_train$response

# ------------------------------------------------------------------important features START
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
# ------------------------------------------------------------------important features END

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

# ------------------------------------------------------------------CT model test START
tree_pred_test = tree_class$predict(task_os, row_ids=test_set_os)
# ------------------------------------------------------------------CT model test END

tree_pred_train$confusion
tree_pred_test$confusion
tree_pred_train$score(measures)
tree_pred_test$score(measures)

### MODEL: Gradient Boosting
gboost_lrn = lrn("classif.xgboost", predict_type = "prob")
gboost_lrn$train(task_os, row_ids = train_set_os)
gboost_pred_train = gboost_lrn$predict(task_os, row_ids=train_set_os)

# ------------------------------------------------------------------GBM model test START
gboost_pred_test = gboost_lrn$predict(task_os, row_ids=test_set_os)
# ------------------------------------------------------------------GBM model test END

gboost_pred_train$confusion
gboost_pred_test$confusion
gboost_pred_train$score(measures)
gboost_pred_test$score(measures)

### MODEL: Support-Vector machines
svm_lrn = lrn("classif.svm", predict_type = "prob")
svm_lrn$train(task_os, row_ids = train_set_os)
svm_pred_train = svm_lrn$predict(task_os, row_ids=train_set_os)

# ------------------------------------------------------------------SVM model test START
svm_pred_test = svm_lrn$predict(task_os, row_ids=test_set_os)
# ------------------------------------------------------------------SVM model test END

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


save.image(file = "AML study.RData")










