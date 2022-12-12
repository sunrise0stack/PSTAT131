# setup
dt_model <- decision_tree() %>%
  set_engine("rpart")

dt_model_class <- dt_model %>%
  set_mode("classification")

# fit training data
dt_model_fit <- dt_model_class %>%
  fit(Class ~ ., data = cc_train)

dt_model_fit %>%
  extract_fit_engine() %>%
  rpart.plot(roundint=FALSE)

# fit testing data
augment(dt_model_fit, new_data = cc_test) %>%
  accuracy(truth = Class, estimate = .pred_class)

# heat map
augment(dt_model_fit, new_data = cc_test) %>%
  conf_mat(truth = Class, estimate = .pred_class) %>%
  autoplot(type = "heatmap")

# AUC
augment(dt_model_fit, new_data = cc_test) %>%
  roc_auc(Class, .pred_1)
