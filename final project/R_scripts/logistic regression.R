# setup
log_model <- logistic_reg() %>% 
  set_engine("glm") %>% 
  set_mode("classification")

# workflow
log_workflow <- workflow() %>% 
  add_model(log_model) %>% 
  add_recipe(cc_recipe)

# fit the training data
log_fit <- fit(log_workflow, cc_train)

# fit the testing data
log_fit <- fit(log_workflow, cc_test)

# predict
predict(log_fit, new_data = cc_test, type = "class") %>% 
  bind_cols(cc_test %>% select(Class)) %>% 
  accuracy(truth = Class, estimate = .pred_class)

# heat map
augment(log_fit, new_data = cc_test) %>%
  conf_mat(truth = Class, estimate = .pred_class) %>% 
  autoplot(type = "heatmap")

# ROC
augment(log_fit, new_data = cc_test) %>%
  roc_curve(Class, .pred_1) %>%
  autoplot()

# AUC
augment(log_fit, new_data = cc_test) %>%
  roc_auc(Class, .pred_1)
