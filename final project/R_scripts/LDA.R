#setup
lda_model <- discrim_linear() %>% 
  set_mode("classification") %>% 
  set_engine("MASS")

# workflow
lda_workflow <- workflow() %>% 
  add_model(lda_model) %>% 
  add_recipe(cc_recipe)

# fit the training data
lda_fit_train <- fit(lda_workflow, cc_train)

# predict for training data
predict(lda_fit_train, new_data = cc_train, type = "class") %>% 
  bind_cols(cc_train %>% select(Class)) %>% 
  accuracy(truth = Class, estimate = .pred_class)

# fit the testing data
lda_fit_test <- fit(lda_workflow, cc_test)

# predict for testing data
predict(lda_fit_test, new_data = cc_test, type = "class") %>% 
  bind_cols(cc_test %>% select(Class)) %>% 
  accuracy(truth = Class, estimate = .pred_class)

# heat map
augment(lda_fit_test, new_data = cc_test) %>%
  conf_mat(truth = Class, estimate = .pred_class) %>% 
  autoplot(type = "heatmap")

# ROC
augment(lda_fit_test, new_data = cc_test) %>%
  roc_curve(Class, .pred_1) %>%
  autoplot()

# AUC
augment(lda_fit_test, new_data = cc_test) %>%
  roc_auc(Class, .pred_1)