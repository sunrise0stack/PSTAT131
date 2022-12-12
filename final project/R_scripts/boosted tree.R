# setup
bt_model <- boost_tree(trees = tune()) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

# define grid
bt_grid <- grid_regular(trees(c(10,200)),levels = 10)

# workflow
bt_workflow <- workflow() %>%
  add_model(bt_model) %>%
  add_recipe(cc_recipe)

bt_res <- tune_grid(
  bt_workflow, 
  resamples = cc_folds, 
  grid = bt_grid, 
  metrics = metric_set(roc_auc),)

autoplot(bt_res)

# select best tree and fit
best_tree <- select_best(bt_res)
bt_final <- finalize_workflow(bt_workflow, best_tree)
bt_final_fit <- fit(bt_final, data = cc_train)

augment(bt_final_fit, new_data = cc_test)%>%
  accuracy(truth = Class, estimate = .pred_class)

# heat map
augment(bt_final_fit, new_data = cc_test) %>%
  conf_mat(truth = Class, estimate = .pred_class) %>%
  autoplot(type = "heatmap")

# ROC
augment(bt_final_fit, new_data = cc_test)%>%
  roc_curve(Class, .pred_1) %>%
  autoplot()

# AUC
augment(bt_final_fit, new_data = cc_test) %>%
  roc_auc(Class, .pred_1)