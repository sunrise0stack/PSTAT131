# read data
mydata <- read.csv("creditcard.csv")
head(mydata)

# check original data dimension
dimension <- dim(mydata)
dimension

# check missing value
sum(is.na(mydata))

# convert class to factor
mydata <- mydata %>%
  mutate(Class = factor(Class, levels = c("1", "0"))) %>%
  select(-Time, )

# splitting
set.seed(2022)
cc_split <- initial_split(mydata, prop = 0.80, strain = Class)
cc_train <- training(cc_split)
cc_test <- testing(cc_split)

# check dimension for training and testing
dim(cc_train)
dim(cc_test)

# create recipe and remove zero variance
cc_recipe <- recipe(Class ~ ., cc_train) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_nominal_predictors()) %>%
  step_normalize(all_predictors())

# create k-fold
cc_folds <- vfold_cv(cc_train, v = 5, strain = Class)

