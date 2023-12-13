library(readxl)
library(glmnet)


global_base <- read_xlsx("dataset1.xlsx")  # raw dataset

global_base_ncol <- ncol(global_base)
global_base_flora <- global_base[, 45: global_base_ncol]


# lasso selection turn one
LassoFloraSelectOne <- function(dataset, xcol_1, xcol_2, xcol_target) {

  require(ggplot2)
  require(glmnet) # package for lasso/ridge training

  # data normalization
  loacl_dataset_normal <- scale(dataset)

  # set the label
  local_dataset_x_lasso <- loacl_dataset_normal[, xcol_1: xcol_2]
  local_dataset_y_lasso <- loacl_dataset_normal[, xcol_target]

  # lasso regression
  set.seed(1234)
  local_lasso_testout <- cv.glmnet(local_dataset_x_lasso,
                                   local_dataset_y_lasso,
                                   alpha = 1, nfolds = 3)

  # draw plots(with no meaning)
  #plot(local_lasso_testout)
  #plot(local_lasso_testout$glmnet.fit, "lambda", label = TRUE)

  # find the best model
  local_lasso_testout_min <- local_lasso_testout$lambda.min
  local_lasso_testout_best <- glmnet(local_dataset_x_lasso,
                                     local_dataset_y_lasso,
                                     alpha = 1,
                                     lambda = local_lasso_testout_min)

  # grab the results
  local_coef_values <- coef(local_lasso_testout_best)

  # return the results
  return(local_coef_values)
}

# loop selection
exposed_local_flag_lasso <- 1
repeat {
  print(LassoFloraSelectOne(global_base_flora,
                            exposed_local_flag_lasso,
                            exposed_local_flag_lasso + 9,
                            xcol_target = 2))
  exposed_local_flag_lasso <- exposed_local_flag_lasso + 10
  if (exposed_local_flag_lasso >= ncol(global_base_flora))  {
    exposed_local_flag_lasso <- 1
    break
  }
}

# temp
temp_function_test <- LassoFloraSelectOne(global_base_flora, 25, 30, 2)

# need to learn dgCMatrix
#print(temp_function_test$s0)
