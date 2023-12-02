library(readxl)
library(glmnet)


base <- read_xlsx("dataset1.xlsx")  # raw dataset

global_base_ncol <- ncol(base)
global_flora <- base[,45:global_base_ncol]


# lasso selection turn one
LassoFloraSelectOne <- function(dataset, xcol_1, xcol_2, xcol_target)
{

    require(ggplot2)
    require(glmnet) # package for lasso/ridge training

    # data normalization
    loacl_dataset_normal <- scale(dataset)
    local_flora_dataset_normal <- loacl_dataset_normal[, 44: ncol(dataset)] # 44 is the head col of flora data

    # set the label
    local_dataset_x_lasso <- loacl_dataset_normal[, xcol_1: xcol_2]
    local_dataset_y_lasso <- loacl_dataset_normal[, xcol_target]

    # lasso regression
    set.seed(1234)
    local_lasso_testout <- 
    cv.glmnet(local_dataset_x_lasso, 
    local_dataset_y_lasso, 
    alpha = 1,
    nfolds = 3)

    # draw some useless plots
    plot(local_lasso_testout)
    plot(local_lasso_testout$glmnet.fit, "lambda", label = TRUE)

    # return the results
    local_lasso_testout_min <- local_lasso_testout$lambda.min
    local_lasso_testout_best <- glmnet(
        local_dataset_x_lasso, local_dataset_y_lasso,
        alpha = 1, lambda = local_lasso_testout_min)
    local_coef_values <- coef(local_lasso_testout_best)
    return(local_coef_values)
    


}


