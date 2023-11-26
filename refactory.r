library(readxl)
library(glmnet)


base <- read_xlsx("dataset1.xlsx")  # raw dataset

global_base_ncol <- ncol(base)
global_flora <- base[,45:n_col]


