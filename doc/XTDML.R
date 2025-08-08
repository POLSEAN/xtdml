## -----------------------------------------------------------------------------

# ML packages
library(XTDML) 
library(mlr3)
library(mlr3learners)
library(paradox)
library(mlr3tuning)
library(mlr3misc)

# other packages
library(tibble) 
#library(datawizard)
library(data.table)


# To suppress messages
lgr::get_logger("mlr3")$set_threshold("warn")
lgr::get_logger("bbotk")$set_threshold("warn")

# Set seed 
set.seed(1234)

# Load data
df = make_plpr_data(n_obs = 500, t_per = 10, dim_x = 10, theta = 0.5, rho=0.8)
head(df)

# Define X columns
x_cols = paste0("X", 1:10)


## -----------------------------------------------------------------------------
set.seed(1234)

# Set data environment
obj_xtdml_data = xtdml_data_from_data_frame(df,
                      x_cols = x_cols,  y_col = "y", d_cols = "d",
                      cluster_cols = "id",
                      approach = "cre", 
                      transformX = "no") # does not transform x_cols
obj_xtdml_data$print()


## -----------------------------------------------------------------------------

set.seed(1234)

# Declare learner
learner = lrn("regr.rpart")
ml_l = learner$clone()
ml_m = learner$clone()

xtdml_rpart = xtdml_plr$new(obj_xtdml_data,
                            ml_l = ml_l, ml_m = ml_m,
                            n_folds = 5, score="orth-PO")

# set up a list of parameter grids
param_grid = list("ml_l" = ps(cp = p_dbl(lower = 0.01, upper = 0.02),
                              maxdepth = p_int(lower = 2, upper = 10)),
                  "ml_m" = ps(cp = p_dbl(lower = 0.01, upper = 0.02),
                              maxdepth = p_int(lower = 2, upper = 10))
                  )

tune_settings = list(n_folds_tune = 5,
                     rsmp_tune = mlr3::rsmp("cv", folds = 5),
                     terminator = mlr3tuning::trm("evals", n_evals = 10),
                     algorithm = tnr("grid_search"), resolution = 20)

xtdml_rpart$tune(param_set = param_grid, tune_settings = tune_settings)

# Estimate target/causal parameter
xtdml_rpart$fit()
xtdml_rpart$print()

# Print selected (tuned) hyperparameters
print(xtdml_rpart$params)


## -----------------------------------------------------------------------------
set.seed(1234)


# Set data environment
obj_xtdml_data_lasso = xtdml_data_from_data_frame(df,
                      x_cols = x_cols,  y_col = "y", d_cols = "d",
                      cluster_cols = "id",
                      approach = "cre",
                      transformX = "poly") # creates extensive dictionary of nonlinear terms for x_cols
#obj_xtdml_data$print()

# Select learner

learner = lrn("regr.cv_glmnet", s="lambda.min")
ml_m = learner$clone()
ml_l = learner$clone()

# Set up estimation environment
xtdml_lasso = xtdml_plr$new(obj_xtdml_data_lasso,
                             ml_l = ml_l, ml_m = ml_m,
                             n_folds = 5)

# Display DML Estimates
xtdml_lasso$fit()
xtdml_lasso$print()


## -----------------------------------------------------------------------------
set.seed(1234)

# Set up data environment
obj_xtdml_data_nnet = xtdml_data_from_data_frame(df,
                                   x_cols = x_cols,  y_col = "y", d_cols = "d",
                                   cluster_cols = "id",
                                   approach = "cre",
                                   transformX = "minmax") # applied the minmax transformation to x_cols
#obj_xtdml_data_nnet$print()

# Declare learner
ml_l = lrn("regr.nnet", maxit=100, MaxNWts=1000, trace=FALSE)
ml_m = lrn("regr.nnet", maxit=100, MaxNWts=1000, trace=FALSE)

# Set up DML environment
xtdml_nnet = xtdml_plr$new(obj_xtdml_data_nnet,
                           ml_l = ml_l, ml_m = ml_m,
                           n_folds = 5)

# Hyperparameter tuning
# Set up a list of parameter grids
param_grid = list("ml_l" = ps(size = p_int(lower = 2, upper = 10),
                              decay = p_dbl(lower = 0, upper = 0.05)),
                  "ml_m" = ps(size = p_int(lower = 2, upper = 10),
                              decay = p_dbl(lower = 0, upper =  0.05))
                  )

# Set up minimum requirements for tuning settings
tune_settings = list(n_folds_tune = 5,
                     rsmp_tune = mlr3::rsmp("cv", folds = 5),
                     terminator = mlr3tuning::trm("evals", n_evals = 10),
                     algorithm = mlr3tuning::tnr("grid_search", resolution = 20))

xtdml_nnet$tune(param_set = param_grid, tune_settings = tune_settings)

# Fit DML model and print results
xtdml_nnet$fit()
xtdml_nnet$print()


## -----------------------------------------------------------------------------

# Display table that compares results
table = matrix(0, 3, 6)
table[1,] = c(xtdml_rpart$coef_theta,
              xtdml_rpart$se_theta,
              xtdml_rpart$pval_theta,
              xtdml_rpart$model_rmse,
              as.numeric(xtdml_rpart$rmses["ml_l"]),
              as.numeric(xtdml_rpart$rmses["ml_m"])
)
table[2,] = c(xtdml_lasso$coef_theta,
              xtdml_lasso$se_theta,
              xtdml_lasso$pval_theta,
              xtdml_lasso$model_rmse,
              as.numeric(xtdml_lasso$rmses["ml_l"]),
              as.numeric(xtdml_lasso$rmses["ml_m"])
)
table[3,] = c(xtdml_nnet$coef_theta,
              xtdml_nnet$se_theta,
              xtdml_nnet$pval_theta,
              xtdml_nnet$model_rmse,
              as.numeric(xtdml_nnet$rmses["ml_l"]),
              as.numeric(xtdml_nnet$rmses["ml_m"])
)

colnames(table)= c("Estimate", "Std. Error", "P-value", "Model RMSE", "MSE of l", "MSE of m")
rownames(table)= c("XTDML-TREE", "XTDML-LASSO", "XTDML-NNET")
print(table)

