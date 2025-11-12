#' @title Abstract class xtdml
#'
#' @description
#' Abstract base class that cannot be initialized.
#'
#' `xtdml` estimates the structural parameter (treatment effect)
#'  in partially linear panel regression models with fixed effects
#'  using double machine learning  (Clarke and Polselli, 2025).
#'  `xtdml` allows the estimation of the nuisance functions in the model by machine
#'  learning methods based on the panel data approach chosen by the user,
#'  and computation of the Neyman-orthogonal score functions.
#'
#' `xtdml` builds on the object-oriented architecture of `DoubleML` (Bach et al., 2024), using
#' the 'mlr3' ecosystem and the 'R6' package. `xtdml` follows most of the notation of `DoubleML`.
#'
#' @importFrom R6 R6Class
#'
#' @format [R6::R6Class] object.
#'
#' @family xtdml
#'
#' @export
xtdml <- R6Class("xtdml",
                 active = list(
                   #' @field all_coef_theta (`matrix()`) \cr
                   #' Estimates of the causal parameter(s) `"theta"` for the `n_rep` different sample
                   #' splits after calling `fit()`.
                   all_coef_theta = function(value) {
                     if (missing(value)) {
                       return(private$all_coef_theta_)
                     } else {
                       stop("can't set field all_coef_theta")
                     }
                   },

                   #' @field all_dml1_coef_theta (`array()`) \cr
                   #' Estimates of the causal parameter(s) `"theta"` for the `n_rep` different sample
                   #' splits after calling `fit()` with `dml_procedure = "dml1"`.
                   all_dml1_coef_theta = function(value) {
                     if (missing(value)) {
                       return(private$all_dml1_coef_theta_)
                     } else {
                       stop("can't set field all_dml1_theta_coef")
                     }
                   },

                   #' @field all_se_theta (`matrix()`) \cr
                   #' Standard errors of the causal parameter(s) `"theta"` for the `n_rep` different
                   #' sample splits after calling `fit()`.
                   all_se_theta = function(value) {
                     if (missing(value)) {
                       return(private$all_se_theta_)
                     } else {
                       stop("can't set field all_se_theta")
                     }
                   },

                   #' @field all_model_rmse (`matrix()`) \cr
                   #' Model root-mean-squared-error.
                   all_model_rmse = function(value) {
                     if (missing(value)) {
                       return(private$all_model_rmse_)
                     } else {
                       stop("can't set field all_model_rmse")
                     }
                   },

                   #' @field apply_cross_fitting (`logical(1)`) \cr
                   #' Indicates whether cross-fitting should be applied. Default is `TRUE`.
                   apply_cross_fitting = function(value) {
                     if (missing(value)) {
                       return(private$apply_cross_fitting_)
                     } else {
                       stop("can't set field apply_cross_fitting")
                     }
                   },

                   #' @field coef_theta (`numeric()`) \cr
                   #' Estimates for the causal parameter(s) `"theta"` after calling `fit()`.
                   coef_theta = function(value) {
                     if (missing(value)) {
                       return(private$coef_theta_)
                     } else {
                       stop("can't set field coef_theta")
                     }
                   },

                   #' @field data ([`data.table`][data.table::data.table()])\cr
                   #' Data object.
                   data = function(value) {
                     if (missing(value)) {
                       return(private$data_)
                     } else {
                       stop("can't set field data")
                     }
                   },

                   #' @field dml_procedure (`character(1)`) \cr
                   #' A `character()` (`"dml1"` or `"dml2"`) specifying the double machine
                   #' learning algorithm. Default is `"dml2"`.
                   dml_procedure = function(value) {
                     if (missing(value)) {
                       return(private$dml_procedure_)
                     } else {
                       stop("can't set field dml_procedure")
                     }
                   },

                   #' @field draw_sample_splitting (`logical(1)`) \cr
                   #' Indicates whether the sample splitting should be drawn during
                   #' initialization of the object. Default is `TRUE`.
                   draw_sample_splitting = function(value) {
                     if (missing(value)) {
                       return(private$draw_sample_splitting_)
                     } else {
                       stop("can't set field draw_sample_splitting")
                     }
                   },

                   #' @field learner (named `list()`) \cr
                   #' The machine learners for the nuisance functions.
                   learner = function(value) {
                     if (missing(value)) {
                       return(private$learner_)
                     } else {
                       stop("can't set field learner")
                     }
                   },

                   #' @field n_folds (`integer(1)`) \cr
                   #' Number of folds. Default is `5`.
                   n_folds = function(value) {
                     if (missing(value)) {
                       return(private$n_folds_)
                     } else {
                       stop("can't set field n_folds")
                     }
                   },

                   #' @field n_rep (`integer(1)`) \cr
                   #' Number of repetitions for the sample splitting. Default is `1`.
                   n_rep = function(value) {
                     if (missing(value)) {
                       return(private$n_rep_)
                     } else {
                       stop("can't set field n_rep")
                     }
                   },

                   #' @field params (named `list()`) \cr
                   #' The hyperparameters of the learners.
                   params = function(value) {
                     if (missing(value)) {
                       return(private$params_)
                     } else {
                       stop("can't set field params")
                     }
                   },

                   #' @field psi_theta (`array()`) \cr
                   #' Value of the score function
                   #' \eqn{\psi(W;\theta_0,\eta_0)=-\psi_a(W;\eta_0) \theta_0 + \psi_b(W;\eta_0)}
                   #' after calling `fit()`.
                   psi_theta = function(value) {
                     if (missing(value)) {
                       return(private$psi_theta_)
                     } else {
                       stop("can't set field psi_theta")
                     }
                   },

                   #' @field psi_theta_a (`array()`) \cr
                   #' Value of the score function component \eqn{\psi_a(W;\eta_0)} after
                   #' calling `fit()`.
                   psi_theta_a = function(value) {
                     if (missing(value)) {
                       return(private$psi_theta_a_)
                     } else {
                       stop("can't set field psi_theta_a")
                     }
                   },

                   #' @field psi_theta_b (`array()`) \cr
                   #' Value of the score function component \eqn{\psi_b(W;\eta_0)} after
                   #' calling `fit()`.
                   psi_theta_b = function(value) {
                     if (missing(value)) {
                       return(private$psi_theta_b_)
                     } else {
                       stop("can't set field psi_theta_b")
                     }
                   },

                   #' @field res_y (`array()`) \cr
                   #' Residual of output equation
                   res_y = function(value) {
                     if (missing(value)) {
                       return(private$res_y_)
                     } else {
                       stop("can't set field res_y")
                     }
                   },

                   #' @field res_d (`array()`) \cr
                   #' Residual of treatment equation
                   res_d = function(value) {
                     if (missing(value)) {
                       return(private$res_d_)
                     } else {
                       stop("can't set field res_d")
                     }
                   },

                   #' @field predictions (`array()`) \cr
                   #' Predictions of the nuisance models after calling
                   #' `fit(store_predictions=TRUE)`.
                   predictions = function(value) {
                     if (missing(value)) {
                       return(private$predictions_)
                     } else {
                       stop("can't set field predictions")
                     }
                   },

                   #' @field targets (`array()`) \cr
                   #' Targets of the nuisance models after calling
                   #' `fit(store_predictions=TRUE)`.
                   targets = function(value) {
                     if (missing(value)) {
                       return(private$targets_)
                     } else {
                       stop("can't set field targets")
                     }
                   },

                   #' @field rmses (`array()`) \cr
                   #' The root-mean-squared-errors of the nuisance parameters
                   rmses = function(value) {
                     if (missing(value)) {
                       return(private$rmses_)
                     } else {
                       stop("can't set field rmses")
                     }
                   },

                   #' @field all_model_mse (`array()`) \cr
                   #' Collection of all mean-squared-errors of the model
                   all_model_mse = function(value) {
                     if (missing(value)) {
                       return(private$all_model_mse_)
                     } else {
                       stop("can't set field all_model_mse")
                     }
                   },

                   #' @field model_rmse (`array()`) \cr
                   #' The root-mean-squared-errors of the model
                   model_rmse = function(value) {
                     if (missing(value)) {
                       return(private$model_rmse_)
                     } else {
                       stop("can't set field model_rmse")
                     }
                   },

                   #' @field models (`array()`) \cr
                   #' The fitted nuisance models after calling
                   #' `fit(store_models=TRUE)`.
                   models = function(value) {
                     if (missing(value)) {
                       return(private$models_)
                     } else {
                       stop("can't set field models")
                     }
                   },

                   #' @field pval_theta (`numeric()`) \cr
                   #' p-values for the causal parameter(s) `"theta"` after calling `fit()`.
                   pval_theta = function(value) {
                     if (missing(value)) {
                       return(private$pval_theta_)
                     } else {
                       stop("can't set field pval_theta ")
                     }
                   },

                   #' @field score (`character(1)`) \cr
                   #' A `character(1)`  specifying the score function among `"orth-PO"`, `"orth-IV"`.
                   #' Default is "`orth-PO`".
                   score = function(value) {
                     if (missing(value)) {
                       return(private$score_)
                     } else {
                       stop("can't set field score")
                     }
                   },

                   #' @field se_theta (`numeric()`) \cr
                   #' Standard errors for the causal parameter(s) `"theta"` after calling `fit()`.
                   se_theta = function(value) {
                     if (missing(value)) {
                       return(private$se_theta_)
                     } else {
                       stop("can't set field se_theta")
                     }
                   },

                   #' @field smpls (`list()`) \cr
                   #' The partition used for cross-fitting.
                   smpls = function(value) {
                     if (missing(value)) {
                       return(private$smpls_)
                     } else {
                       stop("can't set field smpls")
                     }
                   },

                   #' @field smpls_cluster (`list()`) \cr
                   #' The partition used for cross-fitting.
                   #' smpl is at cluster-var
                   smpls_cluster = function(value) {
                     if (missing(value)) {
                       return(private$smpls_cluster_)
                     } else {
                       stop("can't set field smpls_cluster")
                     }
                   },

                   #' @field t_stat_theta (`numeric()`) \cr
                   #' t-statistics for the causal parameter(s)  `"theta"` after calling `fit()`.
                   t_stat_theta = function(value) {
                     if (missing(value)) {
                       return(private$t_stat_theta_)
                     } else {
                       stop("can't set field t_stat_theta")
                     }
                   },

                   #' @field tuning_res_theta (named `list()`) \cr
                   #' Results from hyperparameter tuning.
                   tuning_res_theta = function(value) {
                     if (missing(value)) {
                       return(private$tuning_res_theta_)
                     } else {
                       stop("can't set field tuning_res_theta")
                     }
                   }
                 ),
                 public = list(
                   #' @description
                   #' DML with FE is an abstract class that can't be initialized.
                   initialize = function() {
                     stop("xtdml is an abstract class that can't be initialized.")
                   },

                   #' @description
                   #' Print 'DML with FE' objects.
                   print = function() {

                     class_name = class(self)[1]
                     header = paste0(
                       "================= ", class_name,
                       " Object ==================\n")

                     if (self$data$n_cluster_vars == 1 || self$data$n_cluster_vars == 2) {
                       cluster_info = paste0(
                         "Panel identifier: ", paste0(self$data$panel_id, collapse = ", "), "\n",
                         "Time identifier: ", paste0(self$data$time_id, collapse = ", "), "\n",
                         "Cluster variable(s): ", paste0(self$data$cluster_cols, collapse = ", "), "\n")
                     } else {
                       stop(print("At most two cluster variables allowed"))
                     }
                     data_info = paste0(
                       "Outcome variable: ", self$data$y_col, "\n",
                       "Treatment variable: ", paste0(self$data$d_cols, collapse = ", "),
                       "\n",
                       "Covariates: ", paste0(cbind(self$data$x_cols), collapse = ", "), "\n",
                       cluster_info,
                       "No. Observations: ", self$data$n_obs, "\n",
                       "No. Groups: ", length(unique(self$data$data_model[[self$data$cluster_cols]])), "\n")
                     if (is.character(self$score)) {
                       score_info = paste0(
                         "Score function: ", self$score, "\n",
                         "DML algorithm: ", self$dml_procedure, "\n",
                         "Panel data approach: ", self$data$approach, "\n",
                         "Type of transformation for X: ", self$data$transformX, "\n")
                     }
                     learner_info = character(length(self$learner))
                     for (i_lrn in seq_len(length(self$params))) {
                       if (any(class(self$learner[[i_lrn]]) == "Learner")) {
                         learner_info[i_lrn] = paste0(
                           "Learner of nuisance ", self$params_names()[[i_lrn]], ": ", self$learner[[i_lrn]]$id, "\n",
                           "RMSE of nuisance ", self$params_names()[[i_lrn]], " : ", format(round(self$rmses[[i_lrn]], 5), nsmall = 5), "\n")
                       } else {
                         learner_info[i_lrn] = paste0(
                           "Learner of nuisance ", self$params_names()[[i_lrn]], ": ", self$learner[i_lrn], "\n",
                           "RMSE of nuisance ", self$params_names()[[i_lrn]], " : ", format(round(self$rmses[[i_lrn]], 5), nsmall = 5), "\n")
                       }
                     }
                     model_info = paste0("Model RMSE: ", format(round(self$model_rmse, 5), nsmall = 5), "\n")
                     resampling_info = paste0(
                       "No. folds: ", self$n_folds, "\n",
                       "No. folds per cluster: ", private$n_folds_per_cluster, "\n",
                       "No. repeated sample splits: ", self$n_rep, "\n",
                       "Apply cross-fitting: ", self$apply_cross_fitting, "\n")

                     cat(header, "\n", "\n",
                         "\n------------------ Data summary ------------------\n",
                         data_info,
                         "\n------------------ Score & algorithm ------------------\n",
                         score_info,
                         "\n------------------ Machine learner ------------------\n",
                         learner_info, model_info,
                         "\n------------------ Resampling ------------------\n",
                         resampling_info,
                         "\n------------------ Fit summary ------------------\n ",
                         sep = "")
                     self$summary()

                     invisible(self)
                   },

                   #' @description
                   #' Estimate DML models with FE.
                   #'
                   #' @param store_predictions (`logical(1)`) \cr
                   #' Indicates whether the predictions for the nuisance functions should be
                   #' stored in field `predictions`. Default is `FALSE`.
                   #'
                   #' @param store_models (`logical(1)`) \cr
                   #' Indicates whether the fitted models for the nuisance functions should be
                   #' stored in field `models` if you want to analyze the models or extract
                   #' information like variable importance. Default is `FALSE`.
                   #'
                   #' @return self
                   fit = function(store_predictions = FALSE, store_models = FALSE) {

                     private$initialize_rmses()  #ap

                     if (store_predictions) {
                       private$initialize_predictions_and_targets()
                     }
                     if (store_models) {
                       private$initialize_models()
                     }

                     for (i_rep in 1:self$n_rep) {
                       private$i_rep = i_rep

                       for (i_treat in 1:self$data$n_treat) {
                         private$i_treat = i_treat

                         # ml estimation of nuisance models and computation of psi elements
                         res = private$nuisance_est(private$get__smpls())
                         private$psi_theta_a_[, private$i_rep, private$i_treat] = res$psi_theta_a
                         private$psi_theta_b_[, private$i_rep, private$i_treat] = res$psi_theta_b

                         private$res_y_[, private$i_rep, private$i_treat] = res$res_y
                         private$res_d_[, private$i_rep, private$i_treat] = res$res_d

                         if (store_predictions) {
                           private$store_predictions_and_targets(res$preds, res$targets) #ap
                         }
                         if (store_models) {
                           private$store_models(res$models)
                         }

                         # estimate the causal parameter
                         private$all_coef_theta_[private$i_treat, private$i_rep] = private$est_causal_pars_theta()
                         # compute score (depends on estimated causal parameter)
                         private$psi_theta_[, private$i_rep, private$i_treat] = private$compute_score_theta()
                         private$all_se_theta_[private$i_treat, private$i_rep] = private$se_theta_causal_pars()

                         # calculate RMSE of the nuisance parameters
                         private$calc_rmses(res$preds, res$targets)
                         #calculate model RMSE
                         private$all_model_mse_[, private$i_rep, private$i_treat] = res$model_mse
                       }
                     }
                     private$agg_cross_fit()

                     ##Inference for causal parameter (theta)
                     private$t_stat_theta_ = self$coef_theta / self$se_theta
                     private$pval_theta_ = 2 * pnorm(-abs(self$t_stat_theta))
                     names(private$coef_theta_) = names(private$se_theta_) = names(private$t_stat_theta_) =
                       names(private$pval_theta_) = self$data$d_cols

                     invisible(self)
                   },

                   #' @description
                   #' Draw sample splitting for Double ML models with FE.
                   #'
                   #' The samples are drawn according to the attributes `n_folds`, `n_rep`
                   #' and `apply_cross_fitting`.
                   #'
                   #' @return self
                   split_samples = function() {
                     dummy_task = Task$new("dummy_resampling", "regr", self$data$data)

                     if (self$apply_cross_fitting) {
                       ##for clustered data
                       all_smpls = list()
                       all_smpls_cluster = list()
                       for (i_rep in 1:self$n_rep) {
                         smpls_cluster_vars = list()
                         for (i_var in 1:self$data$n_cluster_vars) {
                           clusters = unique(self$data$data_model[[self$data$cluster_cols[i_var]]])
                           n_clusters = length(clusters)

                           dummy_task = Task$new(
                             "dummy_resampling", "regr",
                             data.table(dummy_var = rep(0, n_clusters)))
                           dummy_resampling_scheme = rsmp("repeated_cv",
                                                          folds = private$n_folds_per_cluster,
                                                          repeats = 1)$instantiate(dummy_task)
                           train_ids = lapply(
                             1:(private$n_folds_per_cluster),    ##n_fold_per cluster?
                             function(x) clusters[dummy_resampling_scheme$train_set(x)])
                           test_ids = lapply(
                             1:(private$n_folds_per_cluster),     ##n_fold_per cluster?
                             function(x) clusters[dummy_resampling_scheme$test_set(x)])

                           smpls_cluster_vars[[i_var]] = list(
                             train_ids = train_ids,
                             test_ids = test_ids)
                         }
                         smpls = list(train_ids = list(), test_ids = list())
                         smpls_cluster = list(train_ids = list(), test_ids = list())
                         cart = expand.grid(lapply(
                           1:self$data$n_cluster_vars,
                           function(x) 1:private$n_folds_per_cluster))
                         for (i_smpl in 1:(self$n_folds)) {
                           ind_train = rep(TRUE, self$data$n_obs)
                           ind_test = rep(TRUE, self$data$n_obs)
                           this_cluster_smpl_train = list()
                           this_cluster_smpl_test = list()
                           for (i_var in 1:self$data$n_cluster_vars) {
                             i_fold = cart[i_smpl, i_var]
                             train_clusters = smpls_cluster_vars[[i_var]]$train_ids[[i_fold]]
                             test_clusters = smpls_cluster_vars[[i_var]]$test_ids[[i_fold]]
                             this_cluster_smpl_train[[i_var]] = train_clusters
                             this_cluster_smpl_test[[i_var]] = test_clusters
                             xx = self$data$data_model[[self$data$cluster_cols[i_var]]] %in% train_clusters
                             ind_train = ind_train & xx
                             xx = self$data$data_model[[self$data$cluster_cols[i_var]]] %in% test_clusters
                             ind_test = ind_test & xx
                           }
                           smpls$train_ids[[i_smpl]] = seq(self$data$n_obs)[ind_train]
                           smpls$test_ids[[i_smpl]] = seq(self$data$n_obs)[ind_test]
                           smpls_cluster$train_ids[[i_smpl]] = this_cluster_smpl_train
                           smpls_cluster$test_ids[[i_smpl]] = this_cluster_smpl_test
                         }
                         all_smpls[[i_rep]] = smpls
                         all_smpls_cluster[[i_rep]] = smpls_cluster
                       }
                       smpls = all_smpls
                       private$smpls_cluster_ = all_smpls_cluster
                     } else {
                       if (self$n_folds <3) {
                         stop(paste0("Set `n_folds` at least equal to 3. Default`n_folds` = 5 "))
                       }
                     }
                     private$smpls_ = smpls
                     invisible(self)
                   },

                   #' @description
                   #' Hyperparameter tuning for Double Machine Learning (DML) models with fixed effects.
                   #'
                   #' The hyperparameter tuning is performed using the tuning methods provided
                   #' in the [mlr3tuning](https://mlr3tuning.mlr-org.com/) package.
                   #' For more information on tuning in [mlr3](https://mlr3.mlr-org.com/),
                   #' see the chapter on hyperparameter optimization in the
                   #' [mlr3 book](https://mlr3book.mlr-org.com/chapters/chapter4/hyperparameter_optimization.html).
                   #'
                   #' @param param_set (named `list()`) \cr
                   #' A named `list` with a parameter grid for each nuisance model/learner
                   #' (see method `learner_names()`).
                   #' Each element must be a [ParamSet][paradox::ParamSet] object.
                   #'
                   #' @param tune_settings (named `list()`) \cr
                   #' A named `list()` of settings controlling the hyperparameter tuning process.
                   #' Each entry is passed to the corresponding components from
                   #' [mlr3tuning](https://mlr3tuning.mlr-org.com/):
                   #' * `terminator` (`[bbotk::Terminator]`) \cr
                   #'   A [Terminator][bbotk::Terminator] object specifying when the tuning
                   #'   process should stop (e.g., `trm("evals", n_evals = 20)`).
                   #' * `tuner` — a [Tuner][mlr3tuning::Tuner] object created with
                   #'   [tnr()][mlr3tuning::tnr()], which defines the optimization algorithm.
                   #'   (e.g., `tnr("grid_search")` or `tnr("random_search")`).
                   #'   If set to `"grid_search"`, then additional argument `"resolution"` is required.
                   #' * `rsmp_tune` — a [Resampling][mlr3::Resampling] object or a key passed to
                   #'   [rsmp()][mlr3::rsmp()].
                   #'   Defines the resampling strategy used during tuning (default: `"cv"`).
                   #' * `n_folds_tune` — an integer scalar (optional).
                   #'   Number of folds used if `rsmp_tune = "cv"`. Default is `5`.
                   #' * `measure` — a named `list()` (optional).
                   #'   Contains the performance measures used for tuning.
                   #'   Each element must be either a [Measure][mlr3::Measure] object or a key to
                   #'   [msr()][mlr3::msr()].
                   #'   Names must match the learner names (see `learner_names()`).
                   #'   If omitted, default measures are used (`"regr.rmse"` for regression and
                   #'   `"classif.ce"` for classification).
                   #'
                   #' @param tune_on_folds (`logical(1)`) \cr
                   #' Indicates whether the tuning should be performed separately for each
                   #' cross-fitting fold (`TRUE`) or globally across all folds (`FALSE`, default).
                   #'
                   #' @examples
                   #' tune_settings = list(
                   #'   n_folds_tune = 5,
                   #'   rsmp_tune = mlr3::rsmp("cv", folds = 5),
                   #'   terminator = mlr3tuning::trm("evals", n_evals = 20),
                   #'   tuner = mlr3tuning::tnr("grid_search", resolution = 10))
                   #'
                   #' @return self
                   tune = function(param_set,
                                   tune_settings = list(
                                     n_folds_tune = 5,
                                     rsmp_tune = mlr3::rsmp("cv", folds = 5),
                                     measure = NULL,
                                     terminator = mlr3tuning::trm("evals", n_evals = 20),
                                     tuner = mlr3tuning::tnr("grid_search", resolution = 10)
                                   ),
                                   tune_on_folds = FALSE) {

                     assert_list(param_set)
                     valid_learner = self$learner_names()
                     if (!test_names(names(param_set), subset.of = valid_learner)) {
                       stop(paste(
                         "Invalid param_set", paste0(names(param_set), collapse = ", "),
                         "\n param_grids must be a named list with elements named",
                         paste0(valid_learner, collapse = ", ")))
                     }
                     for (i_grid in seq_len(length(param_set))) {
                       assert_class(param_set[[i_grid]], "ParamSet")
                     }
                     assert_logical(tune_on_folds, len = 1)
                     tune_settings = private$assert_tune_settings(tune_settings)

                     if (!self$apply_cross_fitting) {
                       stop("Parameter tuning for no-cross-fitting case not implemented.")
                     }

                     if (tune_on_folds) {
                       params_rep = vector("list", self$n_rep)

                       private$tuning_res_theta_ = rep(list(params_rep), self$data$n_treat)
                       names(private$tuning_res_theta_) = self$data$d_cols

                       private$fold_specific_params = TRUE
                     } else {
                       private$tuning_res_theta_ = vector("list", self$data$n_treat)
                       names(private$tuning_res_theta_) = self$data$d_cols

                     }

                     for (i_treat in 1:self$data$n_treat) {
                       private$i_treat = i_treat

                       if (self$data$n_treat > 1) {
                         self$data$set_data_model(self$data$d_cols[i_treat])
                       }

                       if (tune_on_folds) {
                         for (i_rep in 1:self$n_rep) {
                           private$i_rep = i_rep
                           param_tuning = private$nuisance_tuning(
                             private$get__smpls(),
                             param_set, tune_settings, tune_on_folds)
                           private$tuning_res_theta_[[i_treat]][[i_rep]] = param_tuning

                           for (nuisance_model in names(param_tuning)) {
                             if (!is.null(param_tuning[[nuisance_model]][[1]])) {
                               self$set_ml_nuisance_params(
                                 learner = nuisance_model,
                                 treat_var = self$data$treat_col,
                                 params = param_tuning[[nuisance_model]]$params,
                                 set_fold_specific = FALSE)
                             } else {
                               next
                             }
                           }
                         }
                       } else {
                         private$i_rep = 1
                         param_tuning = private$nuisance_tuning(
                           private$get__smpls(),
                           param_set, tune_settings, tune_on_folds)
                         private$tuning_res_theta_[[i_treat]] = param_tuning

                         for (nuisance_model in self$params_names()) {
                           if (!is.null(param_tuning[[nuisance_model]][[1]])) {
                             self$set_ml_nuisance_params(
                               learner = nuisance_model,
                               treat_var = self$data$treat_col,
                               params = param_tuning[[nuisance_model]]$params[[1]],
                               set_fold_specific = FALSE)
                           } else {
                             next
                           }
                         }
                       }
                     }
                     invisible(self)
                   },

                   #' @description
                   #' Summary for DML models with FE after calling `fit()`.
                   #'
                   #' @param digits (`integer(1)`) \cr
                   #' The number of significant digits to use when printing.
                   summary = function(digits = max(3L, getOption("digits") -
                                                     3L)) {
                     if (all(is.na(self$coef_theta))) {
                       message("fit() not yet called.")
                     } else {
                       k = length(self$coef_theta)
                       table = matrix(NA_real_, ncol = 4, nrow = k)
                       rownames(table) = names(self$coef_theta)
                       colnames(table) = c("Estimate.", "Std. Error", "t value", "Pr(>|t|)")
                       table[, 1] = c(self$coef_theta)
                       table[, 2] = c(self$se_theta)
                       table[, 3] = c(self$t_stat_theta)
                       table[, 4] = c(self$pval_theta)
                       private$summary_table = table

                       if (length(k)) {
                         cat(
                           "Estimates and significance testing of the",
                           "effect of target variables\n")
                         res = as.matrix(printCoefmat(private$summary_table,
                                                      digits = digits,
                                                      P.values = TRUE,
                                                      has.Pvalue = TRUE))
                         cat("\n")
                       }
                       else {
                         cat("No coefficients\n")
                       }
                       cat("\n")
                       invisible(res)
                     }
                   },

                   #' @description
                   #' Confidence intervals for DML models with FE.
                   #'
                   #' @param joint (`logical(1)`) \cr
                   #' Indicates whether joint confidence intervals are computed.
                   #' Default is `FALSE`.
                   #'
                   #' @param level (`numeric(1)`) \cr
                   #' The confidence level. Default is `0.95`.
                   #'
                   #' @param parm (`numeric()` or `character()`) \cr
                   #' A specification of which parameters are to be given confidence intervals
                   #' among the variables for which inference was done, either a vector of
                   #' numbers or a vector of names. If missing, all parameters are considered
                   #' (default).
                   #' @return A `matrix()` with the confidence interval(s).
                   confint = function(parm, joint = FALSE, level = 0.95) {
                     assert_logical(joint, len = 1)
                     assert_numeric(level, len = 1)
                     if (level <= 0 | level >= 1) {
                       stop("'level' must be > 0 and < 1.")
                     }
                     if (missing(parm)) {
                       parm = names(self$coef_theta)
                     }
                     else {
                       assert(
                         check_character(parm, max.len = k),
                         check_numeric(parm, max.len = k)
                       )
                       if (is.numeric(parm)) {
                         parm = names(self$coef_theta)[parm]
                       }
                     }
                     if (joint == FALSE) {
                       a = (1 - level) / 2
                       a = c(a, 1 - a)
                       pct = format.perc(a, 3)
                       fac = qnorm(a)
                       ci = array(NA_real_,
                                  dim = c(length(parm), 2L),
                                  dimnames = list(parm, pct))
                       ci[] = c(self$coef_theta[parm] + self$se_theta[parm] %o% fac)
                     }

                     if (joint == TRUE) {

                       a = (1 - level)
                       ab = c(a / 2, 1 - a / 2)
                       pct = format.perc(ab, 3)
                       ci = array(NA_real_,
                                  dim = c(length(parm), 2L),
                                  dimnames = list(parm, pct))


                       ##check if correct
                       ci[, 1] = c(self$coef_theta[parm] - hatc * self$se_theta[parm])
                       ci[, 2] = c(self$coef_theta[parm] + hatc * self$se_theta[parm])
                     }
                     return(ci)
                   },

                   #' @description
                   #' Returns the names of the learners.
                   #'
                   #' @return `character()` with names of learners.
                   learner_names = function() {
                     return(names(self$learner))
                   },

                   #' @description
                   #' Returns the names of the nuisance models with hyperparameters.
                   #'
                   #' @return `character()` with names of nuisance models with hyperparameters.
                   params_names = function() {
                     x = self$params
                     return(names(x))
                   },

                   #' @description
                   #' Set hyperparameters for the nuisance models of DML models with FE.
                   #'
                   #' Note that in the current implementation, either all parameters have to
                   #' be set globally or all parameters have to be provided fold-specific.
                   #'
                   #' @param learner (`character(1)`) \cr
                   #' The nuisance model/learner (see method `params_names`).
                   #'
                   #' @param treat_var (`character(1)`) \cr
                   #' The treatment variAble (hyperparameters can be set treatment-variable
                   #' specific).
                   #'
                   #' @param params (named `list()`) \cr
                   #' A named `list()` with estimator parameters for time-varying covariates. Parameters are used for all
                   #' folds by default. Alternatively, parameters can be passed in a
                   #' fold-specific way if option  `fold_specific`is `TRUE`. In this case, the
                   #' outer list needs to be of length `n_rep` and the inner list of length
                   #' `n_folds_per_cluster`.
                   #'
                   #' @param set_fold_specific (`logical(1)`) \cr
                   #' Indicates if the parameters passed in `params` should be passed in
                   #' fold-specific way. Default is `FALSE`. If `TRUE`, the outer list needs
                   #' to be of length `n_rep` and the inner list of length `n_folds_per_cluster`.
                   #' Note that in the current implementation, either all parameters have to
                   #' be set globally or all parameters have to be provided fold-specific.
                   #'
                   #' @return self
                   set_ml_nuisance_params = function(learner = NULL,
                                                     treat_var = NULL,
                                                     params,
                                                     set_fold_specific = FALSE) {

                     valid_learner = self$params_names()
                     assert_character(learner, len = 1)
                     assert_choice(learner, valid_learner)

                     assert_choice(treat_var, self$data$d_cols)

                     assert_list(params)
                     assert_logical(set_fold_specific, len = 1)

                     if (!set_fold_specific) {
                       if (private$fold_specific_params) {
                         private$params_[[learner]][[treat_var]][[private$i_rep]] = params
                       } else {
                         private$params_[[learner]][[treat_var]] = params
                       }
                     } else {
                       if (length(params) != self$n_rep) {
                         stop("Length of (outer) parameter list does not match n_rep.")
                       }
                       if (!all(lapply(params, length) == self$n_folds)) {
                         stop("Length of (inner) parameter list does not match n_folds.")
                       }

                       private$fold_specific_params = set_fold_specific
                       private$params_[[learner]][[treat_var]] = params
                     }
                   },

                   #' @description
                   #' Get hyper-parameters for the nuisance model of xtdml models.
                   #'
                   #' @param learner (`character(1)`) \cr
                   #' The nuisance model/learner (see method `params_names()`)
                   #'
                   #' @return named `list()`with paramers for the nuisance model/learner.
                   get_params = function(learner) {
                     valid_learner = self$params_names()
                     assert_character(learner, len = 1)
                     assert_choice(learner, valid_learner)

                     if (private$fold_specific_params) {
                       params = self$params[[learner]][[self$data$treat_col]][[private$i_rep]]
                     } else {
                       params = self$params[[learner]][[self$data$treat_col]]
                     }
                     return(params)
                   }
                 ),
                 private = list(
                   all_coef_theta_ = NULL,
                   all_dml1_coef_theta_ = NULL,
                   all_se_theta_ = NULL,
                   all_model_mse_ = NULL,
                   apply_cross_fitting_ = NULL,
                   coef_theta_ = NULL,
                   data_ = NULL,
                   dml_procedure_ = NULL,
                   draw_sample_splitting_ = NULL,
                   learner_ = NULL,
                   model_ = NULL,
                   n_folds_ = NULL,
                   n_rep_ = NULL,
                   params_ = NULL,
                   psi_theta_ = NULL,
                   psi_theta_a_ = NULL,
                   psi_theta_b_ = NULL,
                   res_y_ = NULL,
                   res_d_ = NULL,
                   predictions_ = NULL,
                   targets_ = NULL,
                   rmses_ = NULL,
                   models_ = NULL,
                   pval_theta_ = NULL,
                   score_ = NULL,
                   se_theta_ = NULL,
                   model_rmse_ = NULL,
                   smpls_ = NULL,
                   t_stat_theta_ = NULL,
                   tuning_res_theta_ = NULL,
                   i_rep = NA_integer_,
                   i_treat = NA_integer_,
                   fold_specific_params = NULL,
                   summary_table = NULL,
                   task_type = list(),
                   n_folds_per_cluster = NA_integer_,
                   smpls_cluster_ = NULL,
                   var_scaling_factor_theta = NA_real_,
                   initialize_double_ml = function(data,
                                                   n_folds,
                                                   n_rep,
                                                   score,
                                                   dml_procedure,
                                                   draw_sample_splitting,
                                                   apply_cross_fitting){

                     assert_class(data, "xtdml_data")
                     if (data$n_cluster_vars != 2 && data$n_cluster_vars != 1) {
                       stop("Specify one or two cluster variables.")
                     }
                     private$data_ = data

                     # initialize learners and parameters which are set model specific
                     private$learner_ = NULL
                     private$params_ = NULL

                     # Set fold_specific_params = FALSE at instantiation
                     private$fold_specific_params = FALSE

                     # check resampling specifications
                     assert_count(n_folds)
                     assert_count(n_rep)
                     assert_logical(apply_cross_fitting, len = 1)
                     assert_logical(draw_sample_splitting, len = 1)

                     # set resampling specifications
                     if ((n_folds == 1) | (!apply_cross_fitting)) {
                       stop(paste(
                         "No cross-fitting (`apply_cross_fitting = False`)"))
                     }
                     private$n_folds_per_cluster = n_folds
                     private$n_folds_ = n_folds^self$data$n_cluster_vars
                     private$n_rep_ = n_rep
                     private$apply_cross_fitting_ = apply_cross_fitting
                     private$draw_sample_splitting_ = draw_sample_splitting

                     # check and set dml_procedure and score
                     assert_choice(dml_procedure, c("dml1", "dml2"))
                     private$dml_procedure_ = dml_procedure
                     private$score_ = score

                     if (self$n_folds == 1 & self$apply_cross_fitting) {
                       message(paste(
                         "apply_cross_fitting is set to FALSE.",
                         "Cross-fitting is not supported for n_folds = 1."))
                       private$apply_cross_fitting_ = FALSE
                     }

                     if (!self$apply_cross_fitting) {
                       if (self$n_folds > 2) {
                         stop(paste(
                           "Estimation without cross-fitting not supported for",
                           "n_folds > 2."))
                       }
                       if (self$dml_procedure == "dml2") {
                         # redirect to dml1 which works out-of-the-box; dml_procedure is of no
                         # relevance without cross-fitting
                         private$dml_procedure_ = "dml1"
                       }
                     }

                     # perform sample splitting
                     if (self$draw_sample_splitting) {
                       self$split_samples()
                     } else {
                       private$smpls_ = NULL
                     }

                     # initialize arrays according to obj_dml_data and the resampling settings
                     private$initialize_arrays()

                     # initialize instance attributes which are later used for iterating
                     invisible(self)
                   },
                   assert_learner = function(learner, learner_name, Regr, Classif) {

                     assert(
                       check_character(learner, max.len = 1),
                       check_class(learner, "Learner"))

                     if (test_class(learner, "AutoTuner")) {
                       stop(paste0(
                         "Learners of class 'AutoTuner' are not supported."
                       ))
                     }
                     if (is.character(learner)) {
                       # warning("Learner provision by character() will be deprecated in the
                       # future.")
                       learner = lrn(learner)
                     }

                     if ((Regr & learner$task_type == "regr") |
                         (Classif & learner$task_type == "classif")) {
                       private$task_type[learner_name] = learner$task_type
                     }

                     if ((Regr & !Classif & !learner$task_type == "regr")) {
                       stop(paste0(
                         "Invalid learner provided for ", learner_name,
                         ": 'learner$task_type' must be 'regr'"))
                     }
                     if ((Classif & !Regr & !learner$task_type == "classif")) {
                       stop(paste0(
                         "Invalid learner provided for ", learner_name,
                         ": 'learner$task_type must be 'classif'"))
                     }
                     invisible(learner)
                   },
                   assert_tune_settings = function(tune_settings) {

                     valid_learner = self$learner_names()

                     if (!test_names(names(tune_settings), must.include = "terminator")) {
                       stop(paste(
                         "Invalid tune_settings\n",
                         "object 'terminator' is missing."))
                     }
                     assert_class(tune_settings$terminator, "Terminator")

                     if (test_names(names(tune_settings), must.include = "n_folds_tune")) {
                       assert_integerish(tune_settings$n_folds_tune, len = 1, lower = 2)
                     } else {
                       tune_settings$n_folds_tune = 5
                     }

                     if (test_names(names(tune_settings), must.include = "rsmp_tune")) {
                       assert(
                         check_character(tune_settings$rsmp_tune),
                         check_class(tune_settings$rsmp_tune, "Resampling"))
                       if (!test_class(tune_settings$rsmp_tune, "Resampling")) {
                         if (tune_settings$rsmp_tune == "cv") {
                           tune_settings$rsmp_tune = rsmp(tune_settings$rsmp_tune,
                                                          folds = tune_settings$n_folds_tune)
                         } else {
                           tune_settings$rsmp_tune = rsmp(tune_settings$rsmp_tune)
                         }
                       }
                     } else {
                       tune_settings$rsmp_tune = rsmp("cv", folds = tune_settings$n_folds_tune)
                     }

                     if (test_names(names(tune_settings), must.include = "measure") && !is.null(tune_settings$measure)) {
                       assert_list(tune_settings$measure)
                       if (!test_names(names(tune_settings$measure),
                                       subset.of = valid_learner)) {
                         stop(paste(
                           "Invalid name of measure", paste0(names(tune_settings$measure),
                                                             collapse = ", "),
                           "\n measure must be a named list with elements named",
                           paste0(valid_learner, collapse = ", ")))
                       }
                       for (i_msr in seq_len(length(tune_settings$measure))) {
                         assert(
                           check_character(tune_settings$measure[[i_msr]]),
                           check_class(tune_settings$measure[[i_msr]], "Measure"))
                       }
                     } else {
                       tune_settings$measure = rep(list(NULL), length(valid_learner))
                       names(tune_settings$measure) = valid_learner
                     }

                     for (this_learner in valid_learner) {
                       if (!test_class(tune_settings$measure[[this_learner]], "Measure")) {
                         tune_settings$measure[[this_learner]] = set_default_measure(
                           tune_settings$measure[[this_learner]],
                           private$task_type[[this_learner]])
                       }
                     }

                     # NEW: tuner (modern API)
                     if (!test_names(names(tune_settings), must.include = "tuner")) {
                       tune_settings$tuner = mlr3tuning::tnr("grid_search", resolution = 5)
                     } else {
                       assert_class(tune_settings$tuner, "Tuner")
                     }

                     return(tune_settings)
                   },
                   initialize_arrays = function() {
                     private$psi_theta_ = array(NA_real_, dim = c(
                       self$data$n_obs, self$n_rep,
                       self$data$n_treat))
                     private$psi_theta_a_ = array(NA_real_, dim = c(
                       self$data$n_obs, self$n_rep,
                       self$data$n_treat))
                     private$psi_theta_b_ = array(NA_real_, dim = c(
                       self$data$n_obs, self$n_rep,
                       self$data$n_treat))

                     private$res_y_ = array(NA_real_, dim = c(
                       self$data$n_obs, self$n_rep,
                       self$data$n_treat))
                     private$res_d_ = array(NA_real_, dim = c(
                       self$data$n_obs, self$n_rep,
                       self$data$n_treat))

                     #regression estimates
                     private$coef_theta_ = array(NA_real_, dim = c(self$data$n_treat))
                     private$se_theta_   = array(NA_real_, dim = c(self$data$n_treat))

                     private$all_coef_theta_ = array(NA_real_,
                                                     dim = c(self$data$n_treat, self$n_rep))
                     private$all_se_theta_ = array(NA_real_,
                                                   dim = c(self$data$n_treat, self$n_rep))
                     #metrics
                     private$model_rmse_    = array(NA_real_, dim = c(self$data$n_treat))
                     private$all_model_mse_ = array(NA_real_, dim = c(
                       self$data$n_obs, self$n_rep,
                       self$data$n_treat))

                     if (self$dml_procedure == "dml1") {
                       if (self$apply_cross_fitting) {
                         private$all_dml1_coef_theta_ = array(NA_real_, dim = c(
                           self$data$n_treat, self$n_rep,
                           self$n_folds))
                       } else {
                         private$all_dml1_coef_theta_ = array(NA_real_, dim = c(
                           self$data$n_treat, self$n_rep,
                           1))
                       }
                     }
                   },
                   initialize_predictions_and_targets = function() {
                     private$predictions_ = sapply(self$params_names(),
                                                   function(key) {
                                                     array(NA_real_, dim = c(
                                                       self$data$n_obs, self$n_rep,
                                                       self$data$n_treat))
                                                   },
                                                   simplify = F)
                     private$targets_ = sapply(self$params_names(),
                                               function(key) {
                                                 array(NA_real_, dim = c(
                                                   self$data$n_obs, self$n_rep,
                                                   self$data$n_treat))
                                               },
                                               simplify = F)
                   },
                   initialize_rmses = function() {
                     private$rmses_ = sapply(self$params_names(),
                                             function(key) {
                                               array(NA_real_, dim = c(
                                                 self$n_rep,
                                                 self$data$n_treat))
                                             },
                                             simplify = F)
                   },
                   initialize_models = function() {
                     private$models_ = sapply(self$params_names(),
                                              function(x) {
                                                sapply(self$data$d_cols,
                                                       function(x) {
                                                         lapply(
                                                           seq(self$n_rep),
                                                           function(x) vector("list", length = self$n_folds))
                                                       },
                                                       simplify = F)
                                              },
                                              simplify = F)
                   },
                   store_predictions_and_targets = function(preds, targets) {
                     for (learner in self$params_names()) {
                       if (!is.null(preds[[learner]])) {
                         private$predictions_[[learner]][ , private$i_rep,
                                                          private$i_treat] = preds[[learner]]
                         private$targets_[[learner]][ , private$i_rep,
                                                      private$i_treat] = targets[[learner]]

                       }
                     }
                   },
                   calc_rmses = function(preds, targets){ ##ap
                     for (learner in self$params_names()) {
                       if (is.null(targets[[learner]])){
                         private$rmses_[[learner]] [private$i_rep, private$i_treat] = NULL #rep(NA,n_obs) ##
                       }else{
                         private$rmses_[[learner]][private$i_rep,
                                                   private$i_treat] = MLmetrics::RMSE(preds[[learner]],targets[[learner]])
                       }
                     }
                   },
                   store_models = function(models) {
                     for (learner in self$params_names()) {
                       if (!is.null(models[[learner]])) {
                         private$models_[[learner]][[self$data$treat_col]][[
                           private$i_rep]] = models[[learner]]
                       }
                     }
                   },
                   get__smpls = function() self$smpls[[private$i_rep]],
                   get__smpls_cluster = function() self$smpls_cluster[[private$i_rep]],
                   get__psi_theta = function() self$psi_theta[, private$i_rep, private$i_treat],
                   get__psi_theta_a = function() self$psi_theta_a[, private$i_rep, private$i_treat],
                   get__psi_theta_b = function() self$psi_theta_b[, private$i_rep, private$i_treat],
                   get__all_coef_theta = function() self$all_coef_theta[private$i_treat, private$i_rep],
                   get__all_se_theta = function() self$all_se_theta[private$i_treat, private$i_rep],
                   get__all_model_mse = function() self$all_model_mse[, private$i_rep, private$i_treat],
                   get__res_y = function() self$res_y[, private$i_rep, private$i_treat],
                   get__res_d = function() self$res_d[, private$i_rep, private$i_treat],

                   est_causal_pars_theta = function() {
                     dml_procedure = self$dml_procedure
                     smpls = private$get__smpls()
                     test_ids = smpls$test_ids
                     coef_theta = private$orth_est_theta()
                     return(coef_theta)
                   },
                   se_theta_causal_pars = function() {
                     se_theta = sqrt(private$var_est_theta())
                     return(se_theta)
                   },
                   agg_cross_fit = function() {
                     private$coef_theta_ = apply(
                       self$all_coef_theta, 1,
                       function(x) median(x, na.rm = TRUE))

                     private$se_theta_ = sqrt(apply(
                       private$var_scaling_factor_theta * self$all_se_theta^2 + (self$all_coef_theta - self$coef_theta)^2,
                       1, function(x) median(x, na.rm = TRUE)) / private$var_scaling_factor_theta)

                     invisible(self)
                   },
                   var_est_theta = function() {
                     psi_theta_a = private$get__psi_theta_a()
                     psi_theta   = private$get__psi_theta()

                     if (self$data$n_cluster_vars == 1) {
                       this_cluster_var = self$data$data_model[[self$data$cluster_cols[1]]]
                       clusters = unique(this_cluster_var)
                       gamma_hat = 0
                       j_hat = 0
                       smpls = private$get__smpls()
                       smpls_cluster = private$get__smpls_cluster()
                       for (i_fold in 1:self$n_folds) {   ##n_folds
                         test_inds = smpls$test_ids[[i_fold]]
                         test_cluster_inds = smpls_cluster$test_ids[[i_fold]]
                         I_k = test_cluster_inds[[1]]
                         const = 1 / length(I_k)
                         for (cluster_value in I_k) {
                           ind_cluster = (this_cluster_var == cluster_value)
                           gamma_hat = gamma_hat + const * sum(outer(
                             psi_theta[ind_cluster],
                             psi_theta[ind_cluster]))
                         }
                         j_hat = j_hat + sum(psi_theta_a[test_inds]) / length(I_k)
                       }
                       gamma_hat = gamma_hat / private$n_folds_per_cluster
                       j_hat = j_hat / private$n_folds_per_cluster
                       private$var_scaling_factor_theta = length(clusters)
                       sigma2_theta_hat = gamma_hat / (j_hat^2) / private$var_scaling_factor_theta
                     } else {
                       assert_choice(self$data$n_cluster_vars, 2)
                       first_cluster_var = self$data$data_model[[self$data$cluster_cols[1]]]
                       second_cluster_var = self$data$data_model[[self$data$cluster_cols[2]]]
                       gamma_hat = 0
                       j_hat = 0
                       smpls = private$get__smpls()
                       smpls_cluster = private$get__smpls_cluster()
                       for (i_fold in 1:self$n_folds) {
                         test_inds = smpls$test_ids[[i_fold]]
                         test_cluster_inds = smpls_cluster$test_ids[[i_fold]]
                         I_k = test_cluster_inds[[1]]
                         J_l = test_cluster_inds[[2]]
                         const = min(length(I_k), length(J_l)) / ((length(I_k) * length(J_l))^2)
                         for (cluster_value in I_k) {
                           ind_cluster = (first_cluster_var == cluster_value) &
                             second_cluster_var %in% J_l
                           gamma_hat = gamma_hat + const * sum(outer(
                             psi_theta[ind_cluster],
                             psi_theta[ind_cluster]))
                         }
                         for (cluster_value in J_l) {
                           ind_cluster = (second_cluster_var == cluster_value) &
                             first_cluster_var %in% I_k
                           gamma_hat = gamma_hat + const * sum(outer(
                             psi_theta[ind_cluster],
                             psi_theta[ind_cluster]))
                         }
                         j_hat = j_hat + sum(psi_theta_a[test_inds]) / (length(I_k) * length(J_l))
                       }
                       gamma_hat = gamma_hat / (private$n_folds_per_cluster^2)
                       j_hat = j_hat / (private$n_folds_per_cluster^2)
                       n_first_clusters = length(unique(first_cluster_var))
                       n_second_clusters = length(unique(second_cluster_var))
                       private$var_scaling_factor_theta = min(n_first_clusters, n_second_clusters)
                       sigma2_theta_hat = gamma_hat / (j_hat^2) / private$var_scaling_factor_theta
                     }
                     return(sigma2_theta_hat)
                   },
                   orth_est_theta = function() {

                     dml_procedure = self$dml_procedure

                     psi_theta_a = private$get__psi_theta_a()
                     psi_theta_b = private$get__psi_theta_b()
                     res_y = private$get__res_y()
                     res_d = private$get__res_d()

                     smpls = private$get__smpls()
                     test_ids = smpls$test_ids
                     smpls_cluster = private$get__smpls_cluster()

                     if (dml_procedure == "dml1") {
                       thetas = rep(NA_real_, length(test_ids))
                       rmses  = rep(NA_real_, length(test_ids))

                       for (i_fold in seq_len(length(test_ids))) {
                         test_index = test_ids[[i_fold]]
                         test_cluster_inds = smpls_cluster$test_ids[[i_fold]]
                         xx = sapply(
                           test_cluster_inds,
                           function(x) length(x))
                         scaling_factor = 1 / prod(xx)
                         thetas[i_fold] = -(scaling_factor * sum(psi_theta_b[test_index])) /
                           (scaling_factor * sum(psi_theta_a[test_index]))
                         rmses[i_fold] = sqrt(scaling_factor * sum((res_y[test_index] - res_d[test_index] * thetas[i_fold])^2))
                       }
                       theta = mean(thetas, na.rm = TRUE)
                       private$all_dml1_coef_theta_[private$i_treat, private$i_rep, ] = thetas
                       model_rmse = mean(rmses, na.rm = TRUE)
                       private$model_rmse_ = model_rmse

                     } else if (dml_procedure == "dml2") {
                       # See Chiang et al. (2021) Algorithm 1
                       psi_theta_a = private$get__psi_theta_a()
                       psi_theta_b = private$get__psi_theta_b()
                       res_y = private$get__res_y()
                       res_d = private$get__res_d()

                       psi_theta_a_subsample_mean = 0.
                       psi_theta_b_subsample_mean = 0.
                       theta_subsample_mean = 0.
                       rmses_subsample_mean = 0.

                       for (i_fold in seq_len(length(test_ids))) {
                         test_index = test_ids[[i_fold]]
                         test_cluster_inds = smpls_cluster$test_ids[[i_fold]]
                         xx = sapply(
                           test_cluster_inds,
                           function(x) length(x))
                         scaling_factor = 1 / prod(xx)
                         psi_theta_a_subsample_mean = psi_theta_a_subsample_mean +
                           scaling_factor * sum(psi_theta_a[test_index])
                         psi_theta_b_subsample_mean = psi_theta_b_subsample_mean +
                           scaling_factor * sum(psi_theta_b[test_index])

                         theta_subsample_mean =  - psi_theta_b_subsample_mean / psi_theta_a_subsample_mean
                         rmses_subsample_mean =  sqrt(scaling_factor * sum((res_y[test_index] - res_d[test_index] * theta_subsample_mean)^2))

                       }
                       theta = -psi_theta_b_subsample_mean / psi_theta_a_subsample_mean
                       model_rmse = rmses_subsample_mean

                       private$model_rmse_ = model_rmse
                     }
                     return(theta)
                   },
                   compute_score_theta = function() {
                     psi_theta = private$get__psi_theta_a() * private$get__all_coef_theta() + private$get__psi_theta_b()
                     return(psi_theta)
                   }
                 )
)
