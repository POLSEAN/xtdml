# XTDML
The `xtdml` package implements double machine learning (DML) for static partially linear regression models for panel data with fixed effects, as in [Clarke and Polselli (2025)](https://academic.oup.com/ectj/advance-article/doi/10.1093/ectj/utaf011/8120202?login=false). 
The `xtdml` package is built on the `DoubleML` package by Bach et al. (2022) using the `mlr3` ecosystem and the same notation.

## The Partially Linear Panel Regression Model
The `xtdml` package estimates the structural (causal) parameter from panel data models: 
```math
  Y_{it} = \theta_0 D_{it} + g_0(X_{it}) + \alpha_i + U_{it}
```
```math  
  D_{it} = m_0(X_{it}) + \gamma_i + V_{it},
```
where 
  * $Y_{it}$ is the output, $D_{it}$ the treatment, $X_{it}$ the covariates
  * $\theta_0$ is the structural (causal) parameter to *estimate*; 
  * $(l_0, m_0)$ are (possibly nonlinear) nuisance functions to *learn* from the data using one of the tree proposed approaches ("fd-exact", "wg-approx", "cre");
  * ($\alpha_i, \gamma_i$) are the unobserved individual heterogeneity correlated with the included covariates;
  * ($U_{it}$, $V_{it}$) are disturbances.

> [!NOTE]
> The current version of the package allows the user to immediately use the estimation tools *without the need to* proceed with additional data managing and transformations.
> In particular, the user can choose:
>
>  1. The panel data approach to use among `approach = ("fd-exact", "wg-approx", "cre")`; default is `"fd-exact"`. `xtdml` proceeds with transforming the data based on the selected approach, following Clarke and Polselli (2025).
>
> 2. The type of transformation to apply to the covariates $X$ in the data set among `transformX = ("no", "minmax", "poly")`. `"no"` does not transform the covariates `X` and is recommended for tree-based learners. `"minmax"` applies the Min-Max normalization  $x' = (x-x_{min})/(x_{max}-x_{min})$ to the covariates and is recommended with neural networks. `"poly"` add polynomials up to order three and interactions between all possible combinations of two and three variables; this is recommended for Lasso. Default is `"no"`.


> [!CAUTION]
> 1. The package has been renamed `xtdml` from `XTDML`!
> 2. Major changes in xtdml_data_from_data_frame() function have been introduced since version 0.1.7 and various bugs corrected.
> 3. `tune_settings` have been updated after `mlr3tuning` (>= 0.20.0),  `mlr3tuning` (>= 0.19.0), `bbotk` (>= 1.6.0)
> 4. Previous versions of the package allowed for **treatment endogeneity** in the partially linear panel regression model. This option has been *temporally removed*, but we are currently working on including IV estimation and weak IV tests in the current version of the `xtdml` package. (Panel IV DML *coming soon*) 
> 5. With the current version of the package it is **no longer possible** to choose the **hybrid approach**; this is consistent with the published version of the article.

## Installing the package
The installation of `xtdml` can be done from CRAN using
```
install.packages("xtdml")
library(xtdml)
```
or via devtools from GitHub for the latest version
```
library(devtools)
install_github("POLSEAN/xtdml")
```


## Sample Code

```
library(xtdml)
library(mlr3)
library(mlr3learners)
library(paradox)
library(mlr3tuning)
library(mlr3misc)
library(xgboost)
library(tibble)
library(dplyr)
library(datawizard)
library(data.table)

# Suppress warning messages
lgr::get_logger("mlr3")$set_threshold("warn")
lgr::get_logger("bbotk")$set_threshold("warn")

#______________________________________________________________________________#
# Generate data
#______________________________________________________________________________#
set.seed(1234)

# Load data
df = make_plpr_data(n_obs = 1000, t_per = 10, dim_x = 20, theta = 0.5, rho=0.8)

approaches = c("fd-exact","cre", "wg-approx")
for (approach in approaches){
  # Initialize data environment
  x_cols = paste0("X", 1:20)
  obj_xtdml_data = xtdml_data_from_data_frame(df,
                    x_cols = x_cols,  y_col = "y", d_cols = "d",
                    panel_id = "id",
                    time_id = "time",
                    approach = approach,
                    transformX = "no")
  print(obj_xtdml_data)

  # Gradient boosting
  learner = lrn("regr.xgboost", nrounds = 100)
  ml_m = learner$clone()
  ml_l = learner$clone()

  type_score = c("orth-PO", "orth-IV")

    print(paste0("approach:", approach))
    for (score in type_score){
     if(score == "orth-PO"){
      xtdml_obj = xtdml_plr$new(obj_xtdml_data,
                                ml_l = ml_l, ml_m = ml_m,
                                n_folds = 5,
                                score = score)

      param_grid = list("ml_l" = ps(max_depth = p_int(2,10),
                                    lambda = p_dbl(0,2)),
                        "ml_m" = ps(max_depth = p_int(2,10),
                                    lambda = p_dbl(0,2)))

     }else if (score == "orth-IV"){
      ml_g = learner$clone()
      xtdml_obj = xtdml_plr$new(obj_xtdml_data,
                                ml_l = ml_l, ml_m = ml_m, ml_g = ml_g,
                                n_folds = 5,
                                score = score)

      param_grid = list("ml_l" = ps(max_depth = p_int(2,10),
                                    lambda = p_dbl(0,2)),
                        "ml_m" = ps(max_depth = p_int(2,10),
                                    lambda = p_dbl(0,2)),
                        "ml_g" = ps(max_depth = p_int(2,10),
                                    lambda = p_dbl(0,2)))
    }

    tune_settings = list(n_folds_tune = 5,
                         rsmp_tune  = mlr3::rsmp("cv", folds = 5),
                         terminator = mlr3tuning::trm("evals", n_evals = 5),
                         tuner      = tnr("grid_search", resolution = 10))

    xtdml_obj$tune(param_set = param_grid, tune_settings = tune_settings)

    # Fit DML model and print results
    xtdml_obj$fit()

    #xtdml_obj$print()
    xtdml_obj$summary()

    ci = xtdml_obj$confint()
  }

  print(xtdml_obj$params)

  table = matrix(0, 10, 1)
  table[,1] = cbind(xtdml_obj$coef_theta,
                    xtdml_obj$se_theta,
                    xtdml_obj$pval_theta,
                    ci[ , 1], ci[ , 2],
                    xtdml_obj$model_rmse,
                    as.numeric(xtdml_obj$rmses["ml_l"]),
                    as.numeric(xtdml_obj$rmses["ml_m"]),
                    xtdml_obj$data$n_obs,
                    length(unique(xtdml_obj$data$data_model[[xtdml_obj$data$cluster_cols]])))


  rownames(table)= c("Estimate", "Std. Error", "P-value", "Lower bound 95% CI", "Upper bound 95% CI",
                     "Model RMSE", "MSE of l", "MSE of m","No. observations","No. groups")
  colnames(table)= c("xtdml-boosting")
  formatted_table = matrix("", nrow=10, ncol=1)
  formatted_table[1:8, 1] = formatC(table[1:8, 1], digits=4, format="f")
  formatted_table[9:10, 1] = formatC(table[9:10, 1], digits=0, format="f")

  # Keep row and column names
  rownames(formatted_table) = rownames(table)
  colnames(formatted_table) = colnames(table)

  # Print
  print(formatted_table, quote = FALSE)
}

```
 

## References
Bach, P., Chernozhukov, V., Kurz, M. S., Spindler, M. and Klaassen, S. (2024), DoubleML - An Object-Oriented Implementation of Double Machine Learning in R, *Journal of Statistical Software*, 108(3): 1-56, doi:10.18637/jss.v108.i03.

Bach P, Kurz MS, Chernozhukov V, Spindler M, Klaassen S (2024b). **DoubleML**: Double Machine Learning in R. R package version 1.0.2, URL https://cran.r-project.org/
web/packages/DoubleML/index.html.

Chernozhukov, V., Chetverikov, D., Demirer, M., Duflo, E., Hansen, C., Newey, W., and Robins, J. (2018). Double/debiased machine learning for treatment and structural parameters. *The Econometrics Journal*, 21(1):C1–C68.

Clarke, P. S. and Polselli,  A. (2025). Double Machine Learning for Static Panel Models with Fixed Effects. *The Econometrics Journal*. DOI: 10.1093/ectj/utaf011.

Polselli A (2025). **xtdml**: Double Machine Learning for Static Panel Models with Fixed Effects. R package version 0.1.5, URL https://cran.r-project.org/web/packages/xtdml/
index.html

