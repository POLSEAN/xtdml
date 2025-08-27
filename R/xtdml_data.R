#' @title Set up for data for panel data approaches and up two cluster variables
#'
#' @description
#' Double machine learning (DML) data-backend for data with cluster variables.
#' `xtdml_data` sets up the data environment for panel data analysis with transformed variables.
#'
#' `xtdml_data` objects can be initialized from a
#' [data.table][data.table::data.table()]. The following functions can be used to create a new
#' instance of `xtdml_data`.
#' * `xtdml_data$new()` for initialization from a `data.table`.
#' * [xtdml_data_from_data_frame()] for initialization from a `data.frame`.
#' @export
xtdml_data = R6Class("xtdml_data",
                              active = list(
                                #' @field all_variables (`character()`)\cr
                                #' All variables available in the data frame.
                                all_variables = function(value) {
                                  if (missing(value)) {
                                    return(names(self$data))
                                  } else {
                                    stop("can't set field all_variables")
                                  }
                                },

                                #' @field d_cols (`character()`)\cr
                                #' The treatment variable.
                                d_cols = function(value) {
                                  if (missing(value)) {
                                    return(private$d_cols_)
                                  } else {
                                    d_cols = value
                                    reset_value = !is.null(self$data_model)
                                    assert_character(d_cols, unique = TRUE)
                                    assert_subset(d_cols, self$all_variables)
                                    private$d_cols_ = d_cols
                                    if (reset_value) {
                                      private$check_disjoint_sets()
                                      self$set_data_model(self$d_cols[1])
                                    }
                                  }
                                },
                                #' @field dbar_col (`NULL`, character()`)\cr
                                #' The individual mean of the treatment variable.
                                #'
                                dbar_col = function(value) {
                                  if (missing(value)) {
                                    return(private$dbar_col_)
                                  } else {
                                    dbar_col = value # to get more meaningful assert error messages
                                    reset_value = !is.null(self$data_model)
                                    private$dbar_col_ = dbar_col
                                    if (reset_value) {
                                      private$check_disjoint_sets()
                                      self$set_data_model(self$dbar_col[1])
                                    }
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

                                #' @field data_model ([`data.table`][data.table::data.table()])\cr
                                #' Internal data object that implements the causal panel model as specified by
                                #' the user via `y_col`, `d_cols`, `x_cols`, `dbar_col`.
                                data_model = function(value) {
                                  if (missing(value)) {
                                    return(private$data_model_)
                                  } else {
                                    stop("can't set field data_model")
                                  }
                                },

                                #' @field n_obs (`integer(1)`) \cr
                                #' The number of observations.
                                n_obs = function(value) {
                                  if (missing(value)) {
                                    return(dim(self$data)[1])
                                  } else {
                                    stop("can't set field n_obs")
                                  }
                                },

                                #' @field n_treat (`integer(1)`) \cr
                                #' The number of treatment variables.
                                n_treat = function(value) {
                                  if (missing(value)) {
                                    return(length(self$d_cols))
                                  } else {
                                    stop("can't set field n_treat")
                                  }
                                },

                                #' @field treat_col (`character(1)`) \cr
                                #' "Active" treatment variable in the multiple-treatment case.
                                treat_col = function(value) {
                                  if (missing(value)) {
                                    return(private$treat_col_)
                                  } else {
                                    stop("can't set field treat_col")
                                  }
                                },

                                #' @field x_cols (`character()`) \cr
                                #' The covariates.
                                x_cols = function(value) {
                                  if (missing(value)) {
                                    return(private$x_cols_)
                                  } else {
                                    x_cols = value
                                    reset_value = !is.null(self$data_model)
                                    if (!is.null(x_cols)) {
                                      assert_character(x_cols, unique = TRUE)
                                    }

                                    if (!is.null(x_cols)) {
                                      assert_subset(x_cols, self$all_variables)
                                      private$x_cols_ = x_cols
                                    }
                                    if (reset_value) {
                                      private$check_disjoint_sets()
                                      self$set_data_model(self$d_cols[1])
                                    }
                                  }
                                },

                                #' @field y_col (`character(1)`) \cr
                                #' The outcome variable.
                                y_col = function(value) {
                                  if (missing(value)) {
                                    return(private$y_col_)
                                  } else {
                                    y_col = value
                                    reset_value = !is.null(self$data_model)
                                    assert_character(y_col, len = 1)
                                    assert_subset(y_col, self$all_variables)
                                    private$y_col_ = y_col
                                    if (reset_value) {
                                      private$check_disjoint_sets()
                                      self$set_data_model(self$d_cols[1])
                                    }
                                  }
                                },

                                #' @field cluster_cols (`character()`)\cr
                                #' The cluster variable(s).
                                cluster_cols = function(value) {
                                  if (missing(value)) {
                                    return(private$cluster_cols_)
                                  } else {
                                    cluster_cols = value
                                    reset_value = !is.null(self$data_model)
                                    assert_character(cluster_cols, unique = TRUE)
                                    assert_subset(cluster_cols, self$all_variables)
                                    private$cluster_cols_ = cluster_cols
                                    if (reset_value) {
                                      private$check_disjoint_sets()
                                      self$set_data_model(self$d_cols[1])
                                    }
                                  }
                                },
                                #' @field n_cluster_vars (`integer(1)`) \cr
                                #' The number of cluster variables.
                                n_cluster_vars = function(value) {
                                  if (missing(value)) {
                                    return(length(self$cluster_cols))
                                  } else {
                                    stop("can't set field n_cluster_vars")
                                  }
                                },

                                #' @field approach (`character(1)`) \cr
                                #' A `character()` (`"fd-exact"`, `"wg-approx"` or `"cre"`) specifying the panel data
                                #' technique to apply to estimate the causal model. Default is `"fd-exact"`.
                                approach = function(value) {
                                  if (missing(value)){
                                    return(private$approach_)
                                  } else{
                                    stop("Can't set `approach`.")
                                  }
                                },

                                #' @field transformX (`character(1)`) \cr
                                #' A `character()` (`"no"`, `"minmax"` or `"poly"`) specifying the type
                                #' of transformation to apply to the X data. `"no"` does not transform the covariates `X`
                                #' and is recommended for tree-based learners. `"minmax"` applies the Min-Max normalization
                                #' \eqn{x' = (x-x_{min})/(x_{max}-x_{min})} to the covariates and is recommended with neural networks.
                                #' `"poly"` add polynomials up to order three and interactions between all possible
                                #' combinations of two and three variables; this is recommended for Lasso. Default is `"no"`.
                                transformX = function(value) {
                                  if (missing(value)){
                                    return(private$transformX_)
                                  } else{
                                    stop("Can't set `transformX`.")
                                  }
                                }
                                ),
                              public = list(
                                #' @description
                                #' Creates a new instance of this [R6][R6::R6Class] class.
                                #'
                                #' @param data ([`data.table`][data.table::data.table()], `data.frame()`)\cr
                                #' Data object.
                                #'
                                #' @param y_col (`character(1)`) \cr
                                #' The outcome variable.
                                #'
                                #' @param d_cols (`character(1)`) \cr
                                #' The treatment variable.
                                #'
                                #' @param x_cols (`character()`) \cr
                                #'
                                #' @param dbar_col (`NULL`, character()`) \cr
                                #' Individual mean of the treatment variable
                                #' (used for the CRE approach). Default is `NULL`.
                                #'
                                #' @param approach (`character(1)`) \cr
                                #' A `character()` (`"fd-exact"`, `"wg-approx"` or `"cre"`)
                                #' specifying the panel data technique to apply
                                #' to estimate the causal model. Default is `"fd-exact"`.
                                #'
                                #' @param transformX (`character(1)`) \cr
                                #' A `character()` (`"no"`, `"minmax"` or `"poly"`) specifying the type
                                #' of transformation to apply to the X data. `"no"` does not transform the covariates `X`
                                #' and is recommended for tree-based learners. `"minmax"` applies the Min-Max normalization
                                #' \eqn{x' = (x-x_{min})/(x_{max}-x_{min})} to the covariates and is recommended with neural networks.
                                #' `"poly"` add polynomials up to order three and interactions between all possible
                                #' combinations of two and three variables; this is recommended for Lasso.
                                #' Default is `"no"`.
                                #'
                                #' @param cluster_cols (`character()`) \cr
                                #' The cluster variable(s).
                                #'
                                initialize = function(data = NULL,
                                                      x_cols = NULL,
                                                      y_col = NULL,
                                                      d_cols = NULL,
                                                      dbar_col = NULL,
                                                      cluster_cols = NULL,
                                                      approach = NULL,
                                                      transformX = NULL
                                                      )
                                  {

                                  if (all(class(data) == "data.frame")) {
                                    data = data.table(data)
                                  }
                                  assert_class(data, "data.table")
                                  assert_character(names(data), unique = TRUE)

                                  private$data_ = data

                                  self$cluster_cols = cluster_cols

                                  self$y_col  = y_col
                                  self$d_cols = d_cols
                                  self$x_cols = x_cols
                                  self$dbar_col = dbar_col
                                  private$check_disjoint_sets()

                                  private$approach_ = approach
                                  private$transformX_ = transformX

                                  # by default, we initialize to the first treatment variable
                                  self$set_data_model(d_cols[1])

                                  invisible(self)
                                },

                                #' @description
                                #' Print `xtdml_data` objects.
                                print = function() {
                                  header = "================= xtdml Object ==================\n"
                                  data_info = paste0(
                                    "Outcome variable: ", self$y_col, "\n",
                                    "Treatment variable(s): ", paste0(self$d_cols, collapse = ", "), "\n",
                                    "Cluster variable(s): ", paste0(self$cluster_cols, collapse = ", "),
                                    "\n",
                                    "Covariates: ", paste0(c(self$x_cols, self$dbar_col), collapse = ", "), "\n",
                                    "No. Observations: ", self$n_obs, "\n",
                                    "Panel data approach: ", private$approach_, "\n",
                                    "Type of transformation for X: ", private$transformX_, "\n")
                                  cat(header, "\n",
                                      "\n------------------ Data summary ------------------\n",
                                      data_info,
                                      sep = "")

                                  invisible(self)
                                },

                                #' @description
                                #' Setter function for `data_model`. The function implements the causal model
                                #' as specified by the user via `y_col`, `d_cols`, `x_cols` and
                                #' `cluster_cols` and assigns the role for the treatment variables in the
                                #' multiple-treatment case.
                                #' @param treatment_var (`character()`)\cr
                                #' Active treatment variable that will be set to `treat_col`.
                                set_data_model = function(treatment_var) {

                                  assert_character(treatment_var, max.len = 1)
                                  assert_subset(treatment_var, self$d_cols)

                                  private$treat_col_ = treatment_var

                                  if (self$n_treat > 1) {
                                    stop("Specify one treatment variable at a time.")
                                  }
                                  # add the cluster_cols to the data_model_
                                  col_indx = c(
                                    self$x_cols,
                                    self$y_col,
                                    self$treat_col,
                                    self$dbar_col,
                                    self$cluster_cols
                                  )
                                  private$data_model_ = self$data[, col_indx, with = FALSE]
                                  stopifnot(nrow(self$data) == nrow(self$data_model))

                                  invisible(self)
                                }
                              ),
                              private = list(
                                data_ = NULL,
                                x_cols_ = NULL,
                                y_col_ = NULL,
                                d_cols_ = NULL,
                                dbar_col_ = NULL,
                                cluster_cols_ = NULL,
                                treat_col_ = NULL,
                                data_model_ = NULL,
                                approach_ = NULL,
                                transformX_ = NULL,
                                check_disjoint_sets = function() {

                                 # super$check_disjoint_sets()
                                  y_col     = self$y_col
                                  x_cols    = self$x_cols
                                  d_cols    = self$d_cols
                                  dbar_col  = self$dbar_col

                                  cluster_cols = self$cluster_cols
                                  approach = self$approach
                                  transform = self$transform

                                  if (y_col %in% x_cols) {
                                    stop(paste(
                                      y_col,
                                      "cannot be set as outcome variable 'y_col' and",
                                      "covariate in 'x_cols'."))
                                  }
                                  if (y_col %in% d_cols) {
                                    stop(paste(
                                      y_col,
                                      "cannot be set as outcome variable 'y_col' and",
                                      "treatment variable in 'd_cols'."))
                                  }
                                  if (y_col %in% cluster_cols) {
                                    stop(paste(
                                      y_col,
                                      "cannot be set as outcome variable 'y_col' and",
                                      "cluster variable in 'cluster_cols'."))
                                  }
                                  if (any(d_cols %in% x_cols)) {
                                    stop(paste(
                                      "At least one variable/column is set as treatment",
                                      "variable ('d_cols') and as a covariate ('x_cols').",
                                      "Consider using parameter 'use_other_treat_as_covariate'."))
                                  }
                                  if (any(d_cols %in% cluster_cols)) {
                                    stop(paste(
                                      "At least one variable/column is set as treatment",
                                      "variable ('d_cols') and as a cluster variable ('cluster_cols')."))
                                  }
                                  if (any(cbind(x_cols) %in% cluster_cols)) {
                                    stop(paste(
                                      "At least one variable/column is set as covariate ('x_cols')",
                                      "and as a cluster variable ('cluster_cols')."))
                                  }
                                }
                              )
)

#' @title Wrapper for Double machine learning data-backend initialization from
#' data.frame.
#'
#' @description
#' Initalization of DoubleMLData from `data.frame`.
#'
#' @param df (`data.frame()`)\cr
#' Data object.
#'
#' @param y_col (`character(1)`) \cr
#' The outcome variable.
#'
#' @param d_cols (`character()`) \cr
#' The treatment variable(s).
#'
#' @param x_cols (`character()`) \cr
#' The covariates.
#'
#' @param cluster_cols (`NULL`, `character()`) \cr
#' The cluster variables. Default is `NULL`.
#'
#' @param approach (`character(1)`) \cr
#' A `character()` (`"fd-exact"`, `"wg-approx"` or `"cre"`) specifying the panel data
#' technique to apply to estimate the causal model. Default is `"fd-exact"`.
#'
#' @param transformX (`character(1)`) \cr
#' A `character()` (`"no"`, `"minmax"` or `"poly"`) specifying the type
#' of transformation to apply to the X data. `"no"` does not transform the covariates `X`
#' and is recommended for tree-based learners. `"minmax"` applies the Min-Max normalization
#' \eqn{x' = (x-x_{min})/(x_{max}-x_{min})} to the covariates and is recommended with neural networks.
#' `"poly"` add polynomials up to order three and interactions between all possible
#' combinations of two and three variables; this is recommended for Lasso.
#' Default is `"no"`.
#'
#' @return Creates a new instance of class `xtdml_data`.
#'
#' @examples
#'
#' \dontrun{
#' # Generate simulated panel dataset from `xtdml`
#' data = make_plpr_data(n_obs = 500, t_per = 10, dim_x = 30, theta = 0.5, rho=0.8)
#'
#' # Set up DML data environment
#' x_cols  = paste0("X", 1:30)
#'
#' obj_xtdml_data = xtdml_data_from_data_frame(data,
#'                 x_cols = x_cols,  y_col = "y", d_cols = "d",
#'                 cluster_cols = "id", approach = "fd-exact",
#'                 transformX = "no")
#'
#' obj_xtdml_data$print()
#'
#' }
#'
#' @export
#'
xtdml_data_from_data_frame = function(df,
                               x_cols = NULL, y_col = NULL, d_cols = NULL,
                               cluster_cols = NULL, approach = NULL, transformX = NULL)
  {
  if (is.null(cluster_cols)) {
    stop(print("Specify at least one (`cluster_vars`)."))
  } else {

    if (is.null(approach)){
      approach = "fd-exact"
    } else{
      valid_approach = c("fd-exact", "wg-approx", "cre")
      assert_choice(approach, valid_approach)
    }

    if (is.null(transformX)){
      transformX = "no"
    } else{
      valid_transformX = c("no", "minmax", "poly")
      assert_choice(transformX, valid_transformX)
    }

    dbar_col = NULL

    if(approach=="cre"){

      df.cre = df %>%
        group_by(across(all_of(cluster_cols))) %>%
        mutate(across(c(x_cols, d_cols), ~  mean(.x), .names = "m_{col}"))

      df.transf = as.data.frame(df.cre)

      Lx_cols = paste0("m_", x_cols)
      x_cols_plus = c(x_cols, Lx_cols)
      dbar_col = paste0("m_", d_cols)

    }else if(approach=="fd-exact"){

        df.fd = df %>%
          group_by(across(all_of(cluster_cols))) %>%
          mutate(across(x_cols, ~  lag(.x), .names = "L.{col}"))   %>%
          mutate(across(c(d_cols, y_col), ~ c(NA, diff(.x))))  %>%
          ungroup()

        complete_rows = complete.cases(df.fd)

        df.fd = df.fd[complete_rows, ]
        df.transf = as.data.frame(df.fd)

        Lx_cols = paste0("L.", x_cols)
        x_cols_plus = c(x_cols, Lx_cols)

    }else if(approach=="wg-approx"){

        if (length(cluster_cols) != 1) {
          stop("The `wg-approx` approach currently supports only one cluster column (e.g., individual ID).")
        }

        cluster_id = cluster_cols[[1]]

        df_no_idx = df %>% select(all_of(c(x_cols, y_col, d_cols)))

        df_gm = df_no_idx %>%
          summarise(across(everything(), mean, na.rm = TRUE))
        gm_list = as.list(df_gm)

        df_mi = df %>%
          group_by(across(all_of(cluster_id))) %>%
          mutate(across(all_of(c(x_cols, y_col, d_cols)), ~ mean(.x, na.rm = TRUE), .names = "m.{col}")) %>%
          ungroup()

        var_names = c(x_cols, y_col, d_cols)
        df_dm = df_no_idx

        for (v in var_names) {
          individual_mean = df_mi[[paste0("m.", v)]]
          grand_mean = gm_list[[v]]
          df_dm[[v]] = df_no_idx[[v]] - individual_mean + grand_mean
        }

        df_dm[[cluster_id]] = df[[cluster_id]]
        df.transf = as.data.frame(df_dm)
        x_cols_plus = x_cols
    }

    if (transformX=="poly"){

      dta_x = df.transf[, x_cols, drop = FALSE]
      dta2_x = polyexp(dta_x)

      if (length(Lx_cols) > 0) {
        dta_Lx = df.transf[, Lx_cols, drop = FALSE]
        dta2_Lx = polyexp(dta_Lx)
        df_expanded = cbind(dta2_x, dta2_Lx)
      } else {
        df_expanded = dta2_x
      }

      df_expanded[[y_col]]         = df.transf[[y_col]]
      df_expanded[[d_cols]]        = df.transf[[d_cols]]
      df_expanded[[cluster_cols]]  = df.transf[[cluster_cols]]


      if (approach == "cre"){
        df_expanded[[dbar_col]] = df.transf[[dbar_col]]
        protected_cols = c(y_col, d_cols, dbar_col, cluster_cols)
      } else{
        protected_cols = c(y_col, d_cols, cluster_cols)
      }

      protected_cols = intersect(protected_cols, colnames(df_expanded))

      df2 = df_expanded[, c(protected_cols, setdiff(colnames(df_expanded), protected_cols))]

      # Optional: Replace NAs with 0s (if needed)
      # df2[is.na(df2)] <- 0

      x_cols_plus = setdiff(names(df2), protected_cols)

    } else if (transformX=="minmax"){

      if (approach == "cre"){
        x_to_scale = c(x_cols_plus, dbar_col)
      } else{
        x_to_scale = x_cols_plus
      }

      main = df.transf[, x_to_scale]
      maxs = apply(main, 2, max)
      mins = apply(main, 2, min)
      scaled_x = as.data.frame(scale(main, center = mins, scale = maxs - mins))

      non_x_cols = setdiff(names(df.transf), x_to_scale)
      keep_cols = c(y_col, d_cols, cluster_cols)
      keep_cols = intersect(non_x_cols, keep_cols)
      unscaled_data = df.transf[, keep_cols, drop = FALSE]

      #df.transf[, x_cols] = scaled_main

      df2 = cbind(scaled_x, unscaled_data)

    }else if (transformX=="no"){
      df2 = df.transf
   }

       data = xtdml_data$new(df2,
                          x_cols = x_cols_plus,
                          y_col  = y_col,
                          d_cols = d_cols,
                          dbar_col = dbar_col,
                          cluster_cols = cluster_cols,
                          approach = approach,
                          transformX = transformX)

    return(data)
  }
}

