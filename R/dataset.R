#' @title Generates data from a partially linear panel regression (PLPR) model
#'
#' @description
#'  Generates data from a partially linear regression model for panel data with fixed effects
#'  similar to DGP3 (highly nonlinear) in Clarke and Polselli (2025).
#'
#'  The data generating process is defined as
#'
#' \eqn{Y_{it} = \theta D_{it} + g_0(X_{it}) + \alpha_i + U_{it},}
#'
#' \eqn{D_{it} = m_0(X_{it}) + \gamma_i + V_i,}
#'
#' where \eqn{U_{it} \sim \mathcal{N}(0,1)}, \eqn{V_{it}  \sim \mathcal{N}(0,1)},
#' \eqn{\alpha_i = \rho A_i + \sqrt{1-\rho^2} B_i} with
#' \eqn{A_i\sim \mathcal{N}(3,3)} \eqn{B_i\sim \mathcal{N}(0,1)}, and  \eqn{\gamma_i\sim \mathcal{N}(0,5)}.
#'
#' The covariates are distributed as \eqn{X_{it,p} \sim A_i + \mathcal{N}(0, 5)},
#' where \eqn{p} is the number of covariates.
#'
#' The nuisance functions are given by
#'
#' \eqn{m_0(X_{it}) = a_1 [X_{it,1} \times 1(X_{it,1}>0)] + a_2 [X_{it,1} \times X_{it,3}],}
#'
#' \eqn{g_0(X_{it}) = b_1 [X_{it,1} \times X_{it,3}] + b_2 [X_{it,3} \times 1(X_{it,3}>0)],}
#'
#' with \eqn{a_1=b_2=0.25} and \eqn{a_2=b_1=0.5}.
#'
#' @references Clarke, P. S. and Polselli,  A. (2025). Double Machine Learning
#' for Static Panel Models with Fixed Effects. Econometrics Journal.
#' DOI: 10.1093/ectj/utaf011.
#'
#' @param n_obs (`integer(1)`) \cr
#' The number of cross-sectional observations (i) to simulate.
#'
#' @param t_per (`integer(1)`) \cr
#' The number of time periods (t) to simulate.
#'
#' @param theta (`numeric(1)`) \cr
#' The value of the causal parameter.
#'
#' @param dim_x (`integer(1)`) \cr
#' The number of covariates.
#'
#' @param rho (`numeric(1)`) \cr
#' Parameter governing the relationship between the covariates and the unobserved
#' individual heterogeneity. The value is chosen between 0 (pure random effect)
#' and 1 (pure fixed effects).
#'
#' @return A data object.
#'
#' @examples
#' df = make_plpr_data(n_obs = 500, t_per = 10, dim_x = 20, theta = 0.5, rho=0.8)
#'
#' @export
make_plpr_data = function(n_obs = 500, t_per = 10,
                           dim_x = 20, theta = 0.5,
                           rho=0.8){
  assert_count(n_obs)
  assert_count(dim_x)
  assert_numeric(theta, len = 1)
  assert_number(rho, lower = 0, upper = 1)

  a_1 = 0.25
  a_2 = 0.5
  b_1 = 0.5
  b_2 = 0.25

  time = rep(1:t_per, times=n_obs, each=1)
  id   = rep(1:n_obs, times=1, each=t_per)
  nt = n_obs * t_per

  df = data.frame(id, time)

  # unobserved individual heterogeneities
  df = df %>%
    dplyr::group_by(id) %>%
    mutate(Ai = rnorm(1,3,3),
           Bi = rnorm(1,0,1),
           gammai = rnorm(1,0,5))

  alphai = rho*df$Ai + sqrt(1 - rho^2)*df$Bi

  # covariates
  X = replicate(dim_x, df$Ai + rnorm(nt, 1, 5))

  # nuisance functions
  m0   =  a_1 *(X[, 1]*as.numeric(X[, 1]>0)) + a_2 * (X[, 3]*X[, 1])
  g0   =  b_1 * (X[, 3]*X[, 1]) + b_2*(X[, 3]*as.numeric(X[, 3]>0))

  # treatment and output variables
  d = as.matrix(m0 + df$gammai + rnorm(nt))
  y = as.matrix(1 + theta*d + g0 + alphai + rnorm(nt))

  colnames(X) = paste0("X", 1:dim_x)
  colnames(y) = "y"
  colnames(d) = "d"

  data = data.frame(id,time,X, y, d)
  return(data)
}
