# XTDML
The `XTDML` package implements double machine learning (DML) for static partially linear panel regression (PLPR) models with fixed effects, as in [Clarke and Polselli (2025)](https://academic.oup.com/ectj/advance-article/doi/10.1093/ectj/utaf011/8120202?login=false). 
The `XTDML` package is built on the `DoubleML` package by Bach et al. (2022) using the `mlr3` ecosystem and the same notation.

## The Partially Linear Panel Regression Model
The `XTDML` package estimates the structural (causal) parameter from panel data models: 
```math
  Y_{it} = \theta_0 D_{it} + g_0(X_{it}) + \alpha_i + U_{it}
```
```math  
  D_{it} = m_0(X_{it}) + \gamma_i + V_i,
```
where 
  * $Y_{it}$ is the output, $D_{it}$ the treatment, $X_{it}$ the covariates
  * $\theta_0$ is the structural (causal) parameter to *estimate*; 
  * $(l_0, m_0)$ are (possibly nonlinear) nuisance functions to *learn* from the data using one of the tree proposed approaches ("fd-exact", "wg-approx", "cre");
  * ($\alpha_i, \gamma_i$) are the unobserved individual heterogeneity correlated with the included covariates;
  * ($U_{it}$, $V_{it}$) are disturbances.

> [!WARNING]
> Previous versions of the package allowed for models with endogenous treatment variables. This has been temporally removed from the current package, but we are currently working on including IV estimation and weak IV tests in the current version of the `XTDML` package.

> [!WARNING]
> The current version of the package allows the user to immediately use the estimation tools *without the need to* proceed with additional data managing and transformations.
> In particular, the user can choose:
>
>  1. The panel data approach to use among `approach = ("fd-exact", "wg-approx", "cre")`; default is `"fd-exact"`. `XTDML` proceeds with transforming the data based on the selected approach, following Clarke and Polselli (2025).
>
> 2. The type of transformation to apply to the covariates $X$ in the data set among `transformX = ("no", "minmax", "poly")`. `"no"` does not transform the covariates `X` and is recommended for tree-based learners. `"minmax"` applies the Min-Max normalization  $x' = (x-x_{min})/(x_{max}-x_{min})$ to the covariates and is recommended with neural networks. `"poly"` add polynomials up to order three and interactions between all possible combinations of two and three variables; this is recommended for Lasso. Default is `"no"`.

## Installing the package from GitHub
The current version can be installed via devtools:
```
library(devtools)
install_github("POLSEAN/XTDML")
```

## Loading the package and displaying the vignette (with sample codes)
```
library(XTDML)

# To display compiled examples
vignette("XTDML")
browseVignettes("XTDML") 
```

## References
Bach, P., Chernozhukov, V., Kurz, M. S., Spindler, M. and Klaassen, S. (2024), DoubleML - An Object-Oriented Implementation of Double Machine Learning in R, *Journal of Statistical Software*, 108(3): 1-56, doi:10.18637/jss.v108.i03, arXiv:2103.09603.

Chernozhukov, V., Chetverikov, D., Demirer, M., Duflo, E., Hansen, C., Newey, W., and Robins, J. (2018). Double/debiased machine learning for treatment and structural parameters. *The Econometrics Journal*, 21(1):C1–C68.

Clarke, P. S. and Polselli,  A. (2025). Double Machine Learning for Static Panel Models with Fixed Effects. Econometrics Journal. DOI: 10.1093/ectj/utaf011.



