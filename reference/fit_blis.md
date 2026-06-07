# Fit Baseline-category Logit Intercept-Slope (BLIS) model on nominal data

`blis` fits the IRT Nominal Response Model to data from multiple-choice
tests, while accounting for the correct answer and treating this option
as a baseline in this baseline-category logit model. The intercept-slope
parametrization in BLIS can be converted to IRT
(difficulty-discrimination) parametrization (BLIRT).

## Usage

``` r
fit_blis(Data, key, ...)

blis(Data, key, ...)
```

## Arguments

- Data:

  *data.frame* or *tibble* with all columns being factors. Support for
  *matrix* is limited and behavior not guaranteed.

- key:

  A single-column `data.frame`, (**not** matrix) `tibble` or -
  preferably - a factor vector of levels considered as correct
  responses.

- ...:

  Arguments passed on to
  [`mirt::mirt`](https://philchalmers.github.io/mirt/reference/mirt.html)

  `SE`

  :   logical; estimate the standard errors by computing the parameter
      information matrix? See `SE.type` for the type of estimates
      available

  `covdata`

  :   a data.frame of data used for latent regression models

  `formula`

  :   an R formula (or list of formulas) indicating how the latent
      traits can be regressed using external covariates in `covdata`. If
      a named list of formulas is supplied (where the names correspond
      to the latent trait names in `model`) then specific regression
      effects can be estimated for each factor. Supplying a single
      formula will estimate the regression parameters for all latent
      traits by default

  `itemdesign`

  :   a `data.frame` with rows equal to the number of items and columns
      containing any item-design effects. If items should be included in
      the design structure (i.e., should be left in their canonical
      structure) then fewer rows can be used, however the `rownames`
      must be defined and matched with `colnames` in the `data` input.
      The item design matrix is constructed with the use of
      `item.formula`. Providing this input will fix the associated `'d'`
      intercepts to 0, where applicable

  `item.formula`

  :   an R formula used to specify any intercept decomposition (e.g.,
      the LLTM; Fischer, 1983). Note that only the right-hand side of
      the formula is required for compensatory models.

      For non-compensatory `itemtype`s (e.g., `'PC1PL'`) the formula
      must include the name of the latent trait in the left hand side of
      the expression to indicate which of the trait specification should
      have their intercepts decomposed (see MLTM; Embretson, 1984)

  `SE.type`

  :   type of estimation method to use for calculating the parameter
      information matrix for computing standard errors and
      [`wald`](https://philchalmers.github.io/mirt/reference/wald.html)
      tests. Can be:

      - `'Richardson'`, `'forward'`, or `'central'` for the numerical
        Richardson, forward difference, and central difference
        evaluation of observed Hessian matrix

      - `'crossprod'` and `'Louis'` for standard error computations
        based on the variance of the Fisher scores as well as
        Louis' (1982) exact computation of the observed information
        matrix. Note that Louis' estimates can take a long time to
        obtain for large sample sizes and long tests

      - `'sandwich'` for the sandwich covariance estimate based on the
        `'crossprod'` and `'Oakes'` estimates (see Chalmers, 2018, for
        details)

      - `'sandwich.Louis'` for the sandwich covariance estimate based on
        the `'crossprod'` and `'Louis'` estimates

      - `'Oakes'` for Oakes' (1999) method using a central difference
        approximation (see Chalmers, 2018, for details)

      - `'SEM'` for the supplemented EM (disables the `accelerate`
        option automatically; EM only)

      - `'Fisher'` for the expected information, `'complete'` for
        information based on the complete-data Hessian used in EM
        algorithm

      - `'MHRM'` and `'FMHRM'` for stochastic approximations of observed
        information matrix based on the Robbins-Monro filter or a fixed
        number of MHRM draws without the RM filter. These are the only
        options supported when `method = 'MHRM'`

      - `'numerical'` to obtain the numerical estimate from a call to
        [`optim`](https://rdrr.io/r/stats/optim.html) when
        `method = 'BL'`

      Note that both the `'SEM'` method becomes very sensitive if the ML
      solution has has not been reached with sufficient precision, and
      may be further sensitive if the history of the EM cycles is not
      stable/sufficient for convergence of the respective estimates.
      Increasing the number of iterations (increasing `NCYCLES` and
      decreasing `TOL`, see below) will help to improve the accuracy,
      and can be run in parallel if a
      [`mirtCluster`](https://philchalmers.github.io/mirt/reference/mirtCluster.html)
      object has been defined (this will be used for Oakes' method as
      well). Additionally, inspecting the symmetry of the ACOV matrix
      for convergence issues by passing
      `technical = list(symmetric = FALSE)` can be helpful to determine
      if a sufficient solution has been reached

  `method`

  :   a character object specifying the estimation algorithm to be used.
      The default is `'EM'`, for the standard EM algorithm with fixed
      quadrature, `'QMCEM'` for quasi-Monte Carlo EM estimation, or
      `'MCEM'` for Monte Carlo EM estimation. The option `'MHRM'` may
      also be passed to use the MH-RM algorithm, `'SEM'` for the
      Stochastic EM algorithm (first two stages of the MH-RM stage using
      an optimizer other than a single Newton-Raphson iteration), and
      `'BL'` for the Bock and Lieberman approach (generally not
      recommended for longer tests).

      The `'EM'` is generally effective with 1-3 factors, but methods
      such as the `'QMCEM'`, `'MCEM'`, `'SEM'`, or `'MHRM'` should be
      used when the dimensions are 3 or more. Note that when the
      optimizer is stochastic the associated `SE.type` is automatically
      changed to `SE.type = 'MHRM'` by default to avoid the use of
      quadrature

  `optimizer`

  :   a character indicating which numerical optimizer to use. By
      default, the EM algorithm will use the `'BFGS'` when there are no
      upper and lower bounds box-constraints and `'nlminb'` when there
      are.

      Other options include the Newton-Raphson (`'NR'`), which can be
      more efficient than the `'BFGS'` but not as stable for more
      complex IRT models (such as the nominal or nested logit models)
      and the related `'NR1'` which is also the Newton-Raphson but
      consists of only 1 update that has been coupled with RM Hessian
      (only applicable when the MH-RM algorithm is used). The MH-RM
      algorithm uses the `'NR1'` by default, though currently the
      `'BFGS'`, `'L-BFGS-B'`, and `'NR'` are also supported with this
      method (with fewer iterations by default) to emulate stochastic EM
      updates. As well, the `'Nelder-Mead'` and `'SANN'` estimators are
      available, but their routine use generally is not required or
      recommended.

      Additionally, estimation subroutines from the `Rsolnp` and
      `nloptr` packages are available by passing the arguments `'solnp'`
      and `'nloptr'`, respectively. This should be used in conjunction
      with the `solnp_args` and `nloptr_args` specified below. If
      equality constraints were specified in the model definition only
      the parameter with the lowest `parnum` in the `pars = 'values'`
      data.frame is used in the estimation vector passed to the
      objective function, and group hyper-parameters are omitted.
      Equality an inequality functions should be of the form
      `function(p, optim_args)`, where `optim_args` is a list of
      internally parameters that largely can be ignored when defining
      constraints (though use of
      [`browser()`](https://rdrr.io/r/base/browser.html) here may be
      helpful)

  `dentype`

  :   type of density form to use for the latent trait parameters.
      Current options include

      - `'Gaussian'` (default) assumes a multivariate Gaussian
        distribution with an associated mean vector and
        variance-covariance matrix

      - `'empiricalhist'` or `'EH'` estimates latent distribution using
        an empirical histogram described by Bock and Aitkin (1981). Only
        applicable for unidimensional models estimated with the EM
        algorithm. For this option, the number of cycles, TOL, and
        quadpts are adjusted accommodate for less precision during
        estimation (namely: `TOL = 3e-5`, `NCYCLES = 2000`,
        `quadpts = 121`)

      - `'empiricalhist_Woods'` or `'EHW'` estimates latent distribution
        using an empirical histogram described by Bock and Aitkin
        (1981), with the same specifications as in
        `dentype = 'empiricalhist'`, but with the
        extrapolation-interpolation method described by Woods (2007).
        NOTE: to improve stability in the presence of extreme response
        styles (i.e., all highest or lowest in each item) the
        `technical` option `zeroExtreme = TRUE` may be required to
        down-weight the contribution of these problematic patterns

      - `'Davidian-#'` estimates semi-parametric Davidian curves
        described by Woods and Lin (2009), where the `#` placeholder
        represents the number of Davidian parameters to estimate (e.g.,
        `'Davidian-6'` will estimate 6 smoothing parameters). By
        default, the number of `quadpts` is increased to 121, and this
        method is only applicable for unidimensional models estimated
        with the EM algorithm

      Note that when `itemtype = 'ULL'` then a log-normal(0,1) density
      is used to support the unipolar scaling

  `constrain`

  :   a list of user declared equality constraints. To see how to define
      the parameters correctly use `pars = 'values'` initially to see
      how the parameters are labeled. To constrain parameters to be
      equal create a list with separate concatenated vectors signifying
      which parameters to constrain. For example, to set parameters 1
      and 5 equal, and also set parameters 2, 6, and 10 equal use
      `constrain = list(c(1,5), c(2,6,10))`. Constraints can also be
      specified using the
      [`mirt.model`](https://philchalmers.github.io/mirt/reference/mirt.model.html)
      syntax (recommended)

  `calcNull`

  :   logical; calculate the Null model for additional fit statistics
      (e.g., TLI)? Only applicable if the data contains no NA's and the
      data is not overly sparse

  `draws`

  :   the number of Monte Carlo draws to estimate the log-likelihood for
      the MH-RM algorithm. Default is 5000

  `survey.weights`

  :   a optional numeric vector of survey weights to apply for each case
      in the data (EM estimation only). If not specified, all cases are
      weighted equally (the standard IRT approach). The sum of the
      `survey.weights` must equal the total sample size for proper
      weighting to be applied

  `quadpts`

  :   number of quadrature points per dimension (must be larger than 2).
      By default the number of quadrature uses the following scheme:
      `switch(as.character(nfact), '1'=61, '2'=31, '3'=15, '4'=9, '5'=7, 3)`.
      However, if the method input is set to `'QMCEM'` and this argument
      is left blank then the default number of quasi-Monte Carlo
      integration nodes will be set to 5000 in total

  `TOL`

  :   convergence threshold for EM or MH-RM; defaults are .0001 and
      .001. If `SE.type = 'SEM'` and this value is not specified, the
      default is set to `1e-5`. To evaluate the model using only the
      starting values pass `TOL = NaN`, and to evaluate the starting
      values without the log-likelihood pass `TOL = NA`

  `gpcm_mats`

  :   a list of matrices specifying how the scoring coefficients in the
      (generalized) partial credit model should be constructed. If
      omitted, the standard gpcm format will be used (i.e.,
      `seq(0, k, by = 1)` for each trait). This input should be used if
      traits should be scored different for each category (e.g.,
      `matrix(c(0:3, 1,0,0,0), 4, 2)` for a two-dimensional model where
      the first trait is scored like a gpcm, but the second trait is
      only positively indicated when the first category is selected).
      Can be used when `itemtype`s are `'gpcm'` or `'Rasch'`, but only
      when the respective element in `gpcm_mats` is not `NULL`

  `grsm.block`

  :   an optional numeric vector indicating where the blocking should
      occur when using the grsm, NA represents items that do not belong
      to the grsm block (other items that may be estimated in the test
      data). For example, to specify two blocks of 3 with a 2PL item for
      the last item: `grsm.block = c(rep(1,3), rep(2,3), NA)`. If NULL
      the all items are assumed to be within the same group and
      therefore have the same number of item categories

  `rsm.block`

  :   same as `grsm.block`, but for `'rsm'` blocks

  `monopoly.k`

  :   a vector of values (or a single value to repeated for each item)
      which indicate the degree of the monotone polynomial fitted, where
      the monotone polynomial corresponds to `monopoly.k * 2 + 1` (e.g.,
      `monopoly.k = 2` fits a 5th degree polynomial). Default is
      `monopoly.k = 1`, which fits a 3rd degree polynomial

  `large`

  :   a `logical` indicating whether unique response patterns should be
      obtained prior to performing the estimation so as to avoid
      repeating computations on identical patterns. The default `TRUE`
      provides the correct degrees of freedom for the model since all
      unique patterns are tallied (typically only affects goodness of
      fit statistics such as G2, but also will influence nested model
      comparison methods such as `anova(mod1, mod2)`), while `FALSE`
      will use the number of rows in `data` as a placeholder for the
      total degrees of freedom. As such, model objects should only be
      compared if all flags were set to `TRUE` or all were set to
      `FALSE`

      Alternatively, if the collapse table of frequencies is desired for
      the purpose of saving computations (i.e., only computing the
      collapsed frequencies for the data onte-time) then a character
      vector can be passed with the arguement `large = 'return'` to
      return a list of all the desired table information used by `mirt`.
      This list object can then be reused by passing it back into the
      `large` argument to avoid re-tallying the data again (again,
      useful when the dataset are very large and computing the tabulated
      data is computationally burdensome). This strategy is shown below:

      Compute organized data

      :   e.g., `internaldat <- mirt(Science, 1, large = 'return')`

      Pass the organized data to all estimation functions

      :   e.g., `mod <- mirt(Science, 1, large = internaldat)`

  `GenRandomPars`

  :   logical; generate random starting values prior to optimization
      instead of using the fixed internal starting values?

  `accelerate`

  :   a character vector indicating the type of acceleration to use.
      Default is `'Ramsay'`, but may also be `'squarem'` for the SQUAREM
      procedure (specifically, the gSqS3 approach) described in Varadhan
      and Roldand (2008). To disable the acceleration, pass `'none'`

  `verbose`

  :   logical; print observed- (EM) or complete-data (MHRM)
      log-likelihood after each iteration cycle? Default is TRUE

  `solnp_args`

  :   a list of arguments to be passed to the `solnp::solnp()` function
      for equality constraints, inequality constraints, etc

  `nloptr_args`

  :   a list of arguments to be passed to the
      [`nloptr::nloptr()`](https://astamm.github.io/nloptr/reference/nloptr.html)
      function for equality constraints, inequality constraints, etc

  `spline_args`

  :   a named list of lists containing information to be passed to the
      `bs` (default) `ns`, and
      [`iSpline`](https://wwenjie.org/splines2/reference/iSpline.html)
      for each spline/monospline itemtype. Each element must refer to
      the name of the itemtype with the spline, while the internal list
      names refer to the arguments which are passed. For example, if
      item 2 were called 'read2', and item 5 were called 'read5', both
      of which were of itemtype 'spline' but item 5 should use the `ns`
      form, then a modified list for each input might be of the form:

      `spline_args = list(read2 = list(degree = 4), read5 = list(fun = 'ns', knots = c(-2, 2)))`

      This code input changes the `bs()` splines function to have a
      `degree = 4` input, while the second element changes to the `ns()`
      function with knots set a `c(-2, 2)`

  `control`

  :   a list passed to the respective optimizers (i.e.,
      [`optim()`](https://rdrr.io/r/stats/optim.html),
      [`nlminb()`](https://rdrr.io/r/stats/nlminb.html), etc).
      Additional arguments have been included for the `'NR'` optimizer:
      `'tol'` for the convergence tolerance in the M-step (default is
      `TOL/1000`), while the default number of iterations for the
      Newton-Raphson optimizer is 50 (modified with the `'maxit'`
      control input)

  `technical`

  :   a list containing lower level technical parameters for estimation.
      May be:

      NCYCLES

      :   maximum number of EM or MH-RM cycles; defaults are 500 and
          2000

      MAXQUAD

      :   maximum number of quadrature, which you can increase if you
          have more than 4GB or RAM on your PC; default 20000

      theta_lim

      :   range of integration grid for each dimension; default is
          `c(-6, 6)`. Note that when `itemtype = 'ULL'` a log-normal
          distribution is used and the range is change to
          `c(.01, and 6^2)`, where the second term is the square of the
          `theta_lim` input instead

      set.seed

      :   seed number used during estimation. Default is 12345

      SEtol

      :   standard error tolerance criteria for the S-EM and MHRM
          computation of the information matrix. Default is 1e-3

      symmetric

      :   logical; force S-EM/Oakes information matrix estimates to be
          symmetric? Default is TRUE so that computation of standard
          errors are more stable. Setting this to FALSE can help to
          detect solutions that have not reached the ML estimate

      SEM_window

      :   ratio of values used to define the S-EM window based on the
          observed likelihood differences across EM iterations. The
          default is `c(0, 1 - SEtol)`, which provides nearly the very
          full S-EM window (i.e., nearly all EM cycles used). To use the
          a smaller SEM window change the window to to something like
          `c(.9, .999)` to start at a point farther into the EM history

      warn

      :   logical; include warning messages during estimation? Default
          is TRUE

      message

      :   logical; include general messages during estimation? Default
          is TRUE

      customK

      :   a numeric vector used to explicitly declare the number of
          response categories for each item. This should only be used
          when constructing mirt model for reasons other than parameter
          estimation (such as to obtain factor scores), and requires
          that the input data all have 0 as the lowest category. The
          format is the same as the `extract.mirt(mod, 'K')` slot in all
          converged models

      customPriorFun

      :   a custom function used to determine the normalized density for
          integration in the EM algorithm. Must be of the form
          `function(Theta, Etable){...}`, and return a numeric vector
          with the same length as number of rows in `Theta`. The
          `Etable` input contains the aggregated table generated from
          the current E-step computations. For proper integration, the
          returned vector should sum to 1 (i.e., normalized). Note that
          if using the `Etable` it will be NULL on the first call,
          therefore the prior will have to deal with this issue
          accordingly

      zeroExtreme

      :   logical; assign extreme response patterns a `survey.weight` of
          0 (formally equivalent to removing these data vectors during
          estimation)? When `dentype = 'EHW'`, where Woods'
          extrapolation is utilized, this option may be required if the
          extrapolation causes expected densities to tend towards
          positive or negative infinity. The default is `FALSE`

      customTheta

      :   a custom `Theta` grid, in matrix form, used for integration.
          If not defined, the grid is determined internally based on the
          number of `quadpts`

      fixedTheta

      :   a `matrix` of latent trait values taken to be fixed and known.
          This will perform a single M-step optimization to obtain item
          parameter estimates, holding constant the elements in
          `fixedTheta`, using the `'MHRM'` engine with the BFGS/L-BFGS-B
          algorithm. Matrix input must have as many rows as there are
          rows in `data`

      nconstrain

      :   same specification as the `constrain` list argument, however
          imposes a negative equality constraint instead (e.g., \\a12 =
          -a21\\, which is specified as `nconstrain = list(c(12, 21))`).
          Note that each specification in the list must be of length 2,
          where the second element is taken to be -1 times the first
          element

      delta

      :   the deviation term used in numerical estimates when computing
          the ACOV matrix with the 'forward' or 'central' numerical
          approaches, as well as Oakes' method with the Richardson
          extrapolation. Default is 1e-5

      parallel

      :   logical; use the parallel cluster defined by
          [`mirtCluster`](https://philchalmers.github.io/mirt/reference/mirtCluster.html)?
          Default is TRUE

      storeEMhistory

      :   logical; store the iteration history when using the EM
          algorithm? Default is FALSE. When TRUE, use
          [`extract.mirt`](https://philchalmers.github.io/mirt/reference/extract.mirt.html)
          to extract

      internal_constraints

      :   logical; include the internal constraints when using certain
          IRT models (e.g., 'grsm' itemtype). Disable this if you want
          to use special optimizers such as the solnp. Default is `TRUE`

      gain

      :   a vector of two values specifying the numerator and exponent
          values for the RM gain function \\(val1 / cycle)^val2\\.
          Default is `c(0.10, 0.75)`

      BURNIN

      :   number of burn in cycles (stage 1) in MH-RM; default is 150

      SEMCYCLES

      :   number of SEM cycles (stage 2) in MH-RM; default is 100

      MHDRAWS

      :   number of Metropolis-Hasting draws to use in the MH-RM at each
          iteration; default is 5

      MHcand

      :   a vector of values used to tune the MH sampler. Larger values
          will cause the acceptance ratio to decrease. One value is
          required for each group in unconditional item factor analysis
          (`mixedmirt()` requires additional values for random effect).
          If null, these values are determined internally, attempting to
          tune the acceptance of the draws to be between .1 and .4

      MHRM_SE_draws

      :   number of fixed draws to use when `SE=TRUE` and
          `SE.type = 'FMHRM'` and the maximum number of draws when
          `SE.type = 'MHRM'`. Default is 2000

      MCEM_draws

      :   a function used to determine the number of quadrature points
          to draw for the `'MCEM'` method. Must include one argument
          which indicates the iteration number of the EM cycle. Default
          is `function(cycles) 500 + (cycles - 1)*2`, which starts the
          number of draws at 500 and increases by 2 after each full EM
          iteration

      info_if_converged

      :   logical; compute the information matrix when using the MH-RM
          algorithm only if the model converged within a suitable number
          of iterations? Default is `TRUE`

      logLik_if_converged

      :   logical; compute the observed log-likelihood when using the
          MH-RM algorithm only if the model converged within a suitable
          number of iterations? Default is `TRUE`

      keep_vcov_PD

      :   logical; attempt to keep the variance-covariance matrix of the
          latent traits positive definite during estimation in the EM
          algorithm? This generally improves the convergence properties
          when the traits are highly correlated. Default is `TRUE`

## Value

Fitted model of class [BlisClass](BlisClass-class.md) (extending
standard `mirt`'s `SingleGroupClass`).

## Details

For the details on `coef` method dispatched for fitted BLIS model, see
[coef,BlisClass-method](coef-BlisClass-method.md). To get more on the
class, see [BlisClass](BlisClass-class.md).

## See also

Other BLIS/BLIRT related: [`BlisClass-class`](BlisClass-class.md),
[`coef,BlisClass-method`](coef-BlisClass-method.md),
[`get_orig_levels()`](get_orig_levels.md),
[`nominal_to_int()`](nominal_to_int.md),
[`obtain_nrm_def()`](obtain_nrm_def.md),
[`print.blis_coefs()`](print.blis_coefs.md)

## Author

Jan Netik  
Institute of Computer Science of the Czech Academy of Sciences  
<netik@cs.cas.cz>

Patricia Martinkova  
Institute of Computer Science of the Czech Academy of Sciences  
<martinkova@cs.cas.cz>

## Examples

``` r
fitted_blis <- fit_blis(HCItest[, 1:20], HCIkey, SE = TRUE)
coef(fitted_blis)
#> $`Item 1`
#>            ak0    ak1    ak2 ak3     d0     d1     d2 d3
#> par     -1.374 -0.407 -0.997   0 -3.315 -2.029 -1.632  0
#> CI_2.5  -1.973 -0.755 -1.313  NA -3.941 -2.306 -1.904 NA
#> CI_97.5 -0.775 -0.060 -0.682  NA -2.688 -1.751 -1.360 NA
#> 
#> $`Item 2`
#>            ak0 ak1    ak2     d0 d1     d2
#> par     -0.984   0 -0.445 -1.897  0 -2.039
#> CI_2.5  -1.309  NA -0.773 -2.192 NA -2.309
#> CI_97.5 -0.659  NA -0.116 -1.602 NA -1.768
#> 
#> $`Item 3`
#>         ak0    ak1    ak2 d0     d1     d2
#> par       0 -2.090 -1.363  0 -3.716 -2.805
#> CI_2.5   NA -2.767 -1.850 NA -4.483 -3.264
#> CI_97.5  NA -1.413 -0.876 NA -2.948 -2.347
#> 
#> $`Item 4`
#>            ak0    ak1    ak2 ak3     d0     d1    d2 d3
#> par     -2.963 -2.049 -0.252   0 -5.397 -3.774 0.320  0
#> CI_2.5  -4.424 -2.947 -0.453  NA -7.547 -4.876 0.157 NA
#> CI_97.5 -1.502 -1.151 -0.052  NA -3.246 -2.671 0.483 NA
#> 
#> $`Item 5`
#>            ak0 ak1    ak2    ak3     d0 d1     d2     d3
#> par     -0.805   0 -0.852 -0.669 -1.440  0 -1.091 -0.336
#> CI_2.5  -1.157  NA -1.166 -0.913 -1.733 NA -1.352 -0.534
#> CI_97.5 -0.453  NA -0.538 -0.425 -1.146 NA -0.831 -0.139
#> 
#> $`Item 6`
#>            ak0    ak1 ak2     d0    d1 d2
#> par     -1.592 -0.908   0 -1.647 0.549  0
#> CI_2.5  -2.057 -1.163  NA -2.073 0.351 NA
#> CI_97.5 -1.127 -0.652  NA -1.222 0.747 NA
#> 
#> $`Item 7`
#>            ak0    ak1 ak2     d0     d1 d2
#> par     -0.537 -0.190   0 -1.673 -0.463  0
#> CI_2.5  -0.852 -0.389  NA -1.951 -0.631 NA
#> CI_97.5 -0.223  0.008  NA -1.394 -0.294 NA
#> 
#> $`Item 8`
#>            ak0    ak1 ak2     d0     d1 d2
#> par     -1.036 -1.180   0 -1.611 -1.994  0
#> CI_2.5  -1.358 -1.553  NA -1.879 -2.323 NA
#> CI_97.5 -0.715 -0.806  NA -1.342 -1.666 NA
#> 
#> $`Item 9`
#>            ak0    ak1    ak2 ak3     d0     d1     d2 d3
#> par     -0.334 -1.171 -2.877   0  0.115 -2.026 -5.367  0
#> CI_2.5  -0.541 -1.627 -4.284  NA -0.053 -2.448 -7.455 NA
#> CI_97.5 -0.127 -0.714 -1.470  NA  0.282 -1.603 -3.279 NA
#> 
#> $`Item 10`
#>         ak0    ak1    ak2 d0     d1     d2
#> par       0 -0.742 -0.798  0 -1.381 -1.397
#> CI_2.5   NA -1.023 -1.084 NA -1.617 -1.638
#> CI_97.5  NA -0.461 -0.512 NA -1.145 -1.156
#> 
#> $`Item 11`
#>         ak0    ak1    ak2 d0     d1     d2
#> par       0 -1.480 -0.735  0 -2.889 -1.822
#> CI_2.5   NA -1.966 -1.046 NA -3.394 -2.081
#> CI_97.5  NA -0.995 -0.424 NA -2.384 -1.562
#> 
#> $`Item 12`
#>            ak0    ak1    ak2 ak3    ak4     d0     d1     d2 d3     d4
#> par     -0.945 -1.078 -1.252   0 -0.731 -1.830 -3.606 -2.838  0 -0.790
#> CI_2.5  -1.315 -1.880 -1.808  NA -0.984 -2.145 -4.346 -3.373 NA -0.991
#> CI_97.5 -0.576 -0.277 -0.696  NA -0.478 -1.515 -2.867 -2.303 NA -0.589
#> 
#> $`Item 13`
#>         ak0    ak1    ak2    ak3 d0     d1     d2     d3
#> par       0 -1.517 -1.230 -0.972  0 -2.442 -1.666 -1.270
#> CI_2.5   NA -2.011 -1.604 -1.287 NA -2.900 -1.973 -1.515
#> CI_97.5  NA -1.023 -0.856 -0.656 NA -1.984 -1.359 -1.025
#> 
#> $`Item 14`
#>         ak0    ak1    ak2    ak3 d0     d1     d2     d3
#> par       0 -1.121 -1.004 -1.586  0 -2.942 -2.066 -2.993
#> CI_2.5   NA -1.643 -1.371 -2.112 NA -3.421 -2.378 -3.534
#> CI_97.5  NA -0.599 -0.638 -1.061 NA -2.464 -1.754 -2.452
#> 
#> $`Item 15`
#>            ak0    ak1 ak2    ak3     d0     d1 d2     d3
#> par     -0.978 -1.219   0 -0.487 -1.299 -0.756  0 -0.700
#> CI_2.5  -1.339 -1.536  NA -0.762 -1.588 -1.011 NA -0.918
#> CI_97.5 -0.617 -0.903  NA -0.212 -1.010 -0.501 NA -0.482
#> 
#> $`Item 16`
#>         ak0    ak1    ak2    ak3 d0     d1     d2     d3
#> par       0 -1.098 -1.583 -0.756  0 -1.003 -2.385 -2.287
#> CI_2.5   NA -1.397 -2.065 -1.218 NA -1.234 -2.839 -2.641
#> CI_97.5  NA -0.798 -1.101 -0.293 NA -0.771 -1.932 -1.933
#> 
#> $`Item 17`
#>            ak0    ak1 ak2    ak3     d0     d1 d2     d3
#> par      0.077 -0.294   0 -0.124  0.037 -0.126  0 -0.855
#> CI_2.5  -0.152 -0.539  NA -0.425 -0.162 -0.336 NA -1.114
#> CI_97.5  0.307 -0.049  NA  0.177  0.236  0.084 NA -0.596
#> 
#> $`Item 18`
#>            ak0    ak1 ak2    ak3     d0     d1 d2     d3
#> par     -1.810 -1.866   0 -1.953 -3.235 -2.851  0 -3.892
#> CI_2.5  -2.434 -2.418  NA -2.739 -3.849 -3.379 NA -4.730
#> CI_97.5 -1.187 -1.314  NA -1.167 -2.620 -2.323 NA -3.055
#> 
#> $`Item 19`
#>            ak0    ak1 ak2    ak3    ak4     d0     d1 d2     d3     d4
#> par     -1.352 -1.686   0 -1.117 -1.597 -2.932 -3.152  0 -3.081 -3.634
#> CI_2.5  -1.879 -2.256  NA -1.680 -2.283 -3.428 -3.736 NA -3.582 -4.349
#> CI_97.5 -0.825 -1.116  NA -0.554 -0.912 -2.437 -2.568 NA -2.581 -2.919
#> 
#> $`Item 20`
#>            ak0    ak1    ak2 ak3     d0     d1     d2 d3
#> par     -1.627 -1.727 -0.766   0 -4.550 -3.370 -1.331  0
#> CI_2.5  -2.634 -2.334 -1.043  NA -5.703 -4.042 -1.552 NA
#> CI_97.5 -0.621 -1.120 -0.488  NA -3.397 -2.698 -1.109 NA
#> 
coef(fitted_blis)$`Item 12`
#>                ak0        ak1        ak2 ak3        ak4        d0        d1
#> par     -0.9454655 -1.0783520 -1.2521823   0 -0.7309039 -1.829986 -3.606447
#> CI_2.5  -1.3150061 -1.8796312 -1.8082383  NA -0.9841580 -2.144674 -4.346394
#> CI_97.5 -0.5759249 -0.2770728 -0.6961263  NA -0.4776498 -1.515297 -2.866500
#>                d2 d3         d4
#> par     -2.838070  0 -0.7901459
#> CI_2.5  -3.373078 NA -0.9911868
#> CI_97.5 -2.303062 NA -0.5891050
coef(fitted_blis, IRTpars = TRUE)
#> $`Item 1`
#>             a1     a2     a3 a4     b1     b2     b3 b4
#> par     -1.374 -0.407 -0.997  0 -2.413 -4.982 -1.637  0
#> CI_2.5  -1.973 -0.755 -1.313 NA -3.194 -9.204 -2.088 NA
#> CI_97.5 -0.775 -0.060 -0.682 NA -1.631 -0.761 -1.185 NA
#> 
#> $`Item 2`
#>             a1 a2     a3     b1 b2     b3
#> par     -0.984  0 -0.445 -1.928  0 -4.584
#> CI_2.5  -1.309 NA -0.773 -2.460 NA -7.883
#> CI_97.5 -0.659 NA -0.116 -1.396 NA -1.286
#> 
#> $`Item 3`
#>         a1     a2     a3 b1     b2     b3
#> par      0 -2.090 -1.363  0 -1.778 -2.058
#> CI_2.5  NA -2.767 -1.850 NA -2.103 -2.605
#> CI_97.5 NA -1.413 -0.876 NA -1.453 -1.510
#> 
#> $`Item 4`
#>             a1     a2     a3 a4     b1     b2    b3 b4
#> par     -2.963 -2.049 -0.252  0 -1.821 -1.842 1.268  0
#> CI_2.5  -4.424 -2.947 -0.453 NA -2.145 -2.275 0.143 NA
#> CI_97.5 -1.502 -1.151 -0.052 NA -1.497 -1.409 2.392 NA
#> 
#> $`Item 5`
#>             a1 a2     a3     a4     b1 b2     b3     b4
#> par     -0.805  0 -0.852 -0.669 -1.788  0 -1.281 -0.503
#> CI_2.5  -1.157 NA -1.166 -0.913 -2.568 NA -1.784 -0.852
#> CI_97.5 -0.453 NA -0.538 -0.425 -1.008 NA -0.778 -0.153
#> 
#> $`Item 6`
#>             a1     a2 a3     b1    b2 b3
#> par     -1.592 -0.908  0 -1.035 0.605  0
#> CI_2.5  -2.057 -1.163 NA -1.327 0.363 NA
#> CI_97.5 -1.127 -0.652 NA -0.743 0.846 NA
#> 
#> $`Item 7`
#>             a1     a2 a3     b1     b2 b3
#> par     -0.537 -0.190  0 -3.114 -2.431  0
#> CI_2.5  -0.852 -0.389 NA -4.833 -5.117 NA
#> CI_97.5 -0.223  0.008 NA -1.395  0.256 NA
#> 
#> $`Item 8`
#>             a1     a2 a3     b1     b2 b3
#> par     -1.036 -1.180  0 -1.554 -1.691  0
#> CI_2.5  -1.358 -1.553 NA -1.984 -2.134 NA
#> CI_97.5 -0.715 -0.806 NA -1.125 -1.247 NA
#> 
#> $`Item 9`
#>             a1     a2     a3 a4     b1     b2     b3 b4
#> par     -0.334 -1.171 -2.877  0  0.344 -1.731 -1.866  0
#> CI_2.5  -0.541 -1.627 -4.284 NA -0.176 -2.275 -2.208 NA
#> CI_97.5 -0.127 -0.714 -1.470 NA  0.863 -1.186 -1.523 NA
#> 
#> $`Item 10`
#>         a1     a2     a3 b1     b2     b3
#> par      0 -0.742 -0.798  0 -1.862 -1.751
#> CI_2.5  NA -1.023 -1.084 NA -2.536 -2.343
#> CI_97.5 NA -0.461 -0.512 NA -1.187 -1.159
#> 
#> $`Item 11`
#>         a1     a2     a3 b1     b2     b3
#> par      0 -1.480 -0.735  0 -1.952 -2.479
#> CI_2.5  NA -1.966 -1.046 NA -2.398 -3.441
#> CI_97.5 NA -0.995 -0.424 NA -1.506 -1.516
#> 
#> $`Item 12`
#>             a1     a2     a3 a4     a5     b1     b2     b3 b4     b5
#> par     -0.945 -1.078 -1.252  0 -0.731 -1.936 -3.344 -2.266  0 -1.081
#> CI_2.5  -1.315 -1.880 -1.808 NA -0.984 -2.625 -5.490 -3.072 NA -1.511
#> CI_97.5 -0.576 -0.277 -0.696 NA -0.478 -1.246 -1.199 -1.461 NA -0.652
#> 
#> $`Item 13`
#>         a1     a2     a3     a4 b1     b2     b3     b4
#> par      0 -1.517 -1.230 -0.972  0 -1.609 -1.355 -1.307
#> CI_2.5  NA -2.011 -1.604 -1.287 NA -2.013 -1.724 -1.734
#> CI_97.5 NA -1.023 -0.856 -0.656 NA -1.206 -0.985 -0.880
#> 
#> $`Item 14`
#>         a1     a2     a3     a4 b1     b2     b3     b4
#> par      0 -1.121 -1.004 -1.586  0 -2.625 -2.057 -1.886
#> CI_2.5  NA -1.643 -1.371 -2.112 NA -3.635 -2.704 -2.318
#> CI_97.5 NA -0.599 -0.638 -1.061 NA -1.614 -1.411 -1.455
#> 
#> $`Item 15`
#>             a1     a2 a3     a4     b1     b2 b3     b4
#> par     -0.978 -1.219  0 -0.487 -1.329 -0.620  0 -1.436
#> CI_2.5  -1.339 -1.536 NA -0.762 -1.832 -0.845 NA -2.422
#> CI_97.5 -0.617 -0.903 NA -0.212 -0.825 -0.395 NA -0.451
#> 
#> $`Item 16`
#>         a1     a2     a3     a4 b1     b2     b3     b4
#> par      0 -1.098 -1.583 -0.756  0 -0.913 -1.507 -3.026
#> CI_2.5  NA -1.397 -2.065 -1.218 NA -1.185 -1.855 -4.813
#> CI_97.5 NA -0.798 -1.101 -0.293 NA -0.642 -1.159 -1.239
#> 
#> $`Item 17`
#>             a1     a2 a3     a4     b1     b2 b3      b4
#> par      0.077 -0.294  0 -0.124 -0.484 -0.428  0  -6.894
#> CI_2.5  -0.152 -0.539 NA -0.425 -3.586 -1.193 NA -23.702
#> CI_97.5  0.307 -0.049 NA  0.177  2.618  0.336 NA   9.913
#> 
#> $`Item 18`
#>             a1     a2 a3     a4     b1     b2 b3     b4
#> par     -1.810 -1.866  0 -1.953 -1.787 -1.528  0 -1.993
#> CI_2.5  -2.434 -2.418 NA -2.739 -2.196 -1.825 NA -2.502
#> CI_97.5 -1.187 -1.314 NA -1.167 -1.377 -1.231 NA -1.484
#> 
#> $`Item 19`
#>             a1     a2 a3     a4     a5     b1     b2 b3     b4     b5
#> par     -1.352 -1.686  0 -1.117 -1.597 -2.170 -1.869  0 -2.758 -2.275
#> CI_2.5  -1.879 -2.256 NA -1.680 -2.283 -2.817 -2.295 NA -3.921 -2.963
#> CI_97.5 -0.825 -1.116 NA -0.554 -0.912 -1.522 -1.444 NA -1.594 -1.587
#> 
#> $`Item 20`
#>             a1     a2     a3 a4     b1     b2     b3 b4
#> par     -1.627 -1.727 -0.766  0 -2.796 -1.951 -1.738  0
#> CI_2.5  -2.634 -2.334 -1.043 NA -4.015 -2.394 -2.336 NA
#> CI_97.5 -0.621 -1.120 -0.488 NA -1.576 -1.509 -1.139 NA
#> 
coef(fitted_blis, IRTpars = TRUE, CI = 0.90) # 90% CI instead of 95% CI
#> $`Item 1`
#>           a1     a2     a3 a4     b1     b2     b3 b4
#> par   -1.374 -0.407 -0.997  0 -2.413 -4.982 -1.637  0
#> CI_5  -1.876 -0.699 -1.262 NA -3.068 -8.525 -2.015 NA
#> CI_95 -0.871 -0.115 -0.733 NA -1.757 -1.440 -1.258 NA
#> 
#> $`Item 2`
#>           a1 a2     a3     b1 b2     b3
#> par   -0.984  0 -0.445 -1.928  0 -4.584
#> CI_5  -1.257 NA -0.720 -2.374 NA -7.352
#> CI_95 -0.711 NA -0.169 -1.481 NA -1.816
#> 
#> $`Item 3`
#>       a1     a2     a3 b1     b2     b3
#> par    0 -2.090 -1.363  0 -1.778 -2.058
#> CI_5  NA -2.658 -1.772 NA -2.051 -2.517
#> CI_95 NA -1.522 -0.955 NA -1.505 -1.598
#> 
#> $`Item 4`
#>           a1     a2     a3 a4     b1     b2    b3 b4
#> par   -2.963 -2.049 -0.252  0 -1.821 -1.842 1.268  0
#> CI_5  -4.189 -2.803 -0.421 NA -2.093 -2.205 0.324 NA
#> CI_95 -1.737 -1.295 -0.084 NA -1.549 -1.478 2.211 NA
#> 
#> $`Item 5`
#>           a1 a2     a3     a4     b1 b2     b3     b4
#> par   -0.805  0 -0.852 -0.669 -1.788  0 -1.281 -0.503
#> CI_5  -1.100 NA -1.115 -0.874 -2.443 NA -1.703 -0.796
#> CI_95 -0.510 NA -0.588 -0.464 -1.133 NA -0.859 -0.210
#> 
#> $`Item 6`
#>           a1     a2 a3     b1    b2 b3
#> par   -1.592 -0.908  0 -1.035 0.605  0
#> CI_5  -1.982 -1.122 NA -1.280 0.402 NA
#> CI_95 -1.201 -0.693 NA -0.790 0.807 NA
#> 
#> $`Item 7`
#>           a1     a2 a3     b1     b2 b3
#> par   -0.537 -0.190  0 -3.114 -2.431  0
#> CI_5  -0.801 -0.357 NA -4.556 -4.685 NA
#> CI_95 -0.273 -0.024 NA -1.671 -0.176 NA
#> 
#> $`Item 8`
#>           a1     a2 a3     b1     b2 b3
#> par   -1.036 -1.180  0 -1.554 -1.691  0
#> CI_5  -1.306 -1.493 NA -1.915 -2.063 NA
#> CI_95 -0.766 -0.866 NA -1.194 -1.318 NA
#> 
#> $`Item 9`
#>           a1     a2     a3 a4     b1     b2     b3 b4
#> par   -0.334 -1.171 -2.877  0  0.344 -1.731 -1.866  0
#> CI_5  -0.507 -1.554 -4.058 NA -0.092 -2.188 -2.153 NA
#> CI_95 -0.160 -0.787 -1.696 NA  0.780 -1.274 -1.578 NA
#> 
#> $`Item 10`
#>       a1     a2     a3 b1     b2     b3
#> par    0 -0.742 -0.798  0 -1.862 -1.751
#> CI_5  NA -0.978 -1.038 NA -2.428 -2.248
#> CI_95 NA -0.506 -0.558 NA -1.295 -1.254
#> 
#> $`Item 11`
#>       a1     a2     a3 b1     b2     b3
#> par    0 -1.480 -0.735  0 -1.952 -2.479
#> CI_5  NA -1.888 -0.996 NA -2.327 -3.286
#> CI_95 NA -1.073 -0.474 NA -1.577 -1.671
#> 
#> $`Item 12`
#>           a1     a2     a3 a4     a5     b1     b2     b3 b4     b5
#> par   -0.945 -1.078 -1.252  0 -0.731 -1.936 -3.344 -2.266  0 -1.081
#> CI_5  -1.256 -1.751 -1.719 NA -0.943 -2.514 -5.145 -2.942 NA -1.442
#> CI_95 -0.635 -0.406 -0.786 NA -0.518 -1.357 -1.544 -1.591 NA -0.721
#> 
#> $`Item 13`
#>       a1     a2     a3     a4 b1     b2     b3     b4
#> par    0 -1.517 -1.230 -0.972  0 -1.609 -1.355 -1.307
#> CI_5  NA -1.932 -1.544 -1.236 NA -1.948 -1.665 -1.665
#> CI_95 NA -1.102 -0.916 -0.707 NA -1.271 -1.045 -0.949
#> 
#> $`Item 14`
#>       a1     a2     a3     a4 b1     b2     b3     b4
#> par    0 -1.121 -1.004 -1.586  0 -2.625 -2.057 -1.886
#> CI_5  NA -1.559 -1.312 -2.027 NA -3.473 -2.600 -2.249
#> CI_95 NA -0.683 -0.697 -1.146 NA -1.776 -1.515 -1.524
#> 
#> $`Item 15`
#>           a1     a2 a3     a4     b1     b2 b3     b4
#> par   -0.978 -1.219  0 -0.487 -1.329 -0.620  0 -1.436
#> CI_5  -1.281 -1.485 NA -0.718 -1.751 -0.809 NA -2.264
#> CI_95 -0.675 -0.954 NA -0.256 -0.906 -0.431 NA -0.609
#> 
#> $`Item 16`
#>       a1     a2     a3     a4 b1     b2     b3     b4
#> par    0 -1.098 -1.583 -0.756  0 -0.913 -1.507 -3.026
#> CI_5  NA -1.349 -1.988 -1.144 NA -1.141 -1.799 -4.526
#> CI_95 NA -0.846 -1.178 -0.368 NA -0.686 -1.215 -1.526
#> 
#> $`Item 17`
#>           a1     a2 a3     a4     b1     b2 b3      b4
#> par    0.077 -0.294  0 -0.124 -0.484 -0.428  0  -6.894
#> CI_5  -0.115 -0.500 NA -0.377 -3.088 -1.070 NA -21.000
#> CI_95  0.270 -0.088 NA  0.128  2.119  0.214 NA   7.211
#> 
#> $`Item 18`
#>           a1     a2 a3     a4     b1     b2 b3     b4
#> par   -1.810 -1.866  0 -1.953 -1.787 -1.528  0 -1.993
#> CI_5  -2.334 -2.330 NA -2.613 -2.130 -1.777 NA -2.420
#> CI_95 -1.287 -1.403 NA -1.294 -1.443 -1.279 NA -1.566
#> 
#> $`Item 19`
#>           a1     a2 a3     a4     a5     b1     b2 b3     b4     b5
#> par   -1.352 -1.686  0 -1.117 -1.597 -2.170 -1.869  0 -2.758 -2.275
#> CI_5  -1.794 -2.164 NA -1.590 -2.173 -2.713 -2.226 NA -3.734 -2.852
#> CI_95 -0.909 -1.208 NA -0.645 -1.022 -1.626 -1.512 NA -1.781 -1.698
#> 
#> $`Item 20`
#>           a1     a2     a3 a4     b1     b2     b3 b4
#> par   -1.627 -1.727 -0.766  0 -2.796 -1.951 -1.738  0
#> CI_5  -2.472 -2.237 -0.998 NA -3.819 -2.323 -2.240 NA
#> CI_95 -0.783 -1.218 -0.533 NA -1.772 -1.580 -1.236 NA
#> 
coef(fitted_blis, IRTpars = TRUE, printSE = TRUE) # SE instead of CI
#> $`Item 1`
#>         a1     a2     a3 a4     b1     b2     b3 b4
#> par -1.374 -0.407 -0.997  0 -2.413 -4.982 -1.637  0
#> SE   0.305  0.177  0.161 NA  0.399  2.154  0.230 NA
#> 
#> $`Item 2`
#>         a1 a2     a3     b1 b2     b3
#> par -0.984  0 -0.445 -1.928  0 -4.584
#> SE   0.166 NA  0.168  0.271 NA  1.683
#> 
#> $`Item 3`
#>     a1     a2     a3 b1     b2     b3
#> par  0 -2.090 -1.363  0 -1.778 -2.058
#> SE  NA  0.345  0.248 NA  0.166  0.279
#> 
#> $`Item 4`
#>         a1     a2     a3 a4     b1     b2    b3 b4
#> par -2.963 -2.049 -0.252  0 -1.821 -1.842 1.268  0
#> SE   0.745  0.458  0.102 NA  0.165  0.221 0.574 NA
#> 
#> $`Item 5`
#>         a1 a2     a3     a4     b1 b2     b3     b4
#> par -0.805  0 -0.852 -0.669 -1.788  0 -1.281 -0.503
#> SE   0.180 NA  0.160  0.125  0.398 NA  0.257  0.178
#> 
#> $`Item 6`
#>         a1     a2 a3     b1    b2 b3
#> par -1.592 -0.908  0 -1.035 0.605  0
#> SE   0.237  0.130 NA  0.149 0.123 NA
#> 
#> $`Item 7`
#>         a1     a2 a3     b1     b2 b3
#> par -0.537 -0.190  0 -3.114 -2.431  0
#> SE   0.161  0.101 NA  0.877  1.371 NA
#> 
#> $`Item 8`
#>         a1    a2 a3     b1     b2 b3
#> par -1.036 -1.18  0 -1.554 -1.691  0
#> SE   0.164  0.19 NA  0.219  0.226 NA
#> 
#> $`Item 9`
#>         a1     a2     a3 a4    b1     b2     b3 b4
#> par -0.334 -1.171 -2.877  0 0.344 -1.731 -1.866  0
#> SE   0.106  0.233  0.718 NA 0.265  0.278  0.175 NA
#> 
#> $`Item 10`
#>     a1     a2     a3 b1     b2     b3
#> par  0 -0.742 -0.798  0 -1.862 -1.751
#> SE  NA  0.143  0.146 NA  0.344  0.302
#> 
#> $`Item 11`
#>     a1     a2     a3 b1     b2     b3
#> par  0 -1.480 -0.735  0 -1.952 -2.479
#> SE  NA  0.248  0.159 NA  0.228  0.491
#> 
#> $`Item 12`
#>         a1     a2     a3 a4     a5     b1     b2     b3 b4     b5
#> par -0.945 -1.078 -1.252  0 -0.731 -1.936 -3.344 -2.266  0 -1.081
#> SE   0.189  0.409  0.284 NA  0.129  0.352  1.095  0.411 NA  0.219
#> 
#> $`Item 13`
#>     a1     a2     a3     a4 b1     b2     b3     b4
#> par  0 -1.517 -1.230 -0.972  0 -1.609 -1.355 -1.307
#> SE  NA  0.252  0.191  0.161 NA  0.206  0.188  0.218
#> 
#> $`Item 14`
#>     a1     a2     a3     a4 b1     b2     b3     b4
#> par  0 -1.121 -1.004 -1.586  0 -2.625 -2.057 -1.886
#> SE  NA  0.266  0.187  0.268 NA  0.516  0.330  0.220
#> 
#> $`Item 15`
#>         a1     a2 a3     a4     b1     b2 b3     b4
#> par -0.978 -1.219  0 -0.487 -1.329 -0.620  0 -1.436
#> SE   0.184  0.162 NA  0.140  0.257  0.115 NA  0.503
#> 
#> $`Item 16`
#>     a1     a2     a3     a4 b1     b2     b3     b4
#> par  0 -1.098 -1.583 -0.756  0 -0.913 -1.507 -3.026
#> SE  NA  0.153  0.246  0.236 NA  0.139  0.178  0.912
#> 
#> $`Item 17`
#>        a1     a2 a3     a4     b1     b2 b3     b4
#> par 0.077 -0.294  0 -0.124 -0.484 -0.428  0 -6.894
#> SE  0.117  0.125 NA  0.154  1.583  0.390 NA  8.576
#> 
#> $`Item 18`
#>         a1     a2 a3     a4     b1     b2 b3     b4
#> par -1.810 -1.866  0 -1.953 -1.787 -1.528  0 -1.993
#> SE   0.318  0.282 NA  0.401  0.209  0.151 NA  0.260
#> 
#> $`Item 19`
#>         a1     a2 a3     a4     a5    b1     b2 b3     b4     b5
#> par -1.352 -1.686  0 -1.117 -1.597 -2.17 -1.869  0 -2.758 -2.275
#> SE   0.269  0.291 NA  0.287  0.350  0.33  0.217 NA  0.594  0.351
#> 
#> $`Item 20`
#>         a1     a2     a3 a4     b1     b2     b3 b4
#> par -1.627 -1.727 -0.766  0 -2.796 -1.951 -1.738  0
#> SE   0.513  0.310  0.141 NA  0.622  0.226  0.305 NA
#> 
```
