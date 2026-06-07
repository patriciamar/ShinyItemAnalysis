# Get Coefficients from a fitted BLIS model

Extracts item parameters from fitted BLIS model. For BLIRT
parametrization, use `IRTpars = TRUE` in your function call. Contrary to
[mirt::coef,SingleGroupClass-method](https://philchalmers.github.io/mirt/reference/coef-method.html),
response category labels can be displayed in the output using
`labels = TRUE`. On top of that, as BLIS/BLIRT parametrizations utilize
the information of correct response category, you can denote these in
the output with `mark_correct = TRUE`.

## Usage

``` r
# S4 method for class 'BlisClass'
coef(
  object,
  ...,
  CI = 0.95,
  printSE = FALSE,
  IRTpars = FALSE,
  simplify = FALSE,
  labels = FALSE,
  mark_correct = labels
)
```

## Arguments

- object:

  *object of class [BlisClass](BlisClass-class.md)*, model fitted via
  `fit_blis`() or [`blis()`](fit_blis.md).

- ...:

  Additional arguments. Not utilized at the moment.

- CI:

  *numeric*, a width of the confidence intervals.

- printSE:

  *logical*, print standard errors instead of CI? Defaults to `FALSE`.

- IRTpars:

  *logical*, convert slope intercept parameters into IRT parameters
  (i.e. BLIRT)? Defaults to `FALSE`.

- simplify:

  *logical*, return coefficients as a matrix, instead of list? Defaults
  to `FALSE`. *Not implemented yet.*

- labels:

  *logical*, if `TRUE`, show response labels (e.g. "A", "B", "C")
  instead of response numeric indices (e.g. 0, 1, 2). Defaults to
  `FALSE`.

- mark_correct:

  *logical*, mark the correct response with an asterisk symbol.
  Applicable only if `labels` is `TRUE` (in which case, `mark_correct`
  defaults to `TRUE`).

## Value

List of item coefficients of S3 class `blis_coefs`, so the resulting
output of [`coef()`](https://rdrr.io/r/stats/coef.html) call is
formatted to display only first 3 digits (you can opt for different
rounding via the [print.blis_coefs](print.blis_coefs.md) method, see the
examples). Note that the list-object returned invisibly has the raw
coefficients stored in it.

## See also

Other BLIS/BLIRT related: [`BlisClass-class`](BlisClass-class.md),
[`fit_blis()`](fit_blis.md), [`get_orig_levels()`](get_orig_levels.md),
[`nominal_to_int()`](nominal_to_int.md),
[`obtain_nrm_def()`](obtain_nrm_def.md),
[`print.blis_coefs()`](print.blis_coefs.md)

## Examples

``` r
fitted_blis <- fit_blis(HCItest[, 1:20], HCIkey)

# BLIS coefs
coef(fitted_blis)
#> $`Item 1`
#>        ak0    ak1    ak2 ak3     d0     d1     d2 d3
#> par -1.374 -0.407 -0.997   0 -3.315 -2.029 -1.632  0
#> 
#> $`Item 2`
#>        ak0 ak1    ak2     d0 d1     d2
#> par -0.984   0 -0.445 -1.897  0 -2.039
#> 
#> $`Item 3`
#>     ak0   ak1    ak2 d0     d1     d2
#> par   0 -2.09 -1.363  0 -3.716 -2.805
#> 
#> $`Item 4`
#>        ak0    ak1    ak2 ak3     d0     d1   d2 d3
#> par -2.963 -2.049 -0.252   0 -5.397 -3.774 0.32  0
#> 
#> $`Item 5`
#>        ak0 ak1    ak2    ak3    d0 d1     d2     d3
#> par -0.805   0 -0.852 -0.669 -1.44  0 -1.091 -0.336
#> 
#> $`Item 6`
#>        ak0    ak1 ak2     d0    d1 d2
#> par -1.592 -0.908   0 -1.647 0.549  0
#> 
#> $`Item 7`
#>        ak0   ak1 ak2     d0     d1 d2
#> par -0.537 -0.19   0 -1.673 -0.463  0
#> 
#> $`Item 8`
#>        ak0   ak1 ak2     d0     d1 d2
#> par -1.036 -1.18   0 -1.611 -1.994  0
#> 
#> $`Item 9`
#>        ak0    ak1    ak2 ak3    d0     d1     d2 d3
#> par -0.334 -1.171 -2.877   0 0.115 -2.026 -5.367  0
#> 
#> $`Item 10`
#>     ak0    ak1    ak2 d0     d1     d2
#> par   0 -0.742 -0.798  0 -1.381 -1.397
#> 
#> $`Item 11`
#>     ak0   ak1    ak2 d0     d1     d2
#> par   0 -1.48 -0.735  0 -2.889 -1.822
#> 
#> $`Item 12`
#>        ak0    ak1    ak2 ak3    ak4    d0     d1     d2 d3    d4
#> par -0.945 -1.078 -1.252   0 -0.731 -1.83 -3.606 -2.838  0 -0.79
#> 
#> $`Item 13`
#>     ak0    ak1   ak2    ak3 d0     d1     d2    d3
#> par   0 -1.517 -1.23 -0.972  0 -2.442 -1.666 -1.27
#> 
#> $`Item 14`
#>     ak0    ak1    ak2    ak3 d0     d1     d2     d3
#> par   0 -1.121 -1.004 -1.586  0 -2.942 -2.066 -2.993
#> 
#> $`Item 15`
#>        ak0    ak1 ak2    ak3     d0     d1 d2   d3
#> par -0.978 -1.219   0 -0.487 -1.299 -0.756  0 -0.7
#> 
#> $`Item 16`
#>     ak0    ak1    ak2    ak3 d0     d1     d2     d3
#> par   0 -1.098 -1.583 -0.756  0 -1.003 -2.385 -2.287
#> 
#> $`Item 17`
#>       ak0    ak1 ak2    ak3    d0     d1 d2     d3
#> par 0.077 -0.294   0 -0.124 0.037 -0.126  0 -0.855
#> 
#> $`Item 18`
#>       ak0    ak1 ak2    ak3     d0     d1 d2     d3
#> par -1.81 -1.866   0 -1.953 -3.235 -2.851  0 -3.892
#> 
#> $`Item 19`
#>        ak0    ak1 ak2    ak3    ak4     d0     d1 d2     d3     d4
#> par -1.352 -1.686   0 -1.117 -1.597 -2.932 -3.152  0 -3.081 -3.634
#> 
#> $`Item 20`
#>        ak0    ak1    ak2 ak3    d0    d1     d2 d3
#> par -1.627 -1.727 -0.766   0 -4.55 -3.37 -1.331  0
#> 

# BLIRT coefs
coef(fitted_blis, IRTpars = TRUE)
#> $`Item 1`
#>         a1     a2     a3 a4     b1     b2     b3 b4
#> par -1.374 -0.407 -0.997  0 -2.413 -4.982 -1.637  0
#> 
#> $`Item 2`
#>         a1 a2     a3     b1 b2     b3
#> par -0.984  0 -0.445 -1.928  0 -4.584
#> 
#> $`Item 3`
#>     a1    a2     a3 b1     b2     b3
#> par  0 -2.09 -1.363  0 -1.778 -2.058
#> 
#> $`Item 4`
#>         a1     a2     a3 a4     b1     b2    b3 b4
#> par -2.963 -2.049 -0.252  0 -1.821 -1.842 1.268  0
#> 
#> $`Item 5`
#>         a1 a2     a3     a4     b1 b2     b3     b4
#> par -0.805  0 -0.852 -0.669 -1.788  0 -1.281 -0.503
#> 
#> $`Item 6`
#>         a1     a2 a3     b1    b2 b3
#> par -1.592 -0.908  0 -1.035 0.605  0
#> 
#> $`Item 7`
#>         a1    a2 a3     b1     b2 b3
#> par -0.537 -0.19  0 -3.114 -2.431  0
#> 
#> $`Item 8`
#>         a1    a2 a3     b1     b2 b3
#> par -1.036 -1.18  0 -1.554 -1.691  0
#> 
#> $`Item 9`
#>         a1     a2     a3 a4    b1     b2     b3 b4
#> par -0.334 -1.171 -2.877  0 0.344 -1.731 -1.866  0
#> 
#> $`Item 10`
#>     a1     a2     a3 b1     b2     b3
#> par  0 -0.742 -0.798  0 -1.862 -1.751
#> 
#> $`Item 11`
#>     a1    a2     a3 b1     b2     b3
#> par  0 -1.48 -0.735  0 -1.952 -2.479
#> 
#> $`Item 12`
#>         a1     a2     a3 a4     a5     b1     b2     b3 b4     b5
#> par -0.945 -1.078 -1.252  0 -0.731 -1.936 -3.344 -2.266  0 -1.081
#> 
#> $`Item 13`
#>     a1     a2    a3     a4 b1     b2     b3     b4
#> par  0 -1.517 -1.23 -0.972  0 -1.609 -1.355 -1.307
#> 
#> $`Item 14`
#>     a1     a2     a3     a4 b1     b2     b3     b4
#> par  0 -1.121 -1.004 -1.586  0 -2.625 -2.057 -1.886
#> 
#> $`Item 15`
#>         a1     a2 a3     a4     b1    b2 b3     b4
#> par -0.978 -1.219  0 -0.487 -1.329 -0.62  0 -1.436
#> 
#> $`Item 16`
#>     a1     a2     a3     a4 b1     b2     b3     b4
#> par  0 -1.098 -1.583 -0.756  0 -0.913 -1.507 -3.026
#> 
#> $`Item 17`
#>        a1     a2 a3     a4     b1     b2 b3     b4
#> par 0.077 -0.294  0 -0.124 -0.484 -0.428  0 -6.894
#> 
#> $`Item 18`
#>        a1     a2 a3     a4     b1     b2 b3     b4
#> par -1.81 -1.866  0 -1.953 -1.787 -1.528  0 -1.993
#> 
#> $`Item 19`
#>         a1     a2 a3     a4     a5    b1     b2 b3     b4     b5
#> par -1.352 -1.686  0 -1.117 -1.597 -2.17 -1.869  0 -2.758 -2.275
#> 
#> $`Item 20`
#>         a1     a2     a3 a4     b1     b2     b3 b4
#> par -1.627 -1.727 -0.766  0 -2.796 -1.951 -1.738  0
#> 

# store raw coefs
blis_coefs <- coef(fitted_blis)

# print coefs rounded to 2 digits
print(blis_coefs, digits = 2)
#> $`Item 1`
#>       ak0   ak1 ak2 ak3    d0    d1    d2 d3
#> par -1.37 -0.41  -1   0 -3.31 -2.03 -1.63  0
#> 
#> $`Item 2`
#>       ak0 ak1   ak2   d0 d1    d2
#> par -0.98   0 -0.44 -1.9  0 -2.04
#> 
#> $`Item 3`
#>     ak0   ak1   ak2 d0    d1    d2
#> par   0 -2.09 -1.36  0 -3.72 -2.81
#> 
#> $`Item 4`
#>       ak0   ak1   ak2 ak3   d0    d1   d2 d3
#> par -2.96 -2.05 -0.25   0 -5.4 -3.77 0.32  0
#> 
#> $`Item 5`
#>       ak0 ak1   ak2   ak3    d0 d1    d2    d3
#> par -0.81   0 -0.85 -0.67 -1.44  0 -1.09 -0.34
#> 
#> $`Item 6`
#>       ak0   ak1 ak2    d0   d1 d2
#> par -1.59 -0.91   0 -1.65 0.55  0
#> 
#> $`Item 7`
#>       ak0   ak1 ak2    d0    d1 d2
#> par -0.54 -0.19   0 -1.67 -0.46  0
#> 
#> $`Item 8`
#>       ak0   ak1 ak2    d0    d1 d2
#> par -1.04 -1.18   0 -1.61 -1.99  0
#> 
#> $`Item 9`
#>       ak0   ak1   ak2 ak3   d0    d1    d2 d3
#> par -0.33 -1.17 -2.88   0 0.11 -2.03 -5.37  0
#> 
#> $`Item 10`
#>     ak0   ak1  ak2 d0    d1   d2
#> par   0 -0.74 -0.8  0 -1.38 -1.4
#> 
#> $`Item 11`
#>     ak0   ak1   ak2 d0    d1    d2
#> par   0 -1.48 -0.74  0 -2.89 -1.82
#> 
#> $`Item 12`
#>       ak0   ak1   ak2 ak3   ak4    d0    d1    d2 d3    d4
#> par -0.95 -1.08 -1.25   0 -0.73 -1.83 -3.61 -2.84  0 -0.79
#> 
#> $`Item 13`
#>     ak0   ak1   ak2   ak3 d0    d1    d2    d3
#> par   0 -1.52 -1.23 -0.97  0 -2.44 -1.67 -1.27
#> 
#> $`Item 14`
#>     ak0   ak1 ak2   ak3 d0    d1    d2    d3
#> par   0 -1.12  -1 -1.59  0 -2.94 -2.07 -2.99
#> 
#> $`Item 15`
#>       ak0   ak1 ak2   ak3   d0    d1 d2   d3
#> par -0.98 -1.22   0 -0.49 -1.3 -0.76  0 -0.7
#> 
#> $`Item 16`
#>     ak0  ak1   ak2   ak3 d0 d1    d2    d3
#> par   0 -1.1 -1.58 -0.76  0 -1 -2.39 -2.29
#> 
#> $`Item 17`
#>      ak0   ak1 ak2   ak3   d0    d1 d2    d3
#> par 0.08 -0.29   0 -0.12 0.04 -0.13  0 -0.86
#> 
#> $`Item 18`
#>       ak0   ak1 ak2   ak3    d0    d1 d2    d3
#> par -1.81 -1.87   0 -1.95 -3.23 -2.85  0 -3.89
#> 
#> $`Item 19`
#>       ak0   ak1 ak2   ak3  ak4    d0    d1 d2    d3    d4
#> par -1.35 -1.69   0 -1.12 -1.6 -2.93 -3.15  0 -3.08 -3.63
#> 
#> $`Item 20`
#>       ak0   ak1   ak2 ak3    d0    d1    d2 d3
#> par -1.63 -1.73 -0.77   0 -4.55 -3.37 -1.33  0
#> 
```
