# Print method for BLIS coefficients

Print method for BLIS coefficients

## Usage

``` r
# S3 method for class 'blis_coefs'
print(x, digits = 3, ...)
```

## Arguments

- x:

  result of [`coef()`](https://rdrr.io/r/stats/coef.html).

- digits:

  *integer*, number of digits to show in the output. Note that printed
  object are still an original list, which does not round any value (it
  is returned invisibly).

- ...:

  Additional arguments passed on to
  [`print()`](https://rdrr.io/r/base/print.html).

## See also

Other BLIS/BLIRT related: [`BlisClass-class`](BlisClass-class.md),
[`coef,BlisClass-method`](coef-BlisClass-method.md),
[`fit_blis()`](fit_blis.md), [`get_orig_levels()`](get_orig_levels.md),
[`nominal_to_int()`](nominal_to_int.md),
[`obtain_nrm_def()`](obtain_nrm_def.md)
