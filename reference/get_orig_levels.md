# Get Original Levels from a Fitted BLIS model

Just a simple accessor to original levels and correct key stored in
fitted BLIS model.

## Usage

``` r
get_orig_levels(object)
```

## Arguments

- object:

  *object of class [BlisClass](BlisClass-class.md)*, model fitted via
  [`fit_blis()`](fit_blis.md) or [`blis()`](fit_blis.md).

## Value

*list* of the original levels and correct key. Key is stored as an
attribute `key` for every individual item.

## See also

Other BLIS/BLIRT related: [`BlisClass-class`](BlisClass-class.md),
[`coef,BlisClass-method`](coef-BlisClass-method.md),
[`fit_blis()`](fit_blis.md), [`nominal_to_int()`](nominal_to_int.md),
[`obtain_nrm_def()`](obtain_nrm_def.md),
[`print.blis_coefs()`](print.blis_coefs.md)

## Examples

``` r
fit <- fit_blis(HCItest[, 1:20], HCIkey)
get_orig_levels(fit)
#> $`Item 1`
#> [1] "A" "B" "C" "D"
#> attr(,"key")
#> [1] FALSE FALSE FALSE  TRUE
#> 
#> $`Item 2`
#> [1] "A" "B" "C"
#> attr(,"key")
#> [1] FALSE  TRUE FALSE
#> 
#> $`Item 3`
#> [1] "A" "B" "C"
#> attr(,"key")
#> [1]  TRUE FALSE FALSE
#> 
#> $`Item 4`
#> [1] "A" "B" "C" "D"
#> attr(,"key")
#> [1] FALSE FALSE FALSE  TRUE
#> 
#> $`Item 5`
#> [1] "A" "B" "C" "D"
#> attr(,"key")
#> [1] FALSE  TRUE FALSE FALSE
#> 
#> $`Item 6`
#> [1] "A" "B" "C"
#> attr(,"key")
#> [1] FALSE FALSE  TRUE
#> 
#> $`Item 7`
#> [1] "A" "B" "C"
#> attr(,"key")
#> [1] FALSE FALSE  TRUE
#> 
#> $`Item 8`
#> [1] "A" "B" "C"
#> attr(,"key")
#> [1] FALSE FALSE  TRUE
#> 
#> $`Item 9`
#> [1] "A" "B" "C" "D"
#> attr(,"key")
#> [1] FALSE FALSE FALSE  TRUE
#> 
#> $`Item 10`
#> [1] "A" "B" "C"
#> attr(,"key")
#> [1]  TRUE FALSE FALSE
#> 
#> $`Item 11`
#> [1] "A" "B" "C"
#> attr(,"key")
#> [1]  TRUE FALSE FALSE
#> 
#> $`Item 12`
#> [1] "A" "B" "C" "D" "E"
#> attr(,"key")
#> [1] FALSE FALSE FALSE  TRUE FALSE
#> 
#> $`Item 13`
#> [1] "A" "B" "C" "D"
#> attr(,"key")
#> [1]  TRUE FALSE FALSE FALSE
#> 
#> $`Item 14`
#> [1] "A" "B" "C" "D"
#> attr(,"key")
#> [1]  TRUE FALSE FALSE FALSE
#> 
#> $`Item 15`
#> [1] "A" "B" "C" "D"
#> attr(,"key")
#> [1] FALSE FALSE  TRUE FALSE
#> 
#> $`Item 16`
#> [1] "A" "B" "C" "D"
#> attr(,"key")
#> [1]  TRUE FALSE FALSE FALSE
#> 
#> $`Item 17`
#> [1] "A" "B" "C" "D"
#> attr(,"key")
#> [1] FALSE FALSE  TRUE FALSE
#> 
#> $`Item 18`
#> [1] "A" "B" "C" "D"
#> attr(,"key")
#> [1] FALSE FALSE  TRUE FALSE
#> 
#> $`Item 19`
#> [1] "A" "B" "C" "D" "E"
#> attr(,"key")
#> [1] FALSE FALSE  TRUE FALSE FALSE
#> 
#> $`Item 20`
#> [1] "A" "B" "C" "D"
#> attr(,"key")
#> [1] FALSE FALSE FALSE  TRUE
#> 
```
