# Compute Cronbach alpha with confidence interval

So far internal function.

## Usage

``` r
cronbach_alpha(Data, ci = TRUE, ci_lvl = 0.95)
```

## Arguments

- Data:

  *data.frame* or *matrix*, item data, `NA` gets excluded automatically.

- ci:

  *logical*, whether to compute CI or not. Defaults to `TRUE`.

- ci_lvl:

  *numeric* ranging from 0 to 1, a confidence level to construct CI for.
  Defaults to `.95`.

## Value

A list with \\\alpha\\ estimate and optionally CI.

## Examples

``` r
ShinyItemAnalysis:::cronbach_alpha(HCI[, 1:20])
#> $estimate
#> [1] 0.7154535
#> 
#> $ci
#> [1] 0.6827526 0.7462292
#> 
```
