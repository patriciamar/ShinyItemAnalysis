# Range-restricted reliability with intra-class correlation

Function estimating reliability with intra-class correlation for the
complete or for the range-restricted sample.

## Usage

``` r
ICCrestricted(
  Data,
  case,
  var,
  rank = NULL,
  dir = "top",
  sel = 1,
  nsim = 100,
  ci = 0.95,
  seed = NULL
)
```

## Arguments

- Data:

  `matrix` or `data.frame` which includes variables describing ID of
  ratees (specified in `case`), ratings (specified in `var`), and
  (optionally) rank of ratees (specified in `rank`).

- case:

  character: name of the variable in `Data` with ID of the ratee
  (subject or object being evaluated, such as a respondent, proposal,
  patient, applicant etc.)

- var:

  character: name of the variable in `Data` with the ratings/scores.

- rank:

  numeric: vector of ranks of ratees. If not provided, rank of ratee is
  calculated based on average rating based on `var` variable.

- dir:

  character: direction of range-restriction, available options are
  `"top"` (default) or `"bottom"`. Can be an unambiguous abbreviation
  (i.e., `"t"` or `"b"`).

- sel:

  numeric: selected number (given \> 1) or percentage (given \<= 1) of
  ratees. Default value is 1 (complete dataset).

- nsim:

  numeric: number of simulations for bootstrap confidence interval.
  Default value is 100.

- ci:

  numeric: confidence interval. Default value is 0.95.

- seed:

  seed for simulations. Default value is `NULL`, random seed. See
  [`lme4::bootMer()`](https://rdrr.io/pkg/lme4/man/bootMer.html) for
  more detail.

## Value

A `data.frame` with the following columns:

- n_sel:

  number of ratees selected/subsetted.

- prop_sel:

  proportion of ratees selected.

- dir:

  direction of range-restriction. `NA` if range is effectively not
  restricted (100% used).

- VarID:

  variance due to ratee, "true variance", between-group variance.

- VarResid:

  residual variance.

- VarTotal:

  total variance.

- ICC1:

  single-rater inter-rater reliability.

- ICC1_LCI:

  lower bound of the confidence interval for `ICC1`.

- ICC1_UCI:

  upper bound of the confidence interval for `ICC1`.

- ICC3:

  multiple-rater inter-rater reliability.

- ICC3_LCI:

  lower bound of the confidence interval for `ICC3`.

- ICC3_UCI:

  upper bound of the confidence interval for `ICC3`.

## References

Erosheva, E., Martinkova, P., & Lee, C. (2021a). When zero may not be
zero: A cautionary note on the use of inter-rater reliability in
evaluating grant peer review. Journal of the Royal Statistical Society -
Series A. Accepted.

Erosheva, E., Martinkova, P., & Lee, C. (2021b). Supplementary material
for When zero may not be zero: A cautionary note on the use of
inter-rater reliability in evaluating grant peer review.

## Author

Patricia Martinkova  
Institute of Computer Science of the Czech Academy of Sciences  
<martinkova@cs.cas.cz>

Jan Netik  
Institute of Computer Science of the Czech Academy of Sciences  
<netik@cs.cas.cz>

## Examples

``` r
# ICC for the whole sample
ICCrestricted(Data = AIBS, case = "ID", var = "Score", rank = "ScoreRankAdj")
#>   n_sel prop_sel dir     VarID  VarResid  VarTotal      ICC1  ICC1_LCI
#> 1    72        1  NA 0.2362446 0.4002778 0.6365223 0.3711489 0.2212916
#>    ICC1_UCI     ICC3  ICC3_LCI  ICC3_UCI
#> 1 0.5025967 0.639068 0.4600609 0.7519195

# ICC for the range-restricted sample considering 80% of top ratees
ICCrestricted(
  Data = AIBS, case = "ID", var = "Score", rank = "ScoreRankAdj",
  sel = 0.8
)
#>   n_sel  prop_sel dir      VarID  VarResid  VarTotal       ICC1 ICC1_LCI
#> 1    58 0.8055556 top 0.03090206 0.4116667 0.4425687 0.06982432        0
#>    ICC1_UCI      ICC3 ICC3_LCI  ICC3_UCI
#> 1 0.2228631 0.1838049        0 0.4624359
```
