# Obtain model definition for `mirt`'s nominal model taking in account the key of correct answers

Standard `mirt` model with `itemtype = "nominal"` puts the
identification constrains on the item response category slopes such as
\\ak_0 = 0\\ and \\ak\_{(K-1)} = (K - 1)\\, freely estimating the rest.

While nominal item responses are unordered by definition, it is often
the case that one of the item response categories is correct and the
respondents endorsing this category "naturally" possess a higher latent
ability. Use this function to obtain model definition where the correct
response category \\k_c\\ for item \\i\\ with \\K\\ possible response
categories translates to constrains \\ak\_{k_c} = (K - 1)\\ and
\\ak\_{k\_{d1}} = 0\\, with \\k\_{d1}\\ being the first incorrect
response category (i.e. the first distractor).

## Usage

``` r
obtain_nrm_def(data_with_key, ...)
```

## Arguments

- data_with_key:

  The output of [`nominal_to_int()`](nominal_to_int.md).

- ...:

  arguments passed onto
  [`mirt::mirt()`](https://philchalmers.github.io/mirt/reference/mirt.html).
  No practical use for now.

## Value

A `data.frame` with the starting values, parameter numbers, estimation
constrains etc. Pass it as `pars` argument of
[`mirt::mirt()`](https://philchalmers.github.io/mirt/reference/mirt.html).

## See also

Other BLIS/BLIRT related: [`BlisClass-class`](BlisClass-class.md),
[`coef,BlisClass-method`](coef-BlisClass-method.md),
[`fit_blis()`](fit_blis.md), [`get_orig_levels()`](get_orig_levels.md),
[`nominal_to_int()`](nominal_to_int.md),
[`print.blis_coefs()`](print.blis_coefs.md)

## Examples

``` r
library(mirt)

# convert nominal data to integers and the original labels with correct answers
data_with_key <- nominal_to_int(HCItest[, 1:20], HCIkey)

# build model definition for {mirt} using the returned list from above
nrm_def <- obtain_nrm_def(data_with_key)

# fit the nominal model using the obtained model definition in `pars` argument
fit <- mirt(data_with_key$Data, 1, "nominal", pars = nrm_def)
```
