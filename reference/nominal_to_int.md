# Turn nominal (factor) data to integers, keep original levels with a key of correct responses alongside

Convert a `data.frame` or `tibble` with factor variables (items) to
integers, keeping the original factor levels (i.e. response categories)
and correct answers (stored as an `key` attribute of each item)
alongside.

## Usage

``` r
nominal_to_int(Data, key)
```

## Arguments

- Data:

  *data.frame* or *tibble* with all columns being factors. Support for
  *matrix* is limited and behavior not guaranteed.

- key:

  A single-column `data.frame`, (**not** matrix) `tibble` or -
  preferably - a factor vector of levels considered as correct
  responses.

## Value

*List* of original levels with logical attribute `key`, which stores the
information on which response (level) is considered correct. *Note that
levels not used in the original data are dropped.*

## Details

Fitting a nominal model using
[`mirt::mirt()`](https://philchalmers.github.io/mirt/reference/mirt.html)
package requires the dataset to consist only of integers, *arbitrarily*
representing the response categories. You can convert your dataset to
integers on your own in that case.

On the other hand, BLIS model (and thus also the BLIRT parametrization)
further requires the information of correct item response category. On
top of that, the same information is leveraged when fitting a `mirt`
model that conserves the "directionality" of estimated latent ability
(using a model definition from [`obtain_nrm_def()`](obtain_nrm_def.md)).
In these cases, you are recommended to use `nominal_to_int()` (note that
[`fit_blis()`](fit_blis.md) and [`blis()`](fit_blis.md) does this
internally). Note also that fitted BLIS model (of class
[BlisClass](BlisClass-class.md)) stores the original levels with correct
answer key in its `orig_levels` slot, accessible by a user via
[`get_orig_levels()`](get_orig_levels.md).

## See also

Other BLIS/BLIRT related: [`BlisClass-class`](BlisClass-class.md),
[`coef,BlisClass-method`](coef-BlisClass-method.md),
[`fit_blis()`](fit_blis.md), [`get_orig_levels()`](get_orig_levels.md),
[`obtain_nrm_def()`](obtain_nrm_def.md),
[`print.blis_coefs()`](print.blis_coefs.md)
