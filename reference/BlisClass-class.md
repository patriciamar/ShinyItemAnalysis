# BLIS S4 class

Extends `mirt`'s `SingleGroupClass` directly (meaning all `mirt` methods
that work with that class will work with BlisClass too; make sure `mirt`
is loaded).

## Details

The purpose of the class is to have a custom `coef` method (see
[coef,BlisClass-method](coef-BlisClass-method.md)) dispatched and the
original levels with correct response (as a `key` attribute) stored in
the resulting fitted model.

## Slots

- `orig_levels`:

  *list* of original levels with logical attribute `key`, which stores
  the information on which response (level) has been considered as
  correct. Note that levels not used in the original data are dropped.

## See also

Other BLIS/BLIRT related:
[`coef,BlisClass-method`](coef-BlisClass-method.md),
[`fit_blis()`](fit_blis.md), [`get_orig_levels()`](get_orig_levels.md),
[`nominal_to_int()`](nominal_to_int.md),
[`obtain_nrm_def()`](obtain_nrm_def.md),
[`print.blis_coefs()`](print.blis_coefs.md)
