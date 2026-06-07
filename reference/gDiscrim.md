# Compute generalized item discrimination

Generalized version of discrimination index ULI. The function enumerates
the ability of an item to distinguish between individuals from upper (U)
vs. lower (L) ability groups, i.e. between respondents with high vs. low
overall score on the test. Number of groups, as well as upper and lower
groups can be specified by user. You can also manually supply the
maximal and minimal scores when the theoretical range of item score is
known. Note that if the *observed* item range is zero `NaN` is returned.

## Usage

``` r
gDiscrim(Data, k = 3, l = 1, u = 3, maxscore, minscore, x, ...)
```

## Arguments

- Data:

  matrix or data.frame of items to be examined. Rows represent
  respondents, columns represent items.

- k:

  numeric: number of groups to which may be `Data` divided by the total
  score. Default value is 3. See **Details**.

- l:

  numeric: lower group. Default value is 1. See **Details**.

- u:

  numeric: upper group. Default value is 3. See **Details**.

- maxscore:

  numeric: maximal score in ordinal items. If missing, vector of
  obtained maximal scores is imputed. See **Details**.

- minscore:

  numeric: minimal score in ordinal items. If missing, vector of
  obtained minimal scores is imputed. See **Details**.

- x:

  deprecated. Use argument `Data` instead.

- ...:

  Arguments passed on to
  [`base::findInterval`](https://rdrr.io/r/base/findInterval.html)

  `rightmost.closed`

  :   logical; if true, the rightmost interval, `vec[N-1] .. vec[N]` is
      treated as *closed*, see below.

  `all.inside`

  :   logical; if true, the returned indices are coerced into
      `1,...,N-1`, i.e., `0` is mapped to `1` and `N` to `N-1`.

  `left.open`

  :   logical; if true all the intervals are open at left and closed at
      right; in the formulas below, \\\le\\ should be swapped with
      \\\<\\ (and \\\>\\ with \\\ge\\), and `rightmost.closed` means
      ‘leftmost is closed’. This may be useful, e.g., in survival
      analysis computations.

  `checkSorted`

  :   logical indicating if `vec` should be checked, i.e.,
      [`is.unsorted`](https://rdrr.io/r/base/is.unsorted.html)`(vec)` is
      asserted to be false. Setting this to `FALSE` skips the check
      gaining speed, but may return nonsense results in case `vec` is
      not sorted.

  `checkNA`

  :   logical indicating if each `x[i]` should be checked as with
      [`is.na`](https://rdrr.io/r/base/NA.html)`(.)`. Setting this to
      `FALSE` in case of `NA`'s in `x[]` may result in platform
      dependent nonsense.

## Details

The function computes total test scores for all respondents and then
divides the respondents into `k` groups. The lower and upper groups are
determined by `l` and `u` parameters, i.e., l-th and u-th group where
the ordering is defined by increasing total score.

In ordinal items, difficulty is calculated as difference of average
score divided by range (maximal possible score `maxscore` minus minimal
possible score `minscore` for given item).

Discrimination is calculated as difference in difficulty between upper
and lower group.

## Note

`gDiscrim` is used by [`DDplot()`](DDplot.md) function.

## References

Martinkova, P., Stepanek, L., Drabinova, A., Houdek, J., Vejrazka, M., &
Stuka, C. (2017). Semi-real-time analyses of item characteristics for
medical school admission tests. In: Proceedings of the 2017 Federated
Conference on Computer Science and Information Systems.
https://doi.org/10.15439/2017F380

## See also

[`DDplot()`](DDplot.md)

## Author

Adela Hladka  
Institute of Computer Science of the Czech Academy of Sciences  
<hladka@cs.cas.cz>

Lubomir Stepanek  
Institute of Computer Science of the Czech Academy of Sciences

Jana Vorlickova  
Institute of Computer Science of the Czech Academy of Sciences

Patricia Martinkova  
Institute of Computer Science of the Czech Academy of Sciences  
<martinkova@cs.cas.cz>

Jan Netik  
Institute of Computer Science of the Czech Academy of Sciences  
<netik@cs.cas.cz>

## Examples

``` r
# binary dataset
dataBin <- dataMedical[, 1:100]
# ordinal dataset
dataOrd <- dataMedicalgraded[, 1:100]

# ULI for the first 5 items of binary dataset
# compare to psychometric::discrim(dataBin)
gDiscrim(dataBin)[1:5]
#>     X2001     X2002     X2003     X2004     X2005 
#> 0.1614213 0.1035596 0.4885943 0.3868710 0.3318481 
# generalized ULI using 5 groups, compare 4th and 5th for binary dataset
gDiscrim(dataBin, k = 5, l = 4, u = 5)[1:5]
#>     X2001     X2002     X2003     X2004     X2005 
#> 0.1018641 0.1121100 0.1987705 0.2145360 0.1099617 

# ULI for first 5 items for ordinal dataset
gDiscrim(dataOrd)[1:5]
#>      X2001      X2002      X2003      X2004      X2005 
#> 0.09572166 0.16840558 0.31334624 0.19898637 0.17202272 
# generalized ULI using 5 groups, compare 4th and 5th for binary dataset
gDiscrim(dataOrd, k = 5, l = 4, u = 5)[1:5]
#>      X2001      X2002      X2003      X2004      X2005 
#> 0.03969971 0.12682360 0.08110496 0.09022718 0.05604466 
# maximum (4) and minimum (0) score are same for all items
gDiscrim(dataOrd, k = 5, l = 4, u = 5, maxscore = 4, minscore = 0)[1:5]
#>      X2001      X2002      X2003      X2004      X2005 
#> 0.03969971 0.12682360 0.08110496 0.09022718 0.05604466 
```
