# Compute traditional item analysis indices

Computes various traditional item analysis indices including difficulty,
discrimination and item validity. For ordinal items, the function
returns scaled values for some of the indices. See the details below.

## Usage

``` r
ItemAnalysis(
  Data,
  minscore = NULL,
  maxscore = NULL,
  cutscore = NULL,
  criterion = NULL,
  k = NULL,
  l = NULL,
  u = NULL,
  bin = "deprecated"
)
```

## Arguments

- Data:

  *matrix* or *data.frame* of items to be examined. Rows represent
  respondents, columns represent items.

- minscore, maxscore:

  *integer*, theoretical minimal/maximal score. If not provided, these
  are computed on observed data. Automatically recycled to the number of
  columns of the data.

- cutscore:

  *integer* If provided, the input data are binarized accordingly.
  Automatically recycled to the number of columns of the data.

- criterion:

  vector of criterion values.

- k, l, u:

  Arguments passed on to [`gDiscrim()`](gDiscrim.md). Provide these if
  you want to compute generalized upper-lower index along with a
  standard ULI (using `k` = 3, `l` = 1, `u` = 3), which is provided by
  default.

- bin:

  *deprecated*, use `cutscore` instead. See the **Details**.

## Value

A `data.frame` with following columns:

- `Difficulty`:

  average score of the item divided by its range.

- `Mean`:

  average item score.

- `SD`:

  standard deviation of the item score.

- `Cut.score`:

  cut-score specified in `cutscore`.

- `obs.min`:

  observed minimal score.

- `Min.score`:

  minimal score specified in `minscore`; if not provided, observed
  minimal score.

- `obs.max`:

  observed maximal score.

- `Max.score`:

  maximal score specified in `maxscore`; if not provided, observed
  maximal score.

- `Prop.max.score`:

  proportion of maximal scores.

- `RIT`:

  item-total correlation (correlation between item score and overall
  test score).

- `RIR`:

  item-rest correlation (correlation between item score and overall test
  score without the given item).

- `ULI`:

  upper-lower index using the standard parameters (3 groups, comparing
  1st and 3rd).

- `Corr.criterion`:

  correlation between item score and criterion `criterion`.

- `gULI`:

  generalized ULI. `NA` when the arguments `k`, `l`, and `u` were not
  provided.

- `Alpha.drop`:

  Cronbach's alpha without given item.

- `Index.rel`:

  Gulliksen's (1950) item reliability index.

- `Index.val`:

  Gulliksen's (1950) item validity index.

- `Perc.miss`:

  Percentage of missed responses on the particular item.

- `Perc.nr`:

  Percentage of respondents that did not reached the item nor the
  subsequent ones, see [`recode_nr()`](recode_nr.md) for further
  details.

## Details

For calculation of generalized ULI index, it is possible to specify a
custom number of groups `k`, and which two groups `l` and `u` are to be
compared.

In ordinal items, difficulty is calculated as difference of average
score divided by range (maximal possible score `maxscore` minus minimal
possible score `minscore`).

If `cutscore` is provided, item analysis is conducted on binarized data;
values greater or equal to cut-score are set to `1`, other values are
set to `0`. Both the `minscore` and `maxscore` arguments are then
ingored and set to 0 and 1, respectively.

## References

Martinkova, P., Stepanek, L., Drabinova, A., Houdek, J., Vejrazka, M., &
Stuka, C. (2017). Semi-real-time analyses of item characteristics for
medical school admission tests. In: Proceedings of the 2017 Federated
Conference on Computer Science and Information Systems.
https://doi.org/10.15439/2017F380

Gulliksen, H. (1950). *Theory of mental tests.* John Wiley & Sons Inc.
https://doi.org/10.1037/13240-000

## See also

[`DDplot()`](DDplot.md), [`gDiscrim()`](gDiscrim.md),
[`recode_nr()`](recode_nr.md)

## Author

Patricia Martinkova  
Institute of Computer Science of the Czech Academy of Sciences  
<martinkova@cs.cas.cz>

Jan Netik  
Institute of Computer Science of the Czech Academy of Sciences  
<netik@cs.cas.cz>

Jana Vorlickova  
Institute of Computer Science of the Czech Academy of Sciences

Adela Hladka  
Institute of Computer Science of the Czech Academy of Sciences  
<hladka@cs.cas.cz>

## Examples

``` r
if (FALSE) { # \dontrun{
# binary dataset
dataBin <- dataMedical[, 1:100]
# ordinal dataset
dataOrd <- dataMedicalgraded[, 1:100]
# study success is the same for both data sets
StudySuccess <- dataMedical[, 102]

# item analysis for binary data
head(ItemAnalysis(dataBin))
# item analysis for binary data using also study success
head(ItemAnalysis(dataBin, criterion = StudySuccess))

# item analysis for binary data
head(ItemAnalysis(dataOrd))
# item analysis for binary data using also study success
head(ItemAnalysis(dataOrd, criterion = StudySuccess))
# including also item analysis for binarized data
head(ItemAnalysis(dataOrd,
  criterion = StudySuccess, k = 5, l = 4, u = 5,
  maxscore = 4, minscore = 0, cutscore = 4
))
} # }
```
