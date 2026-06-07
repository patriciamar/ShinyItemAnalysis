# Dichotomous dataset of Medical School Admission Test in Biology.

The `MSATB` dataset consists of the responses of 1,407 subjects (484
males, 923 females) to admission test to medical school in the Czech
republic. It contains 20 selected items from original test while first
item was previously detected as differently functioning (Vlckova, 2014).
A correct answer is coded as 1 and incorrect answer as 0. The column
`gender` represents gender of students, where 0 indicates males
(reference group) and 1 indicates females (focal group).

## Usage

``` r
MSATB
```

## Format

A `MSATB` data frame consists of 1,407 observations on the following 21
variables:

- Item:

  dichotomously scored items of the test

- gender:

  gender of respondents, `"0"` males, `"1"` females

## Source

Reexport from `difNLR` package.

## References

Drabinova, A. & Martinkova, P. (2017). Detection of differential item
functioning with nonlinear regression: A non-IRT approach accounting for
guessing. Journal of Educational Measurement, 54(4), 498–517,
[doi:10.1111/jedm.12158](https://doi.org/10.1111/jedm.12158) .

Vlckova, K. (2014). Test and item fairness. Master's thesis. Faculty of
Mathematics and Physics, Charles University.

## Author

Adela Hladka (nee Drabinova)  
Institute of Computer Science of the Czech Academy of Sciences  
Faculty of Mathematics and Physics, Charles University  
<hladka@cs.cas.cz>  

Patricia Martinkova  
Institute of Computer Science of the Czech Academy of Sciences  
<martinkova@cs.cas.cz>  
