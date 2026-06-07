# Dichotomous dataset based on GMAT with the same total score distribution for groups.

The `GMAT` is a generated dataset based on parameters from Graduate
Management Admission Test (GMAT, Kingston et al., 1985). First two items
were considered to function differently in uniform and non-uniform way
respectively. The dataset represents responses of 2,000 subjects to
multiple-choice test of 20 items. A correct answer is coded as 1 and
incorrect answer as 0. The column `group` represents group membership,
where 0 indicates reference group and 1 indicates focal group. Groups
are the same size (i.e. 1,000 per group). The distributions of total
scores (sum of correct answers) are the same for both reference and
focal group (Martinkova et al., 2017). The column `criterion` represents
generated continuous variable which is intended to be predicted by test.

## Usage

``` r
GMAT
```

## Format

A `GMAT` data frame consists of 2,000 observations on the following 22
variables:

- Item1-Item20:

  dichotomously scored items of the test

- group:

  group membership vector, `"0"` reference group, `"1"` focal group

- criterion:

  continuous critetion intended to be predicted by test

## Source

Reexport from `difNLR` package.

## References

Kingston, N., Leary, L., & Wightman, L. (1985). An exploratory study of
the applicability of item response theory methods to the Graduate
Management Admission Test. ETS Research Report Series, 1985(2): 1–64.

Martinkova, P., Drabinova, A., Liaw, Y. L., Sanders, E. A., McFarland,
J. L., & Price, R. M. (2017). Checking equity: Why differential item
functioning analysis should be a routine part of developing conceptual
assessments. CBE–Life Sciences Education, 16(2), rm2,
[doi:10.1187/cbe.16-10-0307](https://doi.org/10.1187/cbe.16-10-0307) .

## Author

Adela Hladka (nee Drabinova)  
Institute of Computer Science of the Czech Academy of Sciences  
Faculty of Mathematics and Physics, Charles University  
<hladka@cs.cas.cz>  

Patricia Martinkova  
Institute of Computer Science of the Czech Academy of Sciences  
<martinkova@cs.cas.cz>  
