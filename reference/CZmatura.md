# CZmatura dataset

The `CZmatura` dataset comes from matura exam in mathematics. The exam
was assigned in 2019 to students from Grade 13, at the end of their
secondary education. Original data available from
<https://cermat.gov.cz/>.

## Usage

``` r
CZmatura
```

## Format

`CZmatura` is a `data.frame` consisting of 15,702 observations on 75
variables.

- SchType:

  School type code.

- FirstAtt:

  First attempt; `"1"` yes, `"0"` no.

- SchTypeGY:

  School type gymnasium; `"1"` yes, `"0"` no.

- o1 – o26.2:

  Item answers.

- b1 – b26:

  Scored item answers.

- Total:

  Total score, calculated as sum of item scores (0 - 50).

- IRTscore:

  Score estimated from GPCM/2PL model.

- IRTscoreSE:

  SE of score estimated from GPCM/2PL model.

## See also

[`CZmaturaS()`](CZmaturaS.md)
