# CZmatura dataset - sample

The `CZmaturaS` dataset comes from a matura exam in mathematics. The
exam was assigned in 2019 to students in Grade 13, at the end of their
secondary education. This is a random sample of 2,000 students from a
total of 15,702. Original data available from <https://cermat.gov.cz/>.

## Usage

``` r
CZmaturaS
```

## Format

`CZmatura` is a `data.frame` consisting of 2,000 observations on 75
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

[`CZmatura()`](CZmatura.md)
