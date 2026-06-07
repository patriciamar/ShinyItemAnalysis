# Complete theme for `ShinyItemAnalysis` graphics

This complete theme is based on `theme_bw` and it was modified for
purposes of `ShinyItemAnalysis`.

## Usage

``` r
theme_app(base_size = 15, base_family = "")
```

## Arguments

- base_size:

  base font size

- base_family:

  base font family

## See also

[`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html)

## Examples

``` r
library(ggplot2)
data(GMAT, package = "difNLR")
data <- GMAT[, 1:20]
# total score calculation
df <- data.frame(score = apply(data, 1, sum))
# histogram
g <- ggplot(df, aes(score)) +
  geom_histogram(binwidth = 1) +
  xlab("Total score") +
  ylab("Number of respondents")

g

g + theme_app()
```
