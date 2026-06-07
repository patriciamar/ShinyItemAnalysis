# Plot person-item map (Wright map) using `ggplot2`

This function allows to generate Wright map (also called person-item
map) using `ggplot()` function from the ggplot2 package. Wright map is
used to jointly display histogram of abilities (or other measured trait)
and item difficulty parameters. Function takes pre-estimated parameter
estimates, such as those obtained from an IRT model.

## Usage

``` r
ggWrightMap(
  theta,
  b,
  binwidth = 0.5,
  color = "blue",
  size = 15,
  item.names,
  ylab.theta = "Respondent latent trait",
  ylab.b = "Item difficulty",
  rel_widths = c(1, 1)
)
```

## Arguments

- theta:

  numeric: vector of ability estimates.

- b:

  numeric: vector of difficulty estimates.

- binwidth:

  numeric: the width of the bins of histogram.

- color:

  character: color of histogram.

- size:

  text size in pts.

- item.names:

  names of items to be displayed.

- ylab.theta:

  character: description of y-axis for the histogram.

- ylab.b:

  character: description of y-axis for the plot of difficulty estimates.

- rel_widths:

  numeric: vector of length 2 specifying ratio of "facet's" widths.

## References

Wright, B. & Stone, M. (1979). Best test design. MESA Press: Chicago, IL

## Author

Adela Hladka  
Institute of Computer Science of the Czech Academy of Sciences  
<hladka@cs.cas.cz>

Jan Netik  
Institute of Computer Science of the Czech Academy of Sciences  
<netik@cs.cas.cz>

Patricia Martinkova  
Institute of Computer Science of the Czech Academy of Sciences  
<martinkova@cs.cas.cz>

## Examples

``` r
library(mirt)
#> Loading required package: stats4
#> Loading required package: lattice

# fit Rasch model with the mirt package
fit <- mirt(HCI[, 1:20], model = 1, itemtype = "Rasch")
# factor scores
theta <- as.vector(fscores(fit))
# difficulty estimates using IRT parametrization
b <- coef(fit, simplify = TRUE, IRTpars = TRUE)$items[, "b"]

# Wright map
ggWrightMap(theta, b)


# Wright map with modified item names
item.names <- paste("Item", 1:20)
ggWrightMap(theta, b, item.names = item.names)


# Wright map with modified descriptions of y-axis and relative widths of plots
ggWrightMap(theta, b,
  ylab.theta = "Latent trait", ylab.b = "Difficulty estimates",
  rel_widths = c(2, 1)
)
```
