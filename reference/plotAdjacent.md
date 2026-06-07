# Plot category probabilities of adjacent category logit model

Function for plotting category probabilities function estimated by
`vglm()` function from the `VGAM` package using the ggplot2 package.

## Usage

``` r
plotAdjacent(x, matching.name = "matching")
```

## Arguments

- x:

  object of class `vglm`

- matching.name:

  character: name of matching criterion used for estimation in `x`.

## Value

An object of class `ggplot` and/or `gg`.

## See also

[`VGAM::vglm()`](https://rdrr.io/pkg/VGAM/man/vglm.html)

## Author

Tomas Jurica  
Institute of Computer Science of the Czech Academy of Sciences  

Adela Hladka  
Institute of Computer Science of the Czech Academy of Sciences  
<hladka@cs.cas.cz>

Patricia Martinkova  
Institute of Computer Science of the Czech Academy of Sciences  
<martinkova@cs.cas.cz>

## Examples

``` r
# loading packages
library(VGAM)
#> Loading required package: splines

# loading data
data(Science, package = "mirt")

# total score calculation
score <- rowSums(Science)
Science[, 1] <- factor(Science[, 1], levels = sort(unique(Science[, 1])), ordered = TRUE)

# adjacent category logit model for item 1
fit <- vglm(Science[, 1] ~ score, family = acat(reverse = FALSE, parallel = TRUE))
# coefficients for item 1
coef(fit)
#> (Intercept):1 (Intercept):2 (Intercept):3         score 
#>    -4.2930239    -5.8226878   -10.4714049     0.7552421 

plotAdjacent(fit, matching.name = "Total score")
#> Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
#> ℹ Please use `linewidth` instead.
#> ℹ The deprecated feature was likely used in the ShinyItemAnalysis package.
#>   Please report the issue at
#>   <https://github.com/patriciamar/ShinyItemAnalysis/issues>.
```
