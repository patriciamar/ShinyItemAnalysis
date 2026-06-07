# Plot Method for Parallel Analysis Output

You can call this method to plot an existing object resulting from
`fa_paralell()` function, which behaves as a standard `data.frame`, but
can be automatically recognized and processed with a dedicated plot
method. Also, you can *post-hoc* disable the Kaiser boundaries shown by
default.

## Usage

``` r
# S3 method for class 'sia_parallel'
plot(x, y, ...)
```

## Arguments

- x:

  object of class `sia_parallel` to plot.

- y:

  *ignored*

- ...:

  additional argument:

## Examples

``` r
if (FALSE) { # \dontrun{
fa_parallel_result <- BFI2[, 1:60] |> fa_parallel(plot = FALSE) # without plot
fa_parallel_result |> plot() # generate plot from "fitted" object
fa_parallel_result |> plot(show_kaiser = FALSE) # hide Kaiser boundaries
} # }
```
