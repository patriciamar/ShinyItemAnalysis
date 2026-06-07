# Recognize and recode not-reached responses

`recode_nr()` function recognizes and recodes not-reached responses,
i.e., missing responses to items such that all subsequent items are
missed as well by the respondent.

## Usage

``` r
recode_nr(Data, nr_code = 99, df)
```

## Arguments

- Data:

  matrix or data.frame: object to be recoded, must include only items
  columns and no additional information

- nr_code:

  single character, integer or numeric: specifying how should be
  recognized not-reached responses coded (default is `99`)

- df:

  deprecated. Use argument `Data` instead.

## Value

A `data.frame` object.

## See also

[`ItemAnalysis()`](ItemAnalysis.md)

## Author

Jan Netik  
Institute of Computer Science of the Czech Academy of Sciences  
<netik@cs.cas.cz>

Patricia Martinkova  
Institute of Computer Science of the Czech Academy of Sciences  
<martinkova@cs.cas.cz>  

## Examples

``` r
HCImissed <- HCI[, 1:20]

# simulate skipped (missed) and not-reached items in HCI dataset
set.seed(4211)
for (i in 1:150) {
  # not-reached (minimum at 10th item, maximum at 20th)
  HCImissed[sample(1:nrow(HCImissed), 1), seq(sample(10:20, 1), 20)] <- NA

  # missed with random location
  HCImissed[sample(1:nrow(HCImissed), 1), sample(1:20, 1)] <- NA
}

summary(HCImissed)
#>      Item 1           Item 2           Item 3           Item 4      
#>  Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000  
#>  1st Qu.:0.0000   1st Qu.:1.0000   1st Qu.:1.0000   1st Qu.:0.0000  
#>  Median :1.0000   Median :1.0000   Median :1.0000   Median :0.0000  
#>  Mean   :0.7003   Mean   :0.7527   Mean   :0.8483   Mean   :0.4006  
#>  3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:1.0000  
#>  Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000  
#>  NAs    :7        NAs    :4        NAs    :5        NAs    :7       
#>      Item 5          Item 6           Item 7           Item 8      
#>  Min.   :0.000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000  
#>  1st Qu.:0.000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000  
#>  Median :0.000   Median :0.0000   Median :1.0000   Median :1.0000  
#>  Mean   :0.441   Mean   :0.3643   Mean   :0.5488   Mean   :0.7072  
#>  3rd Qu.:1.000   3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:1.0000  
#>  Max.   :1.000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000  
#>  NAs    :7       NAs    :6        NAs    :6        NAs    :9       
#>      Item 9          Item 10          Item 11          Item 12      
#>  Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000  
#>  1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:1.0000   1st Qu.:0.0000  
#>  Median :0.0000   Median :1.0000   Median :1.0000   Median :1.0000  
#>  Mean   :0.4292   Mean   :0.6493   Mean   :0.7731   Mean   :0.5721  
#>  3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:1.0000  
#>  Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000  
#>  NAs    :8        NAs    :18       NAs    :34       NAs    :41      
#>     Item 13          Item 14          Item 15          Item 16      
#>  Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000  
#>  1st Qu.:0.0000   1st Qu.:1.0000   1st Qu.:0.0000   1st Qu.:0.0000  
#>  Median :1.0000   Median :1.0000   Median :0.0000   Median :1.0000  
#>  Mean   :0.5933   Mean   :0.7504   Mean   :0.4464   Mean   :0.5882  
#>  3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:1.0000  
#>  Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000  
#>  NAs    :51       NAs    :62       NAs    :73       NAs    :90      
#>     Item 17          Item 18          Item 19          Item 20      
#>  Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000  
#>  1st Qu.:0.0000   1st Qu.:1.0000   1st Qu.:1.0000   1st Qu.:0.0000  
#>  Median :0.0000   Median :1.0000   Median :1.0000   Median :1.0000  
#>  Mean   :0.3039   Mean   :0.7943   Mean   :0.7814   Mean   :0.7095  
#>  3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:1.0000  
#>  Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000  
#>  NAs    :108      NAs    :121      NAs    :125      NAs    :145     

HCImissedNR <- recode_nr(HCImissed, nr_code = 99)
head(HCImissedNR)
#>   Item 1 Item 2 Item 3 Item 4 Item 5 Item 6 Item 7 Item 8 Item 9 Item 10
#> 1      1      1      1      1      1      0      0      1      1       1
#> 2      1      1      1      1      1      1      0      1      1       1
#> 3      1      1      1      1      0      1      0      1      1       1
#> 4      1      1      1      1      1      1      1      1      1       1
#> 5      1      1      1      1      1      1      1      1      1       1
#> 6      1      1      1      1      1      1      1      1      1       1
#>   Item 11 Item 12 Item 13 Item 14 Item 15 Item 16 Item 17 Item 18 Item 19
#> 1       1       1       1      99      99      99      99      99      99
#> 2       1       1       1       1       1       1       1       1       1
#> 3       1       1       1       1       0       1       1       1       1
#> 4       1       1       1       1       1      NA       1       1       1
#> 5       1       1       1       1       0       1       1       1       1
#> 6       1       1      99      99      99      99      99      99      99
#>   Item 20
#> 1      99
#> 2       1
#> 3       1
#> 4       1
#> 5       1
#> 6      99
summary(HCImissedNR)
#>      Item 1           Item 2           Item 3           Item 4      
#>  Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000  
#>  1st Qu.:0.0000   1st Qu.:1.0000   1st Qu.:1.0000   1st Qu.:0.0000  
#>  Median :1.0000   Median :1.0000   Median :1.0000   Median :0.0000  
#>  Mean   :0.7003   Mean   :0.7527   Mean   :0.8483   Mean   :0.4006  
#>  3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:1.0000  
#>  Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000  
#>  NAs    :7        NAs    :4        NAs    :5        NAs    :7       
#>      Item 5          Item 6           Item 7           Item 8      
#>  Min.   :0.000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000  
#>  1st Qu.:0.000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000  
#>  Median :0.000   Median :0.0000   Median :1.0000   Median :1.0000  
#>  Mean   :0.441   Mean   :0.3643   Mean   :0.5488   Mean   :0.7072  
#>  3rd Qu.:1.000   3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:1.0000  
#>  Max.   :1.000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000  
#>  NAs    :7       NAs    :6        NAs    :6        NAs    :9       
#>      Item 9           Item 10          Item 11          Item 12      
#>  Min.   : 0.0000   Min.   : 0.000   Min.   : 0.000   Min.   : 0.000  
#>  1st Qu.: 0.0000   1st Qu.: 0.000   1st Qu.: 1.000   1st Qu.: 0.000  
#>  Median : 0.0000   Median : 1.000   Median : 1.000   Median : 1.000  
#>  Mean   : 0.5823   Mean   : 2.479   Mean   : 5.183   Mean   : 5.624  
#>  3rd Qu.: 1.0000   3rd Qu.: 1.000   3rd Qu.: 1.000   3rd Qu.: 1.000  
#>  Max.   :99.0000   Max.   :99.000   Max.   :99.000   Max.   :99.000  
#>  NAs    :7         NAs    :6        NAs    :5        NAs    :8       
#>     Item 13          Item 14         Item 15         Item 16    
#>  Min.   : 0.000   Min.   : 0.00   Min.   : 0.00   Min.   : 0.0  
#>  1st Qu.: 0.000   1st Qu.: 1.00   1st Qu.: 0.00   1st Qu.: 0.0  
#>  Median : 1.000   Median : 1.00   Median : 1.00   Median : 1.0  
#>  Mean   : 7.742   Mean   : 9.42   Mean   :10.55   Mean   :13.4  
#>  3rd Qu.: 1.000   3rd Qu.: 1.00   3rd Qu.: 1.00   3rd Qu.: 1.0  
#>  Max.   :99.000   Max.   :99.00   Max.   :99.00   Max.   :99.0  
#>  NAs    :4        NAs    :5       NAs    :7       NAs    :6     
#>     Item 17         Item 18         Item 19         Item 20    
#>  Min.   : 0.00   Min.   : 0.00   Min.   : 0.00   Min.   : 0.0  
#>  1st Qu.: 0.00   1st Qu.: 1.00   1st Qu.: 1.00   1st Qu.: 1.0  
#>  Median : 0.00   Median : 1.00   Median : 1.00   Median : 1.0  
#>  Mean   :15.39   Mean   :17.67   Mean   :19.15   Mean   :22.6  
#>  3rd Qu.: 1.00   3rd Qu.: 1.00   3rd Qu.: 1.00   3rd Qu.: 1.0  
#>  Max.   :99.00   Max.   :99.00   Max.   :99.00   Max.   :99.0  
#>  NAs    :10      NAs    :11      NAs    :4                     
```
