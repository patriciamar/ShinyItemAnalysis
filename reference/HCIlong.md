# Homeostasis Concept Inventory in a long format

`HCIlong` dataset consists of the dichotomously scored responses of 651
students (405 males, 246 females) to Homeostasis Concept Inventory (HCI)
multiple-choice test. It contains 20 items (**in a long format**),
vector of gender membership and identificator whether students plan to
major in life sciences.

## Usage

``` r
HCIlong
```

## Format

`HCIlong` is a `data.frame` consisting of 13,020 rows and 5 variables.

- id:

  Row number of the original observation in a wide format.

- item:

  Name of the item the rating is for.

- rating:

  Response to the item.

- gender:

  Gender membership, `"0"` males, `"1"` females.

- major:

  Identificator whether student plans to major in the life sciences.

- total:

  Total score

- zscore:

  Standardized total score (Z-score)

## References

McFarland, J. L., Price, R. M., Wenderoth, M. P., Martinkova, P., Cliff,
W., Michael, J., ... & Wright, A. (2017). Development and validation of
the homeostasis concept inventory. CBE-Life Sciences Education, 16(2),
ar35.
[doi:10.1187/cbe.16-10-0305](https://doi.org/10.1187/cbe.16-10-0305)

## See also

[HCI](HCI.md) for HCI dichotomous dataset (in a wide format)  
[HCItest](HCItest.md) for HCI multiple-choice dataset  
[HCIkey](HCIkey.md) for key of correct answers for HCI  
[HCIdata](HCIdata.md) for HCI full dataset  
[HCIgrads](HCIgrads.md) for HCI dataset of graduate students  
[HCIprepost](HCIprepost.md) for HCI pretest and posttest scores  
[HCItestretest](HCItestretest.md) for HCI test-retest dataset  

## Author

Jenny L. McFarland  
Biology Department, Edmonds Community College
