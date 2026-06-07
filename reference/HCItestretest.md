# Homeostasis concept inventory test-retest dataset

`HCItestretest` dataset consists of the responses of 45 students to
Homeostasis Concept Inventory (HCI). It contains answers to 20
multiple-choice items, scored items, identifier of test/retest, total
score, gender membership and identifier whether students plan to major
in life sciences. The data are organized so that each pair of subsequent
rows belongs to one student. Students took no courses on homeostasis
between the test and retest.

## Usage

``` r
HCItestretest
```

## Format

`HCItestretest` is a `data.frame` consisting of 90 observations on the
44 variables.

- A1-A20:

  Multiple-choice items of the HCI test.

- QR1-QR20:

  Scored items of the HCI test, `"0"` incorrect, `"1"` correct.

- test:

  Identifier of test vs retest, `"test"` test, `"retest"` retest after.

- total:

  Total test score.

- gender:

  Gender membership, `"M"` male, `"F"` female.

- major:

  Identifier whether student plans to major in the life sciences.

## References

McFarland, J. L., Price, R. M., Wenderoth, M. P., Martinkova, P., Cliff,
W., Michael, J., ... & Wright, A. (2017). Development and validation of
the homeostasis concept inventory. CBE-Life Sciences Education, 16(2),
ar35.
[doi:10.1187/cbe.16-10-0305](https://doi.org/10.1187/cbe.16-10-0305)

## See also

[HCI](HCI.md) for HCI dichotomous dataset  
[HCItest](HCItest.md) for HCI multiple-choice dataset  
[HCIkey](HCIkey.md) for key of correct answers for HCI  
[HCIdata](HCIdata.md) for HCI full dataset  
[HCIlong](HCIlong.md) for HCI in a long format  
[HCIgrads](HCIgrads.md) for HCI dataset of graduate students  
[HCIprepost](HCIprepost.md) for HCI pretest and posttest scores  

## Author

Jenny L. McFarland  
Biology Department, Edmonds Community College
