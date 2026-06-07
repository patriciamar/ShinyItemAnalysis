# Homeostasis concept inventory full dataset

`HCIdata` dataset consists of the responses of 669 students (405 males,
246 females, 18 without gender specification) to Homeostasis Concept
Inventory (HCI) multiple-choice test. It contains answers to 20
multiple-choice items, scored items, total score, gender membership,
identifier whether students plan to major in science, study year,
minority membership, identifier whether English is the student's first
language, and type of school.

## Usage

``` r
HCIdata
```

## Format

`HCIdata` is a `data.frame` consisting of 669 observations on the 47
variables.

- A1-A20:

  Multiple-choice items of the HCI test.

- QR1-QR20:

  Scored items of the HCI test, `"0"` incorrect, `"1"` correct.

- total:

  Total test score.

- gender:

  Gender membership, `"M"` males, `"F"` females, `"none"` undisclosed.

- major:

  Identifier whether students plans to major in the life sciences.

- yearc5:

  Study year.

- minority:

  Minority membership, `"maj"` majority, `"min"` Black/Hispanic
  minority, `"none"` undisclosed.

- EnglishF:

  Identifier whether English is the student's first language.

- typeS:

  Course type, `"allied"` allied health, `"majors"` physiology courses
  for science majors, `"mixed majors"` courses for non-majors.

- typeSCH:

  Type of school, `"AC"` associate's college, `"BCAS"` baccalaureate
  college: arts and sciences focus, `"R1"` research university, `"MCU"`
  master's college and university.

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
[HCIlong](HCIlong.md) for HCI in a long format  
[HCIgrads](HCIgrads.md) for HCI dataset of graduate students  
[HCIprepost](HCIprepost.md) for HCI pretest and posttest scores  
[HCItestretest](HCItestretest.md) for HCI test-retest dataset  

## Author

Jenny L. McFarland  
Biology Department, Edmonds Community College
