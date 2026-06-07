# Height inventory dataset

`HeightInventory` dataset consists of the responses of 4,885 respondents
(1479 males, 3406 females) to a Height Inventory (Rečka, 2018). It
contains 26 ordinal items of self-perceived height rated on a scale
`"1"` strongly disagree, `"2"` disagree, `"3"` agree, `"4"` strongly
agree, vector of self-reported heights (in centimeters), and vector of
gender membership. Total score is included as the last variable, total
score is NA for respondents who missed any item.

## Usage

``` r
HeightInventory
```

## Format

`HeightInventory` is a `data.frame` consisting of 4,885 observations on
the 28 variables. First 26 variables are responses on scale `"1"`
strongly disagree, `"2"` disagree, `"3"` agree, `"4"` strongly agree.
Items 14 - 26 were reverse-coded, so that all items are scored in the
same direction. Names of these items start with `"R-"`. Original item
number and English wording is provided below.

- ShortTrousers:

  1\. A lot of trousers are too short for me.

- TallerThanM:

  2\. I am taller than men of my age.

- TallerThanF:

  3\. I am taller than women of my age.

- HeightForBasketball:

  4\. I have an appropriate height for playing basketball or volleyball.

- AskMeToReach:

  5\. Other people sometimes ask me to reach something for them.

- CommentsTall:

  6\. I am used to hearing comments about how tall I am.

- ConcertObstructs:

  7\. At concerts, my stature usually obstructs other people’s views.

- ShortBed:

  8\. Ordinary beds are too short for me.

- TopShelfEasy:

  9\. I can easily take wares from top shelves at a store.

- CrowdViewComf:

  10\. In a crowd of people, I still have a comfortable view.

- ShortBlanket:

  11\. Blankets and bedspreads rarely cover me completely.

- BendToHug:

  12\. When I want to hug someone, I usually need to bend over.

- CarefulHead:

  13\. I must often be careful to avoid bumping my head against a
  doorjamb or a low ceiling.

- R-SmallerThanM:

  14\. I am smaller than men of my age. (reversed)

- R-StoolNeeded:

  15\. I often need a stool to reach something other people could reach
  without one. (reversed)

- R-PlayDwarf:

  16\. I could play a dwarf. (reversed)

- R-SmallerThanW:

  17\. I am smaller than women of my age. (reversed)

- R-NoticeSmall:

  18\. One of the first things people notice about me is how small I am.
  (reversed)

- R-OnTipToes:

  19\. I often need to stand on the tip of my toes to get a better view.
  (reversed)

- R-ClothChildSize:

  20\. When I buy clothes, children’s sizes often fit me well.
  (reversed)

- R-BusLegsEnoughSpace:

  21\. I have enough room for my legs when traveling by bus. (reversed)

- R-FasterWalk:

  22\. I often need to walk faster than I’m used to in order to keep
  pace with taller people. (reversed)

- R-AgeUnderestim:

  23\. Because of my smaller stature, people underestimate my age.
  (reversed)

- R-WishLowerChair:

  24\. It would be more comfortable for me if chairs were made lower.
  (reversed)

- R-UpwardLook:

  25\. When talking to other adults, I have to look upwards if I want to
  meet their eyes. (reversed)

- R-MirrorTooHigh:

  26\. Some mirrors are placed so high up that I have to crane my neck
  to use them. (reversed)

- gender:

  Gender membership, `"M"` males, `"F"` females.

- HeightCM:

  Self-reported height in centimeters.

- total:

  Total score.

## Note

Thanks to Karel Rečka and Hynek Cígler for sharing this dataset.

## References

Rečka, K. (2018). Height and Weight Inventory. Brno, Masaryk University:
Unpublished Master's thesis
