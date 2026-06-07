# AIBS grant peer review scoring dataset

The `AIBS` dataset (Gallo, 2020) comes from the scientific peer review
facilitated by the American Institute of Biological Sciences (AIBS) of
biomedical applications from and intramural collaborative biomedical
research program for 2014–2017. For each proposal, three assigned
individual reviewers were asked to provide scores and commentary for the
following application criteria: Innovation, Approach/Feasibility,
Investigator, and Significance (Impact added as scored criterion in
2014). Each of these criteria is scored on a scale from 1.0 (best) to
5.0 (worst) with a 0.1 gradation, as well as an overall score (1.0–5.0
with a 0.1 gradation). Asynchronous discussion was allowed, although few
scores changed post-discussion. The data includes reviewers'
self-reported expertise scores (1/2/3, 1 is high expertise) relative to
each proposal reviewed, and reviewer / principal investigator
demographics. A total of 72 applications ("Standard" or "Pilot") were
reviewed in 3 review cycles. The success rate was 34–38 %. Application
scores indicate where each application falls among all practically
possible applications in comparison with the ideal standard of quality
from a perfect application. The dataset was used by Erosheva et al.
(2021a) to demonstrate issues of inter-rater reliability in case of
restricted samples. For details, see Erosheva et al. (2021b).

## Usage

``` r
AIBS
```

## Format

`AIBS` is a `data.frame` consisting of 216 observations on 25 variables.
Data describes 72 proposals with 3 ratings each.

- ID:

  Proposal ID.

- Year:

  Year of the review.

- PropType:

  Proposal type; `"Standard"` or `"Pilot"`.

- PIID:

  Anonymized ID of principal investigator (PI).

- PIOrgType:

  PI's organization type.

- PIGender:

  PI's gender membership; `"1"` females, `"2"` males.

- PIRank:

  PI's rank; `"3"` full professor, `"1"` assistant professor.

- PIDegree:

  PI's degree; `"1"` PhD, `"2"` MD, `"3"` PhD/MD.

- Innovation:

  Innovation score.

- Approach:

  Approach score.

- Investig:

  Investigator score.

- Signif:

  Significance score.

- Impact:

  Impact score.

- Score:

  Scientific merit (overall) score.

- ScoreAvg:

  Average of the three overall scores from three different reviewers.

- ScoreAvgAdj:

  Average of the three overall scores from three different reviewers,
  increased by multiple of 0.001 of the worst score.

- ScoreRank:

  Project rank calculated based on `ScoreAvg`.

- ScoreRankAdj:

  Project rank calculated based on `ScoreAvgAdj`.

- RevID:

  Reviewer's ID.

- RevExp:

  Reviewer's experience.

- RevInst:

  Reviewer's institution; `"1"` academia, `"2"` government.

- RevGender:

  Reviewer's gender; `"1"` females, `"2"` males.

- RevRank:

  Reviewer's rank; `"3"` full professor, `"1"` assistant professor.

- RevDegree:

  Reviewer's degree; `"1"` PhD, `"2"` MD, `"3"` PhD/MD.

- RevCode:

  Reviewer code (`"A"`, `"B"`, `"C"`) in the original wide dataset.

## References

Gallo, S. (2021). Grant peer review scoring data with criteria scores.
[doi:10.6084/m9.figshare.12728087](https://doi.org/10.6084/m9.figshare.12728087)

Erosheva, E., Martinkova, P., & Lee, C. (2021a). When zero may not be
zero: A cautionary note on the use of inter-rater reliability in
evaluating grant peer review. Journal of the Royal Statistical Society -
Series A. [doi:10.1111/rssa.12681](https://doi.org/10.1111/rssa.12681)

Erosheva, E., Martinkova, P., & Lee, C. (2021b). Supplementary material:
When zero may not be zero: A cautionary note on the use of inter-rater
reliability in evaluating grant peer review.
[doi:10.17605/OSF.IO/KNPH8](https://doi.org/10.17605/OSF.IO/KNPH8)

## See also

[`ICCrestricted()`](ICCrestricted.md)

## Author

Stephen Gallo  
American Institute of Biological Sciences
