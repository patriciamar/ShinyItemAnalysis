# ShinyItemAnalysis: Test and Item Analysis via Shiny

The `ShinyItemAnalysis` package contains an interactive Shiny
application for the psychometric analysis of educational tests,
psychological assessments, health-related and other types of multi-item
measurements, or ratings from multiple raters, which can be accessed
using function [`startShinyItemAnalysis()`](startShinyItemAnalysis.md).
The shiny application covers a broad range of psychometric methods and
offers data examples, model equations, parameter estimates,
interpretation of results, together with a selected R code, and is
therefore suitable for teaching psychometric concepts with R. It also
allows the users to upload and analyze their own data and to
automatically generate analysis reports in PDF or HTML.

Besides, the package provides its own functions for test and item
analysis within classical test theory framework (e.g., functions
[`gDiscrim()`](gDiscrim.md), [`ItemAnalysis()`](ItemAnalysis.md),
[`DistractorAnalysis()`](DistractorAnalysis.md), or
[`DDplot()`](DDplot.md)), using various regression models (e.g.,
[`plotCumulative()`](plotCumulative.md),
[`plotAdjacent()`](plotAdjacent.md),
[`plotMultinomial()`](plotMultinomial.md), or
[`plotDIFLogistic()`](plotDIFLogistic.md)), and under IRT framework
(e.g., [`ggWrightMap()`](ggWrightMap.md), or
[`plotDIFirt()`](plotDIFirt.md)).

Package also contains several demonstration datasets including the `HCI`
dataset from the book by Martinkova and Hladka (2023), and from paper by
Martinkova and Drabinova (2018).

## Functions

- [`startShinyItemAnalysis()`](startShinyItemAnalysis.md)

- [`DDplot()`](DDplot.md)

- [`DistractorAnalysis()`](DistractorAnalysis.md)

- [`plotDistractorAnalysis()`](plotDistractorAnalysis.md)

- [`fa_parallel()`](fa_parallel.md)

- [`gDiscrim()`](gDiscrim.md)

- [`ggWrightMap()`](ggWrightMap.md)

- [`ICCrestricted()`](ICCrestricted.md)

- [`ItemAnalysis()`](ItemAnalysis.md)

- [`blis()`](fit_blis.md)

- [`plotAdjacent()`](plotAdjacent.md),
  [`plotCumulative()`](plotCumulative.md),
  [`plotMultinomial()`](plotMultinomial.md)

- [`plotDIFirt()`](plotDIFirt.md),
  [`plotDIFLogistic()`](plotDIFLogistic.md)

- [`plot_corr()`](plot_corr.md)

- [`recode_nr()`](recode_nr.md)

## Datasets

- [`AIBS()`](AIBS.md)

- [`Anxiety()`](Anxiety.md)

- [`AttitudesExpulsion()`](AttitudesExpulsion.md)

- [`BFI2()`](BFI2.md)

- [`CLoSEread6()`](CLoSEread6.md)

- [`CZmatura()`](CZmatura.md)

- [`CZmaturaS()`](CZmaturaS.md)

- [`dataMedical()`](dataMedical.md)

- [`dataMedicalgraded()`](dataMedicalgraded.md)

- [`dataMedicalkey()`](dataMedicalkey.md)

- [`dataMedicaltest()`](dataMedicaltest.md)

- [`HCI()`](HCI.md)

- [`HCIdata()`](HCIdata.md)

- [`HCIgrads()`](HCIgrads.md)

- [`HCIkey()`](HCIkey.md)

- [`HCIlong()`](HCIlong.md)

- [`HCIprepost()`](HCIprepost.md)

- [`HCItest()`](HCItest.md)

- [`HCItestretest()`](HCItestretest.md)

- [`HeightInventory()`](HeightInventory.md)

- [`LearningToLearn()`](LearningToLearn.md)

- [`MSATB()`](MSATB.md)

- [`MSclinical()`](MSclinical.md)

- [`NIH()`](NIH.md)

- [`TestAnxietyCor()`](TestAnxietyCor.md)

## References

Martinkova, P., & Hladka, A. (2023). Computational Aspects of
Psychometric Methods: With R. Chapman and Hall/CRC.
[doi:10.1201/9781003054313](https://doi.org/10.1201/9781003054313)

Martinkova, P., & Drabinova, A. (2018). ShinyItemAnalysis for teaching
psychometrics and to enforce routine analysis of educational tests. The
R Journal, 10(2), 503–515,
[doi:10.32614/RJ-2018-074](https://doi.org/10.32614/RJ-2018-074)

## See also

Useful links:

- <https://shinyitemanalysis.org/>

- Report bugs at
  <https://github.com/patriciamar/ShinyItemAnalysis/issues>

## Author

Patricia Martinkova  
Institute of Computer Science of the Czech Academy of Sciences  
Faculty of Education, Charles University  
<martinkova@cs.cas.cz>

Adela Hladka (nee Drabinova)  
Institute of Computer Science of the Czech Academy of Sciences

Jan Netik  
Institute of Computer Science of the Czech Academy of Sciences  
