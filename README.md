# ShinyItemAnalysis <img src="inst/shiny-examples/ShinyItemAnalysis/hexbin_alpha.png" align="right" width=150/> 
Test and item analysis via shiny

![GHversion](https://img.shields.io/github/release/patriciamar/ShinyItemAnalysis.svg)
[![online](https://img.shields.io/badge/online-1.3.7-yellow.svg)](https://shiny.cs.cas.cz/ShinyItemAnalysis/)
[![version](https://www.r-pkg.org/badges/version/ShinyItemAnalysis)](https://CRAN.R-project.org/package=ShinyItemAnalysis)
![cranlogs](https://cranlogs.r-pkg.org/badges/ShinyItemAnalysis)

## Overview
`ShinyItemAnalysis` is an R package including functions and interactive shiny application for the psychometric analysis of educational tests,
    psychological assessments, health-related and other types of multi-item measurements, or ratings from multiple raters. 
    Offered methods include:

 * Exploration of total and standard scores
 * Analysis of measurement error and reliability
 * Analysis of correlation structure and validity
 * Traditional item analysis
 * Item analysis with regression models
 * Item analysis with IRT models
 * Detection of differential item functioning
 
Number of toy datasets is available, the interactive application also allows the users to upload and analyze their own data and to automatically generate PDF or HTML reports.

`ShinyItemAnalysis` is available online at [Czech Academy of Sciences](https://shiny.cs.cas.cz/ShinyItemAnalysis/) and [shinyapps.io](https://cemp.shinyapps.io/ShinyItemAnalysis/). It can be also downloaded from [**CRAN**](https://CRAN.R-project.org/package=ShinyItemAnalysis). Visit our [**web page**](http://www.shinyitemanalysis.org/) about ShinyItemAnalysis to learn more!

## Installation
```
# The easiest way to get ShinyItemAnalysis is to install from CRAN:
install.packages("ShinyItemAnalysis")

# Or you can get the newest development version from GitHub:
# install.packages("devtools")
devtools::install_github("patriciamar/ShinyItemAnalysis")
```
## Version
Current version available on [**CRAN**](https://CRAN.R-project.org/package=ShinyItemAnalysis) is 1.3.7. 
The newest development version available on [**GitHub**](https://github.com/patriciamar/ShinyItemAnalysis) is 1.3.7.<br> 
Version available online at [Czech Academy of Sciences](https://shiny.cs.cas.cz/ShinyItemAnalysis/) is 1.3.7. 
Version available online at [shinyapps.io](https://cemp.shinyapps.io/ShinyItemAnalysis/) is 1.3.7. <br> 

## Usage
It is very easy to run `ShinyItemAnalysis` in `R`:
```
library(ShinyItemAnalysis)
startShinyItemAnalysis()
```
Or try it directly online at [Czech Academy of Sciences](https://shiny.cs.cas.cz/ShinyItemAnalysis/) or [shinyapps.io](https://cemp.shinyapps.io/ShinyItemAnalysis/)!

## References
When using `ShinyItemAnalysis` software, we appreciate if you include a reference in your publications. To cite the software, please, use: 

  Martinková P., & Drabinová A. (2018) ShinyItemAnalysis for teaching psychometrics and to enforce routine analysis of educational tests. The R Journal, 10(2), 503-515.
[doi: 10.32614/RJ-2018-074](https://doi.org/10.32614/RJ-2018-074). 

Czech speakers can also refer to paper in journal [Testforum](https://doi.org/10.5817/TF2017-9-129).

## Getting help and provide feedback
If you find any bug or just need help with `ShinyItemAnalysis` you can leave your message as an issue [here](https://github.com/patriciamar/ShinyItemAnalysis/issues) or directly contact us at martinkova@cs.cas.cz. We warmly encourage you to provide your feedback using [Google form](https://docs.google.com/forms/d/e/1FAIpQLSdbk2mkDacMlhGQmkFPa4A-Z4KcFMMG1IXugM8eSTzN7m4xnA/viewform).

## License
This program is free software and you can redistribute it and or modify it under the terms of the [GNU GPL 3](https://www.gnu.org/licenses/gpl-3.0.en.html).

