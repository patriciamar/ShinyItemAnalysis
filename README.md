<div><h1 style = "display: inline-block">ShinyItemAnalysis</h1><img style = "float: right; display: inline-block" src="inst/shiny-examples/ShinyItemAnalysis/hexbin_html.png" width="110"></img></div>

Test and item analysis via shiny

![GHversion](https://img.shields.io/github/release/patriciamar/ShinyItemAnalysis.svg)
[![online](https://img.shields.io/badge/online-1.2.6-yellow.svg)](https://shiny.cs.cas.cz/ShinyItemAnalysis/)
[![version](https://www.r-pkg.org/badges/version/ShinyItemAnalysis)](https://CRAN.R-project.org/package=ShinyItemAnalysis)
![cranlogs](https://cranlogs.r-pkg.org/badges/ShinyItemAnalysis)


## Overview
`ShinyItemAnalysis` is an interactive shiny application for analysis of educational tests and their items including 
 * exploration of total and standard scores,
 * item and distractor analysis,
 * item analysis via logistic regression models and their extensions,
 * item analysis via IRT models,
 * training plots for dichotomous and polytomous IRT models,
 * DIF and DDF detection methods.
 
It also offers some training data sets but you can also upload your own data. Moreover it is also possible to generate reports. 

`ShinyItemAnalysis` is available online at [Czech Academy of Sciences](https://shiny.cs.cas.cz/ShinyItemAnalysis/) and [shinyapps.io](cemp.shinyapps.io/ShinyItemAnalysis/). It can be also downloaded from [**CRAN**](https://CRAN.R-project.org/package=ShinyItemAnalysis).

## Installation
```
# The easiest way to get ShinyItemAnalysis is to install from CRAN:
install.packages("ShinyItemAnalysis")

# Or you can get the newest development version from GitHub:
# install.packages("devtools")
devtools::install_github("patriciamar/ShinyItemAnalysis")
```
## Version
Current version available on [**CRAN**](https://CRAN.R-project.org/package=ShinyItemAnalysis) is 1.2.6. <br> 
Version available online at [Czech Academy of Sciences](https://shiny.cs.cas.cz/ShinyItemAnalysis/) and [shinyapps.io](cemp.shinyapps.io/ShinyItemAnalysis/) is 1.2.6. <br> 
The newest development version available on [**GitHub**](https://github.com/patriciamar/ShinyItemAnalysis) is 1.2.6.

## Usage
It's very easy to run `ShinyItemAnalysis`:
```
rm(list = ls())
startShinyItemAnalysis()
```
Or try it directly online at [Czech Academy of Sciences](https://shiny.cs.cas.cz/ShinyItemAnalysis/) or [shinyapps.io](cemp.shinyapps.io/ShinyItemAnalysis/)!

## Getting help
If you find any bug or just need help with `ShinyItemAnalysis` you can leave your message as an issue [here](https://github.com/patriciamar/ShinyItemAnalysis/issues) or directly contact us at martinkova@cs.cas.cz

## License
This program is free software and you can redistribute it and or modify it under the terms of the [GNU GPL 3](https://www.gnu.org/licenses/gpl-3.0.en.html).

## References
For Czech speakers new paper is available [online!](http://testforum.cz/domains/testforum.cz/index.php/testforum/article/view/TF2017-9-129/146#.WZvuzyhJa70)
