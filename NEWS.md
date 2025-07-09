# ShinyItemAnalysis 1.5.5

## Bug fixes
  * Data upload was fixed.
  * Installation of the modules from the app now works even in `R` sessions
    without any CRAN mirror set.
    
# ShinyItemAnalysis 1.5.4 

## Major updates
  * R >= 4.1.0 is now required.
  * Modules installation is now provided in the application itself in the
    `Modules` section. This feature is only enabled when the application is
    run locally.

## Minor updates
  * ShinyItemAnalysis does no longer reexport `%>%` operator from `magrittr`
    package. The native pipe operator `|>` in used instead, internally.
  * The confidence level for Cronbach's alpha in the `Reliability` section
    is now stated.
  * If the interactive application is run locally, the visitors counter is no
    longer displayed.
  * Wording in PDF/HTML reports was edited.
    
## Bug fixes
  * Correlation test in `Item analysis / Item criterion validity` is now
    properly formatted.
  * Criterion type recognition in `Item analysis / Item criterion validity` is
    now fixed (criterion with less than 7 unique levels is considered as
    discrete variable).

# ShinyItemAnalysis 1.5.3 

## Minor updates
  * New methods are offered in  `Generalized logistic regression` in the `DIF/Fairness` section of the app. {difNLR} 
    version 1.5.0 is now required for the DIF analysis.

# ShinyItemAnalysis 1.5.2 



## Bug fixes
  * The starting values and estimation constraints structure in `fit_blis()` function 
    were fixed to work with the new version of the `mirt` package (versions 1.43 onwards).
  * Missing parts of the Logistic regression note in "DIF/Fairness -- Method comparison" tab of the app were fixed.

# ShinyItemAnalysis 1.5.1 



## Major updates
  * `run_app()` function discovers for add-on modules available on SIA repository which are not installed yet,
     and offers their installation.
  
## Minor updates
  * There are now documented options and environment variables that are consulted by `ShinyItemAnalysis` package, see `?ShinyItemAnalysis_options`.
  * In Data/Data tab, new section called Current data was added to explicitly track 
    the current data used in the app and its source.

## Bug fixes
  * The number of iterations (EM cycles) for IRT models provided in the Settings is now considered 
    in the app again.
  * The app won’t crash anymore when the user uploads nominal data without the key.

# ShinyItemAnalysis 1.5.0 



## Bug fixes
  * Calculation of itemfit p values was fixed in the Table of estimated parameters 
    in `IRT model/Dichotous models` section of the app (Thanks to Samsul Hadi for 
    reporting this issue).
  * Calculation of worst split-half coefficient was fixed to respond to updates 
    in the psych package.
  
## Major updates
  * Additional modules can now be easily incorporated into the Shiny app! 
    Shiny app discovers for installed modules and adds them in respective sections.
    Modules can be kept in external packages. See SIAmodules R package for default
    modules.
  * New dataset `CLoSEread6` was added and it is also offered as a toy dataset 
    in the `Data/Data` section of the app.
  * DIF analysis now offers combinations (simple and iterative) of purification 
    and p-value adjustment in the `DIF/Fairness` section of the app.
  
## Minor updates
  * The `plot_corr()` function is now more verbose and handles errors in polychoric 
    correlations estimation in a more user-friendly way; the partial matching of 
    arguments is no longer supported
  * Data upload and treatment of missing values was clarified and slightly updated 
    in `Data/Data` section of the app. 
  * All data are now downloadable in `Data/Data exploration` section of the app.
  * Error messages caused by missing values were made more specific in `DIF` section 
    of the app.
  * Summary table of total scores was complemented by number of complete cases 
    and explanations in the `Scores/Total scores` and `DIF/Observed scores` section 
    of the app.
  * Cutscore for hiding loadings was lowered to 0.2 in `Validity/Factor analysis`
    section of the app.
  * Interfactor correlations were added for oblique rotation and n_factors > 1 
    in the `Validity/Factor analysis` section of the app (Thanks to Stanislav 
    Ježek for suggesting this feature).
  * `HCIlong` dataset now contains `zscore` variable.
  * Some of the `HeightInventory` dataset variables were renamed, new variable 
    `total` was added.
  * Dependency on the `psychometric` package was removed.
  * `Anxiety` dataset is now offered as a toy dataset in the `Data/Data` section 
     of the app.
  * Dataset `CZmaturaS` is newly offered as a toy dataset in the `Data/Data` 
    section of the app.
  


# ShinyItemAnalysis 1.4.2 



## Bug fixes
  * Item criterion validity - Distractor plot was fixed in Item Analysis tab.

## Major updates
  * New datasets `HCIlong`, `MSATB`, `GMAT`, `EPIA` added 
  *  `anxiety` dataset was renamed to `Anxiety`, variables `score` and
     `zscore` were added 
  *  Variable `total` was added to datasets `HCI`, `HCItest`  

## Minor updates
  * Parallel analysis plot has an improved legend in the `Validity / Factor analysis` 
    tab 



# ShinyItemAnalysis 1.4.1 



## Bug fixes
  * `gDiscrim()` function was fixed to assure indices are inside the defined 
    bounds, also for small samples
  * `plotDistractorAnalysis()` function was fixed not to display double legends. 
    Thanks to Scarlett Escudero for reporting this issue.
  * Item difficulty/discrimination plot in Tradition item analysis tab in the app 
    was fixed to prevent error messages when Discrimination type is set to "none".
  * Displayed item parameters were fixed and subsequently extended in the app for 
    the Bock model (see Major updates).
  * The training sections for GRM IRT model was fixed to display category plots 
    also for case of k = 6.
  
## Major updates
  * The `ItemAnalysis()` has been refactored substantially; memory demands are 
    lowered multiple times, as well as run times (most noticeable with large datasets). 
    The changes include following:
    - The function now returns `NA`s for "gULI" when `k`, `l`, and `u` arguments 
      are not specified, as there are no sensible defaults. Previous defaults 
      resulted in exactly the same values that ULI already provided on its own.
    - The `Index.rel.drop` is not provided anymore, as it is not well defined 
      (see Gulliksen, 1950, for more details on  reliability index (`Index.rel`)).
    - The `bin` and `add.bin` arguments were removed, the analysis on binarized 
      data can be conducted by simply providing the cutscore in the `cutscore` argument.
    - The `criterion` argument now defaults to `NULL` instead of `"none"`, but 
      behaves the same.
    - Deprecated arguments `data`, `y` are now defunct.
  * The `DistractorAnalysis()` and  `plotDistractorAnalysis()` functions have been 
    improved: argument items allows options `"all"` to apply for all items (default), 
    or a vector of item names (column names of `Data`), or item  identifiers 
    (integers specifying the column number).
  * The `Anxiety` dataset was added.
  * Baseline Logit Slope-Intercept (BLIS) parametrization for Bock nominal 
    response model is now available as a package function. Fitted model supports 
    IRT reparametrization (to so-called BLIRT) using a standard `coef()` method as well.
  * The app training sections for NRM IRT model now includes parameters in 
    parametrizations and identification restrictions assumed by Bock (1972), 
    Thissen et al. (2011), as well as BLIS and BLIRT parametrizations accounting 
    for the correct option.
  * The DIF section in the app was updated to work with the newest difNLR package version.
  
## Minor updates
  * `gDiscrim()` newly issues a warning when item(s) range is zero and when there 
    is no difficulty difference in either group.
  * In `gDiscrim()`, the groups are newly defined using the `quantile()` function. 
    In the case of ties, the respondents with the same total score are now in 
    the same group, the groups sizes may thus differ, but are now exactly defined. 
    The function may however provide different numerical results than in the previous 
    version.
  * In `gDiscrim()`, you can now change how the dataset is split by passing 
    arguments to `findInterval()`.
  * RStudio Addin was added to provide command-free app startup option, bindable 
    to a keyboard shortcut.
  * The `HCI` data now includes total score variable.  


# ShinyItemAnalysis 1.4.0 



*It includes version 1.3.8 and following further changes:*

## Bug fixes
  * Math in downloadable tables is now completely correctly transformed to human-readable
    (non-LaTeX) format everywhere (thanks to Larry Nelson for reporting this issue).
  
## Major updates
  * The package dependencies that are used only in the interactive app *per se*
    were moved into `Suggests`, meaning you are now not obliged to install any
    of these dependencies if you plan to only use the package's "core" functions, 
    such as `DDplot()` or `ItemAnalysis()`.
  * The `startShinyItemAnalysis()` function now checks for dependencies required 
    for its proper run and offers to install these dependencies if any of them 
    is not available.
  * New dataset `HeightInventory` was added.

## Minor updates
  * Manuals of R functions and data were updated

    

# ShinyItemAnalysis 1.3.8 
## Bug fixes
  * Better treatment of missing values in fa_parallel and in the 
    `Validity/Factor analysis` section of the app, preventing crashes of the app
    (Thanks to @koenderks for reporting this as issue #460)
  * Math in downloadable tables is now partly correctly transformed to human-readable
    (non-LaTeX) format, including the IRT tab.
  
## Major updates
  * `startShinyItemAnalysis()` gained an alias `run_app()`.
  * New dataset `AttitudesExpulsion` was added.
  * `DIF-C` SIAmodule newly available from the `DIF/Fairness` section of the 
    interactive app
  * `IRR-restricted` SIAmodule replaced the `Reliability/IRR-restricted` section 
    of the interactive app
  * Math typesetting is now completely carried out by KaTeX, a lightweight and
    reliable library. By eliminating MathJax, the app now loads faster and no
    math is left unrendered.

## Minor updates
  * Sample R code was added for the `Validity/Correlation structure` section.
  * Manuals of R functions and data were updated
  * The app now uses trivial Cronbach alpha internal function, lowering the 
    number of dependencies
  * `www` folder was cleaned up, .png replaced by .svg files improving the 
    resolution of plots
  * README.md assets moved to .readme_assets folder
  * `inst` folder structure was simplified
  


# ShinyItemAnalysis 1.3.7 



## Bug fixes
  * Bug with reading ordinal data in the app was fixed. (Thanks to pukelevicius
    for reporting on GitHub)
  * Bug with uploading data with all correct/wrong answers in some items was 
    fixed (Thanks to Koen Derks for reporting the issue on GitHub)
    
## Major updates
  * New datasets `MSclinical`, `BFI2`, and `TestAnxietyCor` were added, some 
    datasets and manuals were updated.
  * Functions `fa_parallel()` was updated, smaller changes were made in some 
    other functions
  * In the `Validity` section of the app, tab `Factor analysis` was updated.  
  * In the `IRT` section of the app, tab for the Bock model was updated to offer
    parametrization closer to regression models.
  
## Minor updates
  * Sample R code was updated for the `IRT` section.



# ShinyItemAnalysis 1.3.6 



## Bug fixes
  * Function ItemAnalysis() updated to avoid error messages in case of datasets
    including 2 items only. (Thanks to Frederic Hilkenmeier for reporting this 
    issue.)
  * Data upload in the app was fixed, as well as the warning messages related 
    to data upload.
  * In the `Reports` section of the app the default name of the dataset was set
    to the name of the csv file. This fixed some of the bugs with the HTML
    report download.

## Major updates
  * New function `fa_parallel()` was added offering fast paralel analysis for 
    deciding on number of factors in exploratory factor analysis.
  * New datasets `CZmatura` and `CZmaturaS` were added.
  * Some UI code of the shiny app was reorganized into separate sections.
  * In the `Validity` section of the app, a new tab `Factor analysis` was 
    added offering exploratory factor analysis.
  * In the `IRT models` section of the app, the binary IRT models were
    collapsed into a single `Dichotomous models` tab. 
  * In the `IRT models` section of the app, the Bock IRT model section was
    edited. 
  * In the `DIF/Fairness` section of the app, the logistic and the polytomous 
    regression models now offer both the IRT and the intercept-slope 
    parametrization; the total score is newly offered as the matching criterion 
    besides the standardized total score.
  * In all sections of the shiny app, the wording was edited. 
  * The ggWrightMap() function was rewritten to exclude dependencies other than 
    ggplot2. 
  
  
## Minor updates
  * The sample R code snippets were moved to a separate directory.
  * In the `Criterion validity` and `Item criterion validity`, the Pearson 
    correlation is newly used as the default option.
  * In the shiny app, the item criterion validity analysis was moved to the 
    `Item analysis` section.  
  * In the `Item analysis` section of the app, default discrimination was 
    changed to RIT.  
  * Softer deprecation with warning only was applied to functions.
  * In the shiny app, term `Observed ability` is now consistently used instead 
    of `(DIF/DDF) Matching variable`. The changes done were especially in the 
    `Data`, `Regression models` and the `DIF/Fairness` sections of the app, and 
    in the `plotDIFLogistic()` function.
  * Section `Summary` of the app was renamed to `Scores` to beter reflect focus 
    of this section. 
  * In the `Validity` section of the shiny app, the dendrogram is newly enabled 
    even if numclust == 0.
  * In the `Item Analysis` section of the app, the tab `Distractors plot` was
    renamed to `Item response curves` to better reflect that empirical response
    curves are also available for binary and ordinal items.



# ShinyItemAnalysis 1.3.5 



## Bug fixes
  * Plots in the `DIF/About` section of the app were fixed (uniform vs. 
    nonuniform DDF is now correctly displayed).
  * Display of the discrimination index ULI was fixed for ordinal items.
  * Bug in display of Multinomial regression parameters in case of the IRT 
    parametrization was fixed.
  * In the IRT training section, the calculation and display of item information 
    curves was fixed. (Thanks to Hynek Cigler for reporting this issue.)
  * Calculation of minimal scores in the `ItemAnalysis()` function was fixed.
  * Display of ICC in IRT models was updated to respond to updates in the mirt 
    package.
  * In the `Regression` section of the app, missplaced `SE` for intercepts and 
    slopes were fixed.

## Major updates
  * A new section `Reliability/Restricted range` was added to the app offering 
    interactive examples demonstrating issues with reliability estimates in 
    restricted-range samples.
  * Continuous items are newly allowed, rater variable was added, 
    data upload and warning messages were updated.
  * New function `ICCrestricted()` for computing ICC in restricted samples was 
    added.
  * Functions `gDiscrim()`, `DDplot()` and `ItemAnalysis()` were updated, 
    variable names were unified.
  * New function `plot_corr()` to compute and visualize an item correlation 
    matrix was added and implemented within the ShinyItemAnalysis interactive 
    app.
  * New datasets `AIBS`, `NIH`, `HCIgrads`, `HCItestretest`, `HCIprepost` 
    and `HCIdata` were added.
  * `startShinyItemAnalysis()` function was fixed to finish job persisting in 
    the backgroud after the app is closed.
  * Figures can now be downloaded at once in `DDF` section of the app.
  * Equations were unified in the `Regression`, `IRT` and `DIF/Fairness` 
    sections of the app, with `p` being the index for person and `i` being the 
    index for an item.
  * Interactive training section was added to the `DIF/Fairness` section, 
    offering interpretation of group-specific models.
  * Figures in the `Validity`, `Regression`, `IRT`, and `DIF/Fairness` section 
    are now displayed in plotly mode in the shiny app.
  * The `IRT/Rasch` section was restructured and it now offers both the 
    intercept/slope as well as the IRT parametrization. (This is planned for 
    other `IRT` subsections in future versions.)
  * The `Regression/Multinomial` section newly offers also the IRT 
    parametrization.  
  
## Minor updates
  * The intropage was updated, new News section was added.
  * Method ordering was updated in the `DIF/Fairness` section.
  * Toy data selection was updated.
  * Selected R code was updated.
  * Unnecessary dependencies were removed.
  * Summary tab was updated within the app, interactive help with formulae is 
    newly offered.
  * Interpretation of IIC for selected theta were added in the `IRT/training` 
    section.
  * styler package was applied on .R files, documentation was updated.
  * `ggWright()` plot was updated to more general description.
  * References were updated wintin the app.



# ShinyItemAnalysis 1.3.4 



## Bug fixes
  * Display of Cronbach's alpha in PDF/HTML reports was corrected.
  * Item slider bug in ordinal regression models in the app was fixed.
  * DIF matching criterion was synchronized for the `Summary` and `Item` subtabs 
    for the Generalized logistic regression method in the `DIF/Fairness` section 
    of the app. 
  * Bug in display of table in the `Regression/Model comparison` section was 
    fixed.
  
## Major updates
  * `startShinyItemAnalysis()` function was updated to allow starting the 
    application as a background job in Rstudio (default setting). 
    This permits the user to work and try sample R code examples in the console.
  * Downloadable tables are newly provided in shiny app in the `DIF/Fairness` 
    sections for ordinal DIF.
  * DDplot is now displayed in plotly mode in the shiny app.
  * New function `recode_nr()` for recognizing and recoding not-reached 
    items was introduced.
  * `ItemAnalysis()` function was updated with new columns for ratio of missed 
    and unreached items, shortened column names and ordering.
  * Traditional item analysis table in the `Item analysis` section of the app 
    has been expanded to include item reliability, item validity, and percentage 
    of missing and unreached for each item.
  * `DDplot()` function now offers argument `criterion`. Without `criterion` 
    specification, the difficulty-discrimination plot is provided. When 
    `criterion` is specified, the difficulty-validity plot is provided instead, 
    depicting item criterion validity.
  * Difficulty-validity plot is newly offered in the the 
    `Validity/Criterion_Validity/Items` section.

## Minor updates
  * Documentation of `DDplot()` was updated.
  * All datatable outputs now have proper bootstrap style/theme.



# ShinyItemAnalysis 1.3.3 



## Bug fixes
  * `plotDIFLogistic()` now correctly plots matching criterion when item 
     purification is applied.
  
## Major updates
  * Use of DIF matching criterion other than Total score is now enabled in shiny 
    app in the `DIF/Fairness` sections.
  * Downloadable tables are newly provided in shiny app in the `DIF/Fairness` 
    sections.
  * Iterations of purification process are now displayed in downloadable tables
    in the `DIF/Fairness` sections of shiny app.
  * DIF method comparison table is provided in the `DIF/Fairness` section of the 
    shiny app.

## Minor updates
  * Help page for the `ShinyItemAnalysis` package was added.
  * NEWS file is now formatted using `markdown`.
  * On attach message was updated.
  * Wording, Figures and page structure were updated in shiny app in the 
    `DIF/Fairness` sections, histogram of total scores now better compares the 
    groups.   



# ShinyItemAnalysis 1.3.2. 



*It includes versions 1.3.1-1 - 1.3.1-8 and following changes:*

## Bug fixes
  * Mismatch in specification of H0 and H1 was corrected in `DIF/Fairness` 
    section and in the `Report` section.
  * `plotDIFLogistic()` now correctly plots matching criterion.
  * Logistic method in the `DIF/Fairness` section now correctly plots best 
    fitting model when p.adjust.method is applied.
  * Logistic method in the `DIF/Fairness` section now excludes missing values.

## Major updates
  * New dataset `LearningToLearn` was added to the package.
  * New toy datasets `LearningToLearn6` and `LearningToLearn9` were added to the 
    app.
  * Upload of the DIF matching criterion is now available in Data section of the 
    app.
  * D-DIF index calculation was added into DIF/MH/Item to evaluate DIF effect 
    size
  * OR calculation in DIF/MH was edited.
  * `plotDIFLogistic()` function was updated to accept class of `Logistik`
    as input.
  * `plotDIFLogistic()` now accepts character input for `item` argument.
  * Summary and Item inputs in DIF/Logistic are connected now. Model is now
    fitted only once.

## Minor updates
  * Citation and list of References were updated.
  * Authors' details were updated.
  * Due to changes in `difNLR`, function `ddfORD()`was renamed to `difORD()`.
  * About page was updated.
  * Error messages for `DistractorAnalysis()` and `plotDistractorAnalysis()` 
    were unified.
  * Copyright year in footer is now responsive.
  * Description of toy datasets was updated.
  * styler package was used for all functions. Documentation was updated.
  * For `plotDIFLogistic()` it is now possible to turn off plotting empirical 
    probabilities. This can be done using `draw.empirical = FALSE`.
  * Selected R code was updated in DIF/MH
  * Threshold for discrimination can be now specified in DDplot()
    function via argument `thr`. Default value is 0.2.
  * Warning and error messages were updated for `plotDistractorAnalysis()`
    function when `key` is missing.
  * Width of inputs was unified in `DIF/Fairness` section.
  * Wording was updated.
  * Some typos were fixed.
  * Text in reports was updated.
  * Some examples were set to run to allow for automatic checks of the package.



# ShinyItemAnalysis 1.3.1-8. 

## Bug fixes
  * Typos in `plotAdjacent()`, `plotCumulative()` and `plotMultinomial()` were 
    fixed.



# ShinyItemAnalysis 1.3.1-7. 

## Major updates
  * Functions `DistractorAnalysis()` and `plotDistractorAnalysis()`
    were updated to not requiring `key` input.



# ShinyItemAnalysis 1.3.1-6. 

## Bug fixes
  * Bug in key uploading was fixed. When nominal data was uploaded and
    then ordinal (without cut-score specified in file), key was kept
    from nominal data.
  * Bug in ordinal data was fixed. Recalculation of factors to numeric
    is done.
  * Some calculations with `NA` values were fixed.
  * Double file input `"key"` was unified.

## Minor updates
  * Item names for toy dataset `Science` were updated.
  * Basic summary table for ordinal data was updated.



# ShinyItemAnalysis 1.3.1-5. 

## Bug fixes
  * Table for MH in PDF reports was fixed.
  * Header of sections were fixed in PDF reports.
  * IRT parameter estimates table was fixed in PDF reports.
  * Bug in `plotMultinomial()` for data without NAs was fixed.

## Major updates
  * DIF/DDF detection methods for ordinal data were added. DIF/Fairness
    section was updated.

## Minor updates
  * Tabs in Regression were renamed.
  * Warning when group vector is missing was added into Method comparison tab
    in `DIF/Fairness` section.
  * Method comparison tab for `DIF/Fairness` section was updated.



# ShinyItemAnalysis 1.3.1-4. 

## Bug fixes
  * In DIF section, Summary table for total scores now displays correct number
    of respondents in groups (N).



# ShinyItemAnalysis 1.3.1-3. 

## Bug fixes
  * Bug in Adjacent model in Regression tab was fixed. Matching criterion
    and parametrization inputs now have effect on model.
  * Bug in `plotMultinomial()` was fixed. Function now displays more than 12
    response patterns as linetype.

## Minor updates
  * Range for x-axes in `plotAdjacent()` and `plotCumulative()` were updated to
    avoid removing data.
  * Some typos were fixed.



# ShinyItemAnalysis 1.3.1-2. 

## Major updates
  * Cronbach's alpha with CI was added into reports.
  * Bonferroni correction for multiple comparison was added as an option
    for correction methods in Regression, DIF and Reports tabs.

## Minor updates
  * Warning for table for parameters in Bock's IRT model was modified.
  * Wright map now considers item names when option Keep item names.
  * Some wording was updated.
  * Some typos were fixed.



# ShinyItemAnalysis 1.3.1-1. 

## Bug fixes
  * Minimal number of groups for gULI in DDplot was set to 2 in both Item
    analysis and Report tabs.

## Major updates
  * Mantel-Haenszel test was added into reports.

## Minor updates
  * Error for DIF/Logistic was customized.
  * Text in reports in DIF section was updated.
  * Tables in DIF section now includes both p-values and adjusted p-values
    when correction method is applied.
  * Warning for fancyhdr latex package was solved.



# ShinyItemAnalysis 1.3.1. 



*It includes versions 1.3.0-1 - 1.3.0-18 and following changes:*

## Minor updates
  * In `ItemAnalysis()` function, documentation for item reliability and
    item validity indices has been added (Reported by Lot Fonteyne).



# ShinyItemAnalysis 1.3.0-18. 

## Bug fixes
  * Bug in `plotDistractorAnalysis()` was fixed. X-ticks are now displayed
    even with discrete = T argument.

## Minor updates
  * Inputs in Validity/Correlation overlaped when window was narrow. Fixed.
  * Generate and Download buttons overlaped in Reports when window was narrow.
    Fixed.



# ShinyItemAnalysis 1.3.0-17. 

## Bug fixes
  * `DistractorAnalysis()` and `plotDistractorAnalysis()` now handle discrete
    matching and groups specified via argument cut.points.
  * Distractor plots in Validity/Items tab now properly handle discrete
    criterion and the number of groups is correctly descreased when criterion
    does not have unique cuts.
  * Bug in Raju in `DIF/Fairness` tab was fixed. Plots for items are now
    displayed for dataBov dataset.

## Major updates
  * Checking of unique cuts for distractor plots was added into Reports.
    Warning is displayed when cuts are not unique and slider is updated.

## Minor updates
  * Some typos were fixed.



# ShinyItemAnalysis 1.3.0-16. 

## Bug fixes
  * Bug in Bock's model for binary items was fixed in reports.
  * Histogram when some levels of total scores are missing were fixed
    in Summary and DIF tab.

## Major updates
  * Legend was added into IRT plots when downloaded.
  * IRT models now respect ordering of items.

## Minor updates
  * Some typos in IRT models tab were fixed.



# ShinyItemAnalysis 1.3.0-15. 

## Bug fixes
  * DIF summary table bug was fixed. Table is disaplayed when
    some DIF methods cannot be fitted.
  * DDF parameter tables were fixed for data BOV and Czech Trivia.
  * With `difNLR` package version 1.2.3 plots for DDF are displayed
    even for binary data. Plots were also fixed for data Bov.
  * PDF Reports now work locally and on shinyapps.io.
  * Dendrogram now properly replaces item names when Keep item names
    is not selected in Data upload section.

## Minor updates
  * Authors' and contributors' details were updated.
  * Function `plotMultinomial()` was updated to return smooth curves
    for any matching criterion.



# ShinyItemAnalysis 1.3.0-14. 

## Bug fixes
  * Bug in Bock's nominal IRT model was fixed. ICCs are now properly
    displayed for all binary data. Error for non-displayable
    table of parameters was further improved. Tables for items are
    displayed even in case that table for all items is not.
  * DIF items are now correctly written in reports.
  * Corrplot now properly replaces item names when Keep item names
    is not selected in Data upload section.
  * In `DIF/Fairness` section, histograms of Total scores for groups
    are now properly displayed also for ordinal items.

## Major updates
  * Legends for multinomial and distractor plots were improved.

## Minor updates
  * Authors' and contributors' details were updated.



# ShinyItemAnalysis 1.3.0-13. 

## Bug fixes
  * Bug in Bock's nominal IRT model was fixed. ICCs are now properly
    displayed for Czech Trivia dataset. Error for non-displayable
    table of parameters was improved.
  * Bug in `DistractorAnalysis()` was partially fixed, need to be revised.

## Major updates
  * Reports:
      - long tables have now header on each new page
      - legend is now visible for IRT plots even when dataset consists
        of more than 50 items
  * Matching criterion in `plotMultinomial()` was relaxed to decrease
    size of reports.



# ShinyItemAnalysis 1.3.0-12. 

## Bug fixes
  * Reports are now rendered even in case that multinomial model
    cannot be fitted for some items.
  * With option Keep item names, names of items are properly
    displayed in Report for DIF detection methods.
  * Model comparison table returns now also partial results when
    some models cannot be fitted.
  * Bug in plotMultinomial() was fixed. In case of only two categories
    observed, plot is displayed properly now.
  * Bug in `DistractorAnalysis()` was fixed. In case of possibly discrete
    matching, matching is cutted based on its factors.

## Major updates
  * Uploaded data can be now unloaded without restarting app.
  * Distractor plot for validity analysis was updated to allow
    for lowering the number of categories in cases when cuts
    are not uniquely defined. Application was updated accordingly.

## Minor updates
  * Errors and warnings were customized.
  * In all dichotomous models the number of EM cycles can be
    increased in Settings.
  * Seed was set for validity boxplot for reproducibility of
    jitter points.
  * All static DOI links were updated.
    (Thanks to Katrin Leinweber)
  * Some typos were fixed in reports.



# ShinyItemAnalysis 1.3.0-11. 

## Bug fixes
  * Option for displaying session info in reports now functions properly.
  * Option to display average score in DDplot in reports fixed.

## Major updates
  * New function `plotCumulative()` which plots category probabilities
    functions estimated by \code{multinom()} from `nnet` package
    was added. It is now used in application.
  * New function `plotAdjacent()` which plots category probabilities
    functions estimated by \code{multinom()} from `nnet` package
    was added. It is now used in application.
  * New function `plotMultinomial()` which plots category probabilities
    functions estimated by \code{multinom()} from `nnet` package
    was added. It is now used in application.

## Minor updates
  * Selected R codes for cumulative, adjacent and multinomial models
    were updated.
  * Option for matching criterion (total vs Z-score) in multinomial
    model was added.
  * Equations in DIF section are now center-aligned.



# ShinyItemAnalysis 1.3.0-10. 

## Major updates
  * Alerts for analysis where data with missing values are omitted
    were added.
  * `DDplot()` now can displayed average item score as estimate of
    difficulty for ordinal data. See argument average.score.

## Minor updates
  * Correlation structure now use pairwise complete observations
    for calculation of correlation.



# ShinyItemAnalysis 1.3.0-9. 

## Major updates
  * `DistractorAnalysis()` and `plotDistractorAnalysis()` functions were
    updated to allow for lowering the number of categories in cases when
    cuts are not uniquely defined. Application was updated accordingly.

## Minor updates
  * Tables and equations of polytomous models in Regression section are
    now center-aligned.



# ShinyItemAnalysis 1.3.0-8. 

## Bug fixes
  * With option `Keep missing values` unfunctioning analyses were fixed
    (missing data removed, warning message added)

## Major updates
  * Multinomial regression plot is now displayed using smooth function

## Minor updates
  * With option `Keep missing values` warning message is now displayed
    for those analyses for which missing values are omitted.
  * For ordinal data, Cronbach's alpha is now calculated from original
    ordinal (non-binarized) data both in Reliability and in Item analysis
    section.



# ShinyItemAnalysis 1.3.0-7. 

## Bug fixes
  * Busy indicator now properly displays above input tabs.
  * Bug when using matching in `DistractorAnalysis()` was fixed.

## Major updates
  * New ordinal toy dataset SCIENCE was added.

## Minor updates
  * Each tab is now open at the top of the page.



# ShinyItemAnalysis 1.3.0-6. 

## Major updates
  * Adjacent logistic regression model was implemented for ordinal data.
  * Both adjacent and cummulative regression models allow for selection
    of matching criterion (total score, standardized total score) and
    parametrization (classical slope/intercept and IRT parametrization).



# ShinyItemAnalysis 1.3.0-5. 

## Bug fixes
  * SD in Regression section was changed to SE.
  * Keep missing values button is now functioning and it is applied
    for ordinal and binary data.

## Major updates
  * Cumulative logistic regression model was implemented for ordinal data.
  * Delta method is now applied for calculation of SE in IRT parametrization
    in cumulative logistic regression.



# ShinyItemAnalysis 1.3.0-4. 

## Bug fixes
  * Bug in slider in histogram of total scores was fixed. Range is now
    correctly displayed when NA values are present.
  * Fixing versions.

## Major updates
  * Math mode is now stable in tables.
    (Thanks to Stéphane Laurent)
  * UI for polytomous models in Regression section was added.
  * Traditional item analysis table is done via ItemAnalysis() function.
    It now includes also average score.

## Minor updates
  * `startShinyItemAnalysis()` automatically launches default web browser.
  * Sample code and wording was updated for traditional item analysis table
    in app and also in reports.



# ShinyItemAnalysis 1.3.0-3. 

## Minor updates
  * `ggWrightMap()` now can display user-specified names of items
    (Reported by Matthew G R Courtney)
  * `plotDIFLogistic()` now can display user-specified names
    of reference and focal group.
  * Some typos were fixed.



# ShinyItemAnalysis 1.3.0-2. 

## Major updates
  * Analysis for ordinal items has been added. This includes use of ordinal
    data in sections Summary (histogram of Total scores, success rate),
    Reliability, Validity (correlations, predictive validity),



# ShinyItemAnalysis 1.3.0-1. 

## Bug fixes
  * Equations for 3PL and 4PL regression models were fixed.

## Major updates
  * Math symbols are now displayed using MathJax.
  * Exercises were added in IRT section / Polytomous Training.
  * DIF section now includes method comparison.

## Minor updates
  * Labeling of correction methods in DIF section was improved.
  * Ability parameters are now available also for Bock's model.



# ShinyItemAnalysis 1.3.0. 



*It includes versions 1.2.9-1 - 1.2.9-3 and following changes:*

## Minor updates
  * Reference to a new paper on `ShinyItemAnalysis` in The R journal was added.



# ShinyItemAnalysis 1.2.9-3. 

## Bug fixes
  * Histograms for total scores now handle cut-scores out of range of
    observed values
  * Bug in reports when using delta plot with purification was fixed.
  * Cut-scores for histograms are properly displayed in reports.
  * Year in reports was updated.

## Major updates
  * Sections About, References and Settings are displayed only with icon
    and are right-aligned.
  * Margins and padding were updated.
  * t-test was added for comparison of total scores between groups
    into DIF section and into reports.
  * Histograms for total scores are now displayed in plotly mode.

## Minor updates
  * Some math mode was fixed in IRT section.
  * Some typos were fixed.
  * Selected R code was updated in DIF/Total scores.
  * Animate function for sliders was slowed down.



# ShinyItemAnalysis 1.2.9-2. 

## Bug fixes
  * Generalized ULI index is now correctly displayed in traditional
    item analysis table in reports.
  * Item names are now correctly displayed in reports.
  * Item names are now correctly displayed in DDF section.
  * Function `plotDIFLogistic()` now handles user-specified matching
    criterion.

## Minor updates
  * Legend of DDplot was modified.



# ShinyItemAnalysis 1.2.9-1. 

## Minor updates
  * Wording in PDF/HTML reports was edited.



# ShinyItemAnalysis 1.2.9. 

## Bug fixes
  * IRT/Training/Dichotomous models - plots are correctly plotted.
  * Download buttons in IRT/4PL works now.

## Major updates
  * TIF were converted into plotly.
  * Section Validity/FA is not accessible for now.

## Minor updates
  * Selected R code for IRT/Bock's model was updated.
  * Wording in IRT models was updated.
  * Wording in Reports was updated.
  * Legend for plots of IRT models was modified in Reports.
  * Ability estimates summary table is now round in Reports.
  * Some typos were fixed in Reports.
  * Colours of IRT summary plots correspond to model for Items.




# ShinyItemAnalysis 1.2.8-7. 

## Major updates
  * IRT section of shiny app was updated and now provides
    new and more shiny figures in plotly
  * PDF and HTML reports were updated and now allow
    - different types of correlation in corrplots
    - different types of discrimination indices in DDplots
    - summary table for ability estimates in IRT section



# ShinyItemAnalysis 1.2.8-6. 

## Bug fixes
  * Dendrogram is now correctly rendered.

## Major updates
  * Function `ItemAnalysis()` was added.
  * IRT section now allows download of ability estimates

## Minor updates
  * Help pages for functions were updated.



# ShinyItemAnalysis 1.2.8-5. 

## Bug fixes
  * Key for ordinal data for binarization is now correctly used. Values
    greater or equal to value of the key are set to 1.
  * When NULL data is uploaded, default dataset GMAT is in use.

## Major updates
  * Scree plot was moved into the new tab `Factor analysis`.
  * Dendrogram was added into `Correlation structure` tab.
  * Option for global cut-score for ordinal data was added.
  * R package ggdendro is now used.
  * Removal of items with only 0s or 1s now works.
  * Removal of observation with NA group now works.

## Minor updates
  * Text in Data section was updated.
  * List of references was updated.
  * Selected R code in `Correlation structure` tab was updated.



# ShinyItemAnalysis 1.2.8-4. 

## Bug fixes
  * Bug in IRT models/Training/Dichotomous models text of theta in
    plotly was fixed.

## Major updates
  * Reorganization of data upload and renaming data variables.
      test_answers()> nominal()
      correct_answers()> binary()
      scored_test()> total_score()
      test_key()> key()
      DIF_groups()> group()
      criterion_variable -> criterion()

    And creating new variables:
      ordinal() ordinal data
      maximal() maximal values for ordinal data
      minimal() minimal values for ordinal data
      z_score() standardized total score, no need to use scale(total_score())
                  in each function

## Minor updates
  * Updating text in Data section.
  * Editing boxes for data upload in Data section.



# ShinyItemAnalysis 1.2.8-3. 

## Bug fixes
  * Bug in Regression/Nonlinear 4P IRT Z (rounding in interpretation)
    was fixed.
  * Bug in IRT models/Training/Dichotomous models text of theta in
    plotly was fixed.
  * Bug in IRT models/Training in plotly a ModeBar was fixed.

## Major updates
  * Data upload now allows also ordinal data, min/max values can be
    specified.
  * Validity/Correlation structure now allows selection between
    Pearson/Spearman/Polychoric correlation, Polychoric is default
    for binary data
  * DDplot now allows also ordinal items and offers different types
    of discrimination indices to be displayed
  * Download of tables (as .csv files) is now provided for
    standard scores, correlation matrix, traditional item analysis,
    and for parameter estimates in IRT models.

## Minor updates
  * Data upload now better points to "Upload data" button.
  * Precision was re-set for IRT models/Training tab. Now = 0.1.



# ShinyItemAnalysis 1.2.8-2. 

## Major updates
  * `gDiscrim()` function has been extended to calculate Upper-Lower
    Index (ULI) also for ordinal items



# ShinyItemAnalysis 1.2.8-1. 

## Bug fixes
  * Bug in IRT dichotomous models comparison is fixed.
    (Reported by Jean-Luc Kop)
  * Bug in IRT dichotomous models (3PL and 4PL) was fixed.
    Tables of parameters are rendered even in case that EM cycles
    terminated without convergence. In such a case SD of parameters
    are set to NA.
  * Bug in `plotDIFLogistic()` function was fixed. Function now returns
    also item.name argument.
  * Bug in `plotDIFirt()` function was fixed. Function now returns also
    item.name argument.

## Major updates
  * Global setting for figure downloads was created. This can be changed
    in Setting section.



# ShinyItemAnalysis 1.2.8 



*It includes versions 1.2.7-1 - 1.2.7-7*



# ShinyItemAnalysis 1.2.7-7. 

## Major updates
  * Section Reliability was added.
    - Split-half methods including also Revelle's beta were added
    - Cronbach's alpha method with CI was moved to section Reliability.
    - Spearman-Brown formula was added.
  * New function ggWrightMap replaced wrightMap from WrightMap package.
  * Section IRT models
    - Selected R code for Rasch and 1PL models was updated.

## Minor updates
  * Reference list was updated.



# ShinyItemAnalysis 1.2.7-6. 

## Major updates
  * Section DIF/Fairness
    - Subsection Logistic IRT Z is deprecated.
    - Subsection Nonlinear IRT Z is now called Generalized logistic.
    - Subsection Generalized logistic includes now wide range of models
      and different specification of DIF type.
    - Subsection SIBTEST was added, including SIBTEST method and
      Crossing-SIBTEST method.

## Minor updates
  * Reference list was updated.



# ShinyItemAnalysis 1.2.7-5. 

## Minor updates
  * Some wording was updated.
  * Selected R code was updated.
  * Function theme_shiny is deprecated.
    New function `theme_app()` was created and applied for all
    graphics, including R functions in package.



# ShinyItemAnalysis 1.2.7-4. 

## Minor updates
  * Paddings were adjusted.
  * Acknowledgments were updated.



# ShinyItemAnalysis 1.2.7-3. 

## Bug fixes
  * Crashing of the app when upload no data was fixed.

## Major updates
  * Data page
    - Design of data page was changed.
    - User can now specify encoding of missing values.
    - Basic summary was added.



# ShinyItemAnalysis 1.2.7-2. 

## Major updates
  * Phone version
    - collapsible menu closes when clicked



# ShinyItemAnalysis 1.2.7-1. 

## Bug fixes
  * Plotly files in IRT training section are now displayed with
    mode bar due a bug in plotly package.

## Minor updates
  * Some typos were fixed.
  * Link to google form for feedback was added into About section.
  * New R icon was added into footer.



# ShinyItemAnalysis 1.2.7. 



*It includes versions 1.2.6-1 - 1.2.6-12*



# ShinyItemAnalysis 1.2.6-12. 

## Minor updates
  * Mirror at Charles University is no longer in use.



# ShinyItemAnalysis 1.2.6-11. 

## Major updates
  * Function `DDplot()`
    - Limits for y-axis were adjusted when negative values of discrimination
      are detected.
    - Warning for negative discrimination values were added.
  * Reports
    - Some unused parts of reports were excluded.



# ShinyItemAnalysis 1.2.6-10. 

## Major updates
  * UI
    - Some parts were split from main ui.R file
  * IRT
    - Exercises in IRT training were extended.
  * Data
    - Bug in data upload was fixed. Now length of criterion variable
      is correctly checked.



# ShinyItemAnalysis 1.2.6-9. 

## Major updates
  * Packages - shinyBS is now required.
  * Data
    - HCI dataset was added into package.
    - Description of dataMedical was improved.
    - Description of training datasets was improved.
  * About
    - Page was improved.
  * IRT
    - Helps for interactive exercises were added.

## Minor updates
  * Typos were fixed.



# ShinyItemAnalysis 1.2.6-8. 

## Major updates
  * IRT
    - Expected item scores were removed from IRT training NRM.
    - Interactive exercises were added into  IRT training - dichotomous
      models.



# ShinyItemAnalysis 1.2.6-7. 

## Major updates
  * Data
    - Graded data od Medical data set was added.
    - In all dataMedical data sets the criterion variable was renamed to
      StudySuccess.
  * IRT
    - Expected item scores were added into polytomous models in IRT training.
    - Selected R code was updated in IRT training.

## Minor updates
  * IRT section
    - Colors of sliders and curves in IRT training section were adjusted.



# ShinyItemAnalysis 1.2.6-6. 

## Major updates
  * Regression
    - NLR 4P model was added into model comparison.

## Minor updates
  * README file was updated. Typos were fixed.
  * Design of figures was unified.
  * Download button options were unified.
  * Selected R code syntax was improved in some sections
    (IRT only partionally, DIF not).
  * Selected R code was improved for first 4 sections.
  * Some wording was improved.



# ShinyItemAnalysis 1.2.6-5. 

## Major updates
  * IRT section
    - Wording for IRT training was added and improved.
  * Selected R code syntax was improved in some sections. This can be
    done with
    txt <- gsub("\n", "<br>", txt)
    txt <- gsub(" ", "&nbsp;", txt)
    txt
    where txt is R code in "".
  * theme_shiny was created as a default theme for all figures.
    Now it is used only partionally.

## Minor updates
  * README file was updated. Typos were fixed. Links were added.
    Hexbin was added.
  * References were updated.



# ShinyItemAnalysis 1.2.6-4. 

## Major updates
  * IRT section
    - Selected R code was added into polytomous models.
    - Equations were added into polytomous models.
    - Download buttons were added into polytomous models.
    - 4PL model was added.
    - 4PL model was added into model comparison.
    - Sliders in IRT training are now colored with respect to line colors.
  * Regression section
    - Section is newly organized.
    - NLR 4P model was added.
  * Reports
    - 4PL IRT model was added as an option.
    - IRT equation is generated based on selected model only.
  * Unnecessary files were deleted. Some cleaning of files was done.

## Minor updates
  * IRT section
    - Selected R code in model comparison was updated. Typos were fixed.
  * Regression section
    - Interpretation of NLR 3P model was extended.
    - Syntax for NLR 3P model was improved.



# ShinyItemAnalysis 1.2.6-3. 

## Major updates
  * IRT section
    - IRT section is newly organized.
    - IRT training section was expanded by polytomous models.
      GRM, GPCM and NRM were added.
    - Syntax for IRT training was improved.

## Minor updates
  * IRT section
    - Sliders steps are now 0.01 in dichotomous models.



# ShinyItemAnalysis 1.2.6-2. 

## Minor updates
  * Numeric inputs were removed from IRT training section.
  * Selected R code was extended in IRT training section by item
    information function plot.
  * References were updated.
  * Mirror web link was updated.



# ShinyItemAnalysis 1.2.6-1. 

## Bug fixes
  * Reports with DDF but none DDF item are now correctly generated.

## Minor updates
  * Hexbin
    - was added into footer.
    - is now as shortcut icon.
  * Paddings
    - in large devices was adjusted.
    - in small devices was adjusted.
  * Unnecessary files were deleted.
  * Reports
    - Some wording in reports was improved.
    - PDF report text is fully justified.
    - Subsections characteristic curves is not visible when none DIF
      or DDF item is present.
    - Some typos were fixed.
    - Some parameters were renamed. The unnecessary ones were removed.



# ShinyItemAnalysis 1.2.6 

## Major updates
  * UI for `DIF/Fairness` section was updated.
  * References were added into About page.
  * New design of reports is now available. Design of PDF reports was
    updated.
  * Wording in report was improved.
  * `DIF/Fairness` section in report was improved.
  * New hexbin for `ShinyItemAnalysis` was created and added into reports.
  * Sample code in `DIF/Fairness` IRT Lord and IRT Raju was extended to
    1PL, 2PL and 3PL models.
  * Bugs fixing
      - Wording in regression plots was fixed.
      - Examples in `DIF/Fairness` IRT Lord and IRT Raju were fixed.
      - Function `plotDIFirt()` now can be also used for only one item.

## Minor updates
  * Reference list was updated.
  * CITATION file was modified.
  * In Summary page the description for summary table was added. The sample
    code was updated.
  * Wording in summary statistics in Summary section was improved.
  * Description of Session info section in reports was added.



# ShinyItemAnalysis 1.2.5 

## Major updates
  * Correlation plots now offer reordering by hierarchical clustering.
    Several linkage methods are offered. This is also included in reports.
  * Confirmation of data upload was added. Also warnings and errors are
    displayed in case that something went wrong when uploading.
  * In IRT training
      - parameter theta (latent ability) is now included
      - the interpretation for characteristic curves is provided
      - horizontal and vertical lines for specified theta were added
        into plot for characteristic curves
      - selected R code was added.
  * In multinomial regression bug when only two choices in data are present
    was fixed.

## Minor updates
  * Link for Adela's website was added.
  * Columns for BIC and its adjusted version were swapped in table for
    IRT models comparison. Text above table was also updated.
  * In IRT section dpi of downloaded plots was set to 300.
  * In IRT training downloaded plots also include legend with parameters.
  * In all IRT models table with parameters includes all parameters
    a, b, c, d.
  * In IRT training equations were moved above interpretation.



# ShinyItemAnalysis 1.2.4 

## Minor updates

  * Reports were mildly updated.
  * Resolution of figures in reports was increased (now 300ppi)
  * Dependencies were updated.
  * Some typos fixed.
  * Legends of plots were unified and placed in better way for
      - `NLR IRT Z`
      - `DDF`
      - function `plotDIFLogistic()`
  * Bug in item names for `DDF` subsection of `Fairness/DIF` section
    was fixed.



# ShinyItemAnalysis 1.2.3 



## Major updates
  * Report's customized options were extended. Reports now include also
    dynamic description.
  * Author and dataset name can be now filled and add into reports.
  * Dataset `difMedical` was renamed to `MSAT-B` according to `difNLR`
    package. Its description was mildly updated.
  * Purification options were added into `Nonlinear IRT Z` and `DDF` subsections
    of `DIF/Fairness` sections.
  * Purification options was added `DDF` analysis in `Reports` section when customized
    setting is selected.
    
## Minor updates

  * Size of plots and font in plots was changed for reports.
  * Plots in HTML report are now centered.
  * Wording of reports' default option was updated.
  * References were updated in `Reference` section and also in README file.
  * Wording of Spearman test in `Validity` section was mildly updated.



# ShinyItemAnalysis 1.2.2 

## Major updates
  * Design of reports was edited.
  * Several bugs were fixed:
     - Bug when creating report for uploaded data was fixed.
     - Bug when creating report for data without predictive
       criterion or DIF group was fixed.
  * `ShinyItemAnalysis` now uses `plotly` package!

## Minor updates
  * Design and options of `Report` page were updated.
  * Layout of IRT curves training page was mildly changed. There are now
    two curves in each plot. Also text area input was added and synchronized with sliders.
    Colour of sliders now correspond to colour of curve.



# ShinyItemAnalysis 1.2.1 

## Major updates
  * Several bugs were fixed:
     - Interpretation of regression models was fixed.
     - Interpretation of text about correlation in Predictive validity
       was fixed.
  * `ShinyItemAnalysis` now used also `data.table` package.
  * Data page was divided into two subpages to avoid long rendering
    of tables.
    when changing datasets.
  * Function `gDiscrim()` to calculate generalized discrimination was
    implemented.
  * Function `DDplot()` was edited to be able to handle generalized
    discrimination.
    Double slider was added into `Traditional analysis page`.
  * Traditional analysis table was extended by adding column
    with generalized
    discrimination. This is done also in reports.
  * Some pages has now their own source code files:
     - `Summary` - Summary.R
     - `Validity`- Validity.R
     - `Traditional analysis` - TraditionalAnalysis.R
     - `Regression` - Regression.R
     - `IRT models` - IRT.R
     - `DIF/Fairness` - DIF.IRT

## Minor updates
  * Last three rows in regression comparison table are now bold.
  * Reference list was extended.
  * The subpage `Distractors` of `Item analysis` page was mildly edited.
  * Generalized DD plot was removed from `Distractor` subpage. It is now
    available on `Traditional item analysis` subpage.
  * Item patterns frequency plot is now made with `ggplot2` package.
  * Names of downloaded plots were customized and their resolution was
    increased and size of text was descreased in some pages:
     - Summary
     - Validity
     - Item analysis
     - Regression



# ShinyItemAnalysis 1.2.0 



## Major updates
  * `ShinyItemAnalysis` can now handle missing values. Newly there is
    an option to handle NA values as zeros in scored data.
  * Bug in WrightMap was fixed.
  * We have new contributor - Lubomir Stepanek!
  * New online mirrors were added.
  * Busy indicator and tooltips were added.
  * Structure of application was mildy updated.
  * Datasets `dataMedical`, `dataMedicalkey`, `dataMedicaltest`
    were updated. Group variable and criterion variable were added.
  * The newest versions of some packages are now required (`difNLR v 1.0.2`,
    `difR v 4.7`, `ggplot2 v 2.2.1`, `mirt v 1.24`, `shiny 1.0.3` and
    `shinyjs v 0.9`).
  * DIF/Fairness
    - Option of item purification was added into some DIF detection
      methods - Delta plot method, Mantel-Haneszel, Logistic regression
      with total score, Lord's test and Raju's test.
    - Default option of p-values adjustments in DIF detection methods
      was set to `none`.
    - Bugs in DIF Logistic IRT Z and DIF Nonlinear IRT Z tables of parameters
      were fixed.
  * IRT models
    - IRT models are now fitted only by mirt package.
    - Training plots of item characteristic and information curves were
      added into IRT models section.
    - Pearson's correlation coefficient between factor scores and
      standardized total score was added for Bock's nominal IRT model.
  * Reports
    - New options for reports were added. By default no IRT models and
      no graphs of correlation structure are fitted.
    - Reports are now described in detail.
    - The settings of reports are now also offered in Report section.
    - Generation of reports is now done part by part. Once it is finished,
      the download button is offered.
    - Message boxs with warning for generation and download of reports
      were added.
  * Validity analysis:
    - Validity analysis of predictive outcome is now included on Validity
      page in Predictive validity section.
    - Now it is possible to upload predictive outcome.
    - Dataset `GMAT` and `GMATtest` from `difNLR` package include now criterion
      variable which can be used in Predictive validity section.
    - Functions `DistractorAnalysis()` and `plotDistractorAnalysis()` were
      extended by `matching` argument.
  * Item analysis
    - Barplot of answer selection proportion was added into Distractor
      subsection.
    - Diagram of custom discrimination - new version of DD plot - was
      added into Distractor subsection.

## Minor updates
  * CITATION file was added.
  * Maximum upload size was set to 30MB.
  * Item names were fixed and unified.
  * Some typos were fixed.
  * Readme file was updated.
  * Reports design was slightly updated.
  * Code was slightly updated to correspond to new versions of packages
    `difNLR`, mirt and `ggplot2`.
  * Selected code was updated.
  * Package dependencies were updated (packages `knitr` and `xtable` are
    now used in report generation).
  * Minor bug in DIF IRT (Lord and Raju) plots and downloads was fixed.
  * Designs of footer and navigation bar were updated.
  * The version for small media was designed.
  * Weblink for version 1.1.0 was created.
  * Reference list was expanded and links to online versions of articles
    were added.



# ShinyItemAnalysis 1.1.0 


## Major updates
  *  New overall appearance using shinythemes package.
  *  New datasets `dataMedical`, `dataMedicalkey`, `dataMedicaltest`
     were added into the package.
  *  New dataset is offered in example datasets inside the shiny app.
  *  Bug in Distractor plots for uploaded data was fixed.
  *  Calculation of Success Rate in Standard Scores was fixed.
  *  Differential Distractor Functioning detection method was added.
  *  Syntax of `difNLR` functions was edited to response to new version of
     package.
  *  Function `plotDIFirt()` was updated.
  *  Correlation structure section was added including correlation plot
     and scree plot.
  *  Pages structure was changed and updated.
  *  IRT models are now fitted by mirt package.
  *  Subsection IRT model selection was added into IRT models section.
  *  Wrigh maps were added for Rasch and 1PL IRT model.
  *  Bug in data upload was fixed.
  *  Subsection regression model selection was added into Regression
     section.

## Minor updates
  *  Typos were fixed.
  *  Selected R code was updated.
  *  Description of data including uploading own data set was extended.
  *  Description of Standard scores was added.
  *  Lower panel was updated.
  *  Rasch model was added into IRT models section.
  *  Pearson correlation coefficient of factor scores and Z-scores were
     added for IRT models
  *  Reference list was expanded.
  *  Bug in multinomial regression for scored data was fixed.
  *  Bug in DDF for datasets withou DIF items was fixed.
  *  Selected code was updated.


# ShinyItemAnalysis 1.0.0 

## Major updates
  *  New graphical representation via `ggplot2` package
  *  Cronbach's alpha was added
  *  Distractor analysis was extended
  *  More DIF detection methods were added
     Mantel-Haenszel test, Logistic regression,
     Lord's and Raju's statistic for IRT models
     all are now used via difR package
  *  Delta plots were extended:
     fixed and normal threshold are now allowed
  *  Multiple comparison correction methods were added
     into DIF detection procedures
  *  More examples of datasets were added
     from `difNLR` package
  *  References were added
  *  New functions were added into the package:
    - `DDplot()` function for graphical representation
       of difficulties and discriminations.
    - `DistractorAnalysis()` function and its graphical
      representation `plotDistractorAnalysis()`.
    - `plotDIFirt()` and `plotDIFLogistic()` functions for
      plotting characteristic curves based on IRT models
      and based on logistic regression.
