## Changes and developments in the ShinyItemAnalysis package

----------

### Changes in version 1.3.4-1 (2020-11-23)
#### BUG FIXING
  * In the IRT training section, the calculation and display of item information curves was 
    fixed. (Thanks to Hynek Cigler for reporting this issue.)
  * Calculation of minimal scores in ItemAnalysis() function was fixed.
  * Display of ICC in IRT models is being updated to respond to updates in mirt package.
  
#### MAJOR UPDATES
  * New function `plot_corr()` to compute and visualize an item correlation matrix was added
    and implemented within the ShinyItemAnalysis interactive app
  * New datasets `HCIgrads`, `HCItestretest`, `HCIprepost` and `HCIdata` were added
  * `startShinyItemAnalysis()` function was fixed to finish job persisting in the backgroud 
    after the app is closed
  * Figures can now be downloaded at once in DDF section of the app.
  * Equations were unified in Regression and IRT sections of the app, with p being the index 
    for person and i being the index for an item. 
  
#### MINOR UPDATES
  * Selected R code was updated.
  * Unnecessary dependencies were removed.
  * Summary tab was updated within the app, interactive help with formulae is newly offered.
  * Lines for interpretation of IIC for selected theta were added in IRT training section.
  * styler package was applied on .R files, documentation was updated
  * ggWright() plot was updated to more general description
  
  ----------

### Changes in version 1.3.4 (2020-08-24)
#### BUG FIXING
  * Display of Cronbach's alpha in PDF/HTML reports was corrected.
  * Item slider bug in ordinal regression models in the app was fixed.
  * DIF matching criterion was synchronized for Summary and Item subtabs in the
    Generalized logistic regression method, DIF/Fairness section of the app. 
  * Bug in display of table for Regression / Model comparison was fixed.
  
#### MAJOR UPDATES
  * `startShinyItemAnalysis()` function was updated to allow starting the application 
    as a background job in Rstudio (default setting). This permits the user to work 
    and try sample R code examples in the console.
  * Downloadable tables are newly provided in shiny app in DIF sections for ordinal DIF.
  * DDplot is now displayed in plotly mode in the shiny app.
  * New function `recode_nr()` for recognizing and recoding not-reached responses 
    was introduced.
  * `ItemAnalysis()` function was updated with new columns for ratio of missed and 
    unreached items, shortened column names and ordering.
  * Traditional item analysis table in Item analysis section of the app has been 
    expanded to include item reliability, item validity, and percentage of missing and
    unreached for each item.
  * `DDplot()` function now offers argument `criterion`. Without `criterion` specification,
    difficulty-discrimination plot is provided. When `criterion` is specified, 
    difficulty-validity plot is provided instead, depicting item criterion validity.
  * Difficulty-validity plot is newly offered in section Validity/Criterion_Validity/Items

#### MINOR UPDATES
  * Documentation of `DDplot()` was updated.
  * All datatable outputs now have proper bootstrap style/theme.
----------

### Changes in version 1.3.3 (2020-05-04)

#### BUG FIXING
  * `plotDIFLogistic()` now correctly plots matching criterion when item purification 
    is applied.
  
#### MAJOR UPDATES
  * Use of DIF matching criterion other than Total score is now enabled in shiny app
    in DIF sections.
  * Downloadable tables are newly provided in shiny app in DIF sections.
  * Iterations of purification process are now displayed in downloadable tables
    in the DIF sections of shiny app.
  * DIF method comparison table is provided in DIF section of the shiny app.

#### MINOR UPDATES
  * Help page for the `ShinyItemAnalysis` package was added.
  * NEWS file is now formatted using `markdown`.
  * On attach message was updated.
  * Wording, Figures and page structure were updated in shiny app in DIF sections,
    histogram of total scores now better compares the groups.   
----------

### Changes in version 1.3.2. (2020-01-27)

**_THIS IS A CRAN VERSION_**

*It includes versions 1.3.1-1 - 1.3.1-8 and following changes:*

#### BUG FIXING
  * Mismatch in specification of H0 and H1 was corrected in `DIF/Fairness` section
    and in `Report` section.
  * `plotDIFLogistic()` now correctly plots matching criterion.
  * Logistic method in `DIF/Fairness` section now correctly plots best fitting
    model when p.adjust.method is applied.
  * Logistic method in `DIF/Fairness` section now excludes missing values.

#### MAJOR UPDATES
  * New dataset `LearningToLearn` was added to the package.
  * New toy datasets `LearningToLearn6` and `LearningToLearn9` were added to the app.
  * Upload of the DIF matching criterion is now available in Data section of the app.
  * D-DIF index calculation was added into DIF/MH/Item to evaluate DIF effect size
  * OR calculation in DIF/MH was edited.
  * `plotDIFLogistic()` function was updated to accept class of `Logistik`
    as input.
  * `plotDIFLogistic()` now accepts character input for `item` argument.
  * Summary and Item inputs in DIF/Logistic are connected now. Model is now
    fitted only once.

#### MINOR UPDATES
  * Citation and list of References were updated.
  * Authors' details were updated.
  * Due to changes in `difNLR`, function `ddfORD()`was renamed to `difORD()`.
  * About page was updated.
  * Error messages for `DistractorAnalysis()` and `plotDistractorAnalysis()` were unified.
  * Copyright year in footer is now responsive.
  * Description of toy datasets was updated.
  * styler package was used for all functions. Documentation was updated.
  * For `plotDIFLogistic()` it is now possible to turn off plotting empirical probabilities.
    This can be done using`draw.empirical = FALSE`.
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

----------

### Changes in version 1.3.1-8. (2019-11-02)

#### BUG FIXING
  * Typos in `plotAdjacent()`, `plotCumulative()` and `plotMultinomial()` were fixed.

----------

### Changes in version 1.3.1-7. (2019-09-30)

#### MAJOR UPDATES
  * Functions `DistractorAnalysis()` and `plotDistractorAnalysis()`
    were updated to not requiring `key` input.

----------

### Changes in version 1.3.1-6. (2019-09-25)

#### BUG FIXING
  * Bug in key uploading was fixed. When nominal data was uploaded and
    then ordinal (without cut-score specified in file), key was kept
    from nominal data.
  * Bug in ordinal data was fixed. Recalculation of factors to numeric
    is done.
  * Some calculations with `NA` values were fixed.
  * Double file input `"key"` was unified.

#### MINOR UPDATES
  * Item names for toy dataset `Science` were updated.
  * Basic summary table for ordinal data was updated.

----------

### Changes in version 1.3.1-5. (2019-08-26)

#### BUG FIXING
  * Table for MH in PDF reports was fixed.
  * Header of sections were fixed in PDF reports.
  * IRT parameter estimates table was fixed in PDF reports.
  * Bug in `plotMultinomial()` for data without NAs was fixed.

#### MAJOR UPDATES
  * DIF/DDF detection methods for ordinal data were added. DIF/Fairness
    section was updated.

#### MINOR UPDATES
  * Tabs in Regression were renamed.
  * Warning when group vector is missing was added into Method comparison tab
    in `DIF/Fairness` section.
  * Method comparison tab for `DIF/Fairness` section was updated.

----------

### Changes in version 1.3.1-4. (2019-08-07)

#### BUG FIXING
  * In DIF section, Summary table for total scores now displays correct number
    of respondents in groups (N).

----------

### Changes in version 1.3.1-3. (2019-07-02)

#### BUG FIXING
  * Bug in Adjacent model in Regression tab was fixed. Matching criterion
    and parametrization inputs now have effect on model.
  * Bug in `plotMultinomial()` was fixed. Function now displays more than 12
    response patterns as linetype.

#### MINOR UPDATES
  * Range for x-axes in `plotAdjacent()` and `plotCumulative()` were updated to
    avoid removing data.
  * Some typos were fixed.

----------

### Changes in version 1.3.1-2. (2019-07-01)

#### MAJOR UPDATES
  * Cronbach's alpha with CI was added into reports.
  * Bonferroni correction for multiple comparison was added as an option
    for correction methods in Regression, DIF and Reports tabs.

#### MINOR UPDATES
  * Warning for table for parameters in Bock's IRT model was modified.
  * Wright map now considers item names when option Keep item names.
  * Some wording was updated.
  * Some typos were fixed.

----------

### Changes in version 1.3.1-1. (2019-06-28)

#### BUG FIXING
  * Minimal number of groups for gULI in DDplot was set to 2 in both Item
    analysis and Report tabs.

#### MAJOR UPDATES
  * Mantel-Haenszel test was added into reports.

#### MINOR UPDATES
  * Error for DIF/Logistic was customized.
  * Text in reports in DIF section was updated.
  * Tables in DIF section now includes both p-values and adjusted p-values
    when correction method is applied.
  * Warning for fancyhdr latex package was solved.

----------

### Changes in version 1.3.1. (2019-06-26)

**_THIS IS A CRAN VERSION_**

*It includes versions 1.3.0-1 - 1.3.0-18 and following changes:*

#### MINOR UPDATES
  * In `ItemAnalysis()` function, documentation for item reliability and
    item validity indices has been added (Reported by Lot Fonteyne).

----------

### Changes in version 1.3.0-18. (2019-06-25)

#### BUG FIXING
  * Bug in `plotDistractorAnalysis()` was fixed. X-ticks are now displayed
    even with discrete = T argument.

#### MINOR UPDATES
  * Inputs in Validity/Correlation overlaped when window was narrow. Fixed.
  * Generate and Download buttons overlaped in Reports when window was narrow.
    Fixed.

----------

### Changes in version 1.3.0-17. (2019-06-24)

#### BUG FIXING
  * `DistractorAnalysis()` and `plotDistractorAnalysis()` now handle discrete
    matching and groups specified via argument cut.points.
  * Distractor plots in Validity/Items tab now properly handle discrete
    criterion and the number of groups is correctly descreased when criterion
    does not have unique cuts.
  * Bug in Raju in `DIF/Fairness` tab was fixed. Plots for items are now
    displayed for dataBov dataset.

#### MAJOR UPDATES
  * Checking of unique cuts for distractor plots was added into Reports.
    Warning is displayed when cuts are not unique and slider is updated.

#### MINOR UPDATES
  * Some typos were fixed.

----------

### Changes in version 1.3.0-16. (2019-06-21)

#### BUG FIXING
  * Bug in Bock's model for binary items was fixed in reports.
  * Histogram when some levels of total scores are missing were fixed
    in Summary and DIF tab.

#### MAJOR UPDATES
  * Legend was added into IRT plots when downloaded.
  * IRT models now respect ordering of items.

#### MINOR UPDATES
  * Some typos in IRT models tab were fixed.

----------

### Changes in version 1.3.0-15. (2019-06-20)

#### BUG FIXING
  * DIF summary table bug was fixed. Table is disaplayed when
    some DIF methods cannot be fitted.
  * DDF parameter tables were fixed for data BOV and Czech Trivia.
  * With `difNLR` package version 1.2.3 plots for DDF are displayed
    even for binary data. Plots were also fixed for data Bov.
  * PDF Reports now work locally and on shinyapps.io.
  * Dendrogram now properly replaces item names when Keep item names
    is not selected in Data upload section.

#### MINOR UPDATES
  * Authors' and contributors' details were updated.
  * Function `plotMultinomial()` was updated to return smooth curves
    for any matching criterion.

----------

### Changes in version 1.3.0-14. (2019-06-19)

#### BUG FIXING
  * Bug in Bock's nominal IRT model was fixed. ICCs are now properly
    displayed for all binary data. Error for non-displayable
    table of parameters was further improved. Tables for items are
    displayed even in case that table for all items is not.
  * DIF items are now correctly written in reports.
  * Corrplot now properly replaces item names when Keep item names
    is not selected in Data upload section.
  * In `DIF/Fairness` section, histograms of Total scores for groups
    are now properly displayed also for ordinal items.

#### MAJOR UPDATES
  * Legends for multinomial and distractor plots were improved.

#### MINOR UPDATES
  * Authors' and contributors' details were updated.

----------

### Changes in version 1.3.0-13. (2019-06-18)

#### BUG FIXING
  * Bug in Bock's nominal IRT model was fixed. ICCs are now properly
    displayed for Czech Trivia dataset. Error for non-displayable
    table of parameters was improved.
  * Bug in `DistractorAnalysis()` was partially fixed, need to be revised.

#### MAJOR UPDATES
  * Reports:
      - long tables have now header on each new page
      - legend is now visible for IRT plots even when dataset consists
        of more than 50 items
  * Matching criterion in `plotMultinomial()` was relaxed to decrease
    size of reports.

----------

### Changes in version 1.3.0-12. (2019-06-17)

#### BUG FIXING
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

#### MAJOR UPDATES
  * Uploaded data can be now unloaded without restarting app.
  * Distractor plot for validity analysis was updated to allow
    for lowering the number of categories in cases when cuts
    are not uniquely defined. Application was updated accordingly.

#### MINOR UPDATES
  * Errors and warnings were customized.
  * In all dichotomous models the number of EM cycles can be
    increased in Settings.
  * Seed was set for validity boxplot for reproducibility of
    jitter points.
  * All static DOI links were updated.
    (Thanks to Katrin Leinweber)
  * Some typos were fixed in reports.

----------

### Changes in version 1.3.0-11. (2019-05-28)

#### BUG FIXING
  * Option for displaying session info in reports now functions properly.
  * Option to display average score in DDplot in reports fixed.

#### MAJOR UPDATES
  * New function `plotCumulative()` which plots category probabilities
    functions estimated by \code{multinom()} from `nnet` package
    was added. It is now used in application.
  * New function `plotAdjacent()` which plots category probabilities
    functions estimated by \code{multinom()} from `nnet` package
    was added. It is now used in application.
  * New function `plotMultinomial()` which plots category probabilities
    functions estimated by \code{multinom()} from `nnet` package
    was added. It is now used in application.

#### MINOR UPDATES
  * Selected R codes for cumulative, adjacent and multinomial models
    were updated.
  * Option for matching criterion (total vs Z-score) in multinomial
    model was added.
  * Equations in DIF section are now center-aligned.

----------

### Changes in version 1.3.0-10. (2019-05-17)

#### MAJOR UPDATES
  * Alerts for analysis where data with missing values are omitted
    were added.
  * `DDplot()` now can displayed average item score as estimate of
    difficulty for ordinal data. See argument average.score.

#### MINOR UPDATES
  * Correlation structure now use pairwise complete observations
    for calculation of correlation.

----------

### Changes in version 1.3.0-9. (2019-05-08)

#### MAJOR UPDATES
  * `DistractorAnalysis()` and `plotDistractorAnalysis()` functions were
    updated to allow for lowering the number of categories in cases when
    cuts are not uniquely defined. Application was updated accordingly.

#### MINOR UPDATES
  * Tables and equations of polytomous models in Regression section are
    now center-aligned.

----------

### Changes in version 1.3.0-8. (2019-05-07)

#### BUG FIXING
  * With option `Keep missing values` unfunctioning analyses were fixed
    (missing data removed, warning message added)

#### MAJOR UPDATES
  * Multinomial regression plot is now displayed using smooth function

#### MINOR UPDATES
  * With option `Keep missing values` warning message is now displayed
    for those analyses for which missing values are omitted.
  * For ordinal data, Cronbach's alpha is now calculated from original
    ordinal (non-binarized) data both in Reliability and in Item analysis
    section.

----------

### Changes in version 1.3.0-7. (2019-04-19)

#### BUG FIXING
  * Busy indicator now properly displays above input tabs.
  * Bug when using matching in `DistractorAnalysis()` was fixed.

#### MAJOR UPDATES
  * New ordinal toy dataset SCIENCE was added.

#### MINOR UPDATES
  * Each tab is now open at the top of the page.

----------

### Changes in version 1.3.0-6. (2019-04-18)

#### MAJOR UPDATES
  * Adjacent logistic regression model was implemented for ordinal data.
  * Both adjacent and cummulative regression models allow for selection
    of matching criterion (total score, standardized total score) and
    parametrization (classical slope/intercept and IRT parametrization).

----------

### Changes in version 1.3.0-5. (2019-04-03)

#### BUG FIXING
  * SD in Regression section was changed to SE.
  * Keep missing values button is now functioning and it is applied
    for ordinal and binary data.

#### MAJOR UPDATES
  * Cumulative logistic regression model was implemented for ordinal data.
  * Delta method is now applied for calculation of SE in IRT parametrization
    in cumulative logistic regression.

----------

### Changes in version 1.3.0-4. (2019-03-28)

#### BUG FIXING
  * Bug in slider in histogram of total scores was fixed. Range is now
    correctly displayed when NA values are present.
  * Fixing versions.

#### MAJOR UPDATES
  * Math mode is now stable in tables.
    (Thanks to Stéphane Laurent)
  * UI for polytomous models in Regression section was added.
  * Traditional item analysis table is done via ItemAnalysis() function.
    It now includes also average score.

#### MINOR UPDATES
  * `startShinyItemAnalysis()` automatically launches default web browser.
  * Sample code and wording was updated for traditional item analysis table
    in app and also in reports.

----------

### Changes in version 1.3.0-3. (2019-03-26)

#### MINOR UPDATES
  * `ggWrightMap()` now can display user-specified names of items
    (Reported by Matthew G R Courtney)
  * `plotDIFLogistic()` now can display user-specified names
    of reference and focal group.
  * Some typos were fixed.

----------

### Changes in version 1.3.0-2. (2019-03-15)

#### MAJOR UPDATES
  * Analysis for ordinal items has been added. This includes use of ordinal
    data in sections Summary (histogram of Total scores, success rate),
    Reliability, Validity (correlations, predictive validity),

----------

### Changes in version 1.3.0-1. (2019-03-08)

#### BUG FIXING
  * Equations for 3PL and 4PL regression models were fixed.

#### MAJOR UPDATES
  * Math symbols are now displayed using MathJax.
  * Exercises were added in IRT section / Polytomous Training.
  * DIF section now includes method comparison.

#### MINOR UPDATES
  * Labeling of correction methods in DIF section was improved.
  * Ability parameters are now available also for Bock's model.

----------

### Changes in version 1.3.0. (2019-02-25)

**_THIS IS A CRAN VERSION_**

*It includes versions 1.2.9-1 - 1.2.9-3 and following changes:*

#### MINOR UPDATES
  * Reference to a new paper on `ShinyItemAnalysis` in The R journal was added.

----------

### Changes in version 1.2.9-3. (2019-01-30)

#### BUG FIXING
  * Histograms for total scores now handle cut-scores out of range of
    observed values
  * Bug in reports when using delta plot with purification was fixed.
  * Cut-scores for histograms are properly displayed in reports.
  * Year in reports was updated.

#### MAJOR UPDATES
  * Sections About, References and Settings are displayed only with icon
    and are right-aligned.
  * Margins and padding were updated.
  * t-test was added for comparison of total scores between groups
    into DIF section and into reports.
  * Histograms for total scores are now displayed in plotly mode.

#### MINOR UPDATES
  * Some math mode was fixed in IRT section.
  * Some typos were fixed.
  * Selected R code was updated in DIF/Total scores.
  * Animate function for sliders was slowed down.

----------

### Changes in version 1.2.9-2. (2019-01-11)

#### BUG FIXING
  * Generalized ULI index is now correctly displayed in traditional
    item analysis table in reports.
  * Item names are now correctly displayed in reports.
  * Item names are now correctly displayed in DDF section.
  * Function `plotDIFLogistic()` now handles user-specified matching
    criterion.

#### MINOR UPDATES
  * Legend of DDplot was modified.

----------

### Changes in version 1.2.9-1. (2018-12-10)

#### MINOR UPDATES
  * Wording in PDF/HTML reports was edited.

----------

### Changes in version 1.2.9. (2018-12-06)

#### BUG FIXING
  * IRT/Training/Dichotomous models - plots are correctly plotted.
  * Download buttons in IRT/4PL works now.

#### MAJOR UPDATES
  * TIF were converted into plotly.
  * Section Validity/FA is not accessible for now.

#### MINOR UPDATES
  * Selected R code for IRT/Bock's model was updated.
  * Wording in IRT models was updated.
  * Wording in Reports was updated.
  * Legend for plots of IRT models was modified in Reports.
  * Ability estimates summary table is now round in Reports.
  * Some typos were fixed in Reports.
  * Colours of IRT summary plots correspond to model for Items.


----------

### Changes in version 1.2.8-7. (2018-12-05)

#### MAJOR UPDATES
  * IRT section of shiny app was updated and now provides
    new and more shiny figures in plotly
  * PDF and HTML reports were updated and now allow
    - different types of correlation in corrplots
    - different types of discrimination indices in DDplots
    - summary table for ability estimates in IRT section

----------

### Changes in version 1.2.8-6. (2018-12-03)

#### BUG FIXING
  * Dendrogram is now correctly rendered.

#### MAJOR UPDATES
  * Function `ItemAnalysis()` was added.
  * IRT section now allows download of ability estimates

#### MINOR UPDATES
  * Help pages for functions were updated.

----------

### Changes in version 1.2.8-5. (2018-11-29)

#### BUG FIXING
  * Key for ordinal data for binarization is now correctly used. Values
    greater or equal to value of the key are set to 1.
  * When NULL data is uploaded, default dataset GMAT is in use.

#### MAJOR UPDATES
  * Scree plot was moved into the new tab `Factor analysis`.
  * Dendrogram was added into `Correlation structure` tab.
  * Option for global cut-score for ordinal data was added.
  * R package ggdendro is now used.
  * Removal of items with only 0s or 1s now works.
  * Removal of observation with NA group now works.

#### MINOR UPDATES
  * Text in Data section was updated.
  * List of references was updated.
  * Selected R code in `Correlation structure` tab was updated.

----------

### Changes in version 1.2.8-4. (2018-11-23)

#### BUG FIXING
  * Bug in IRT models/Training/Dichotomous models text of theta in
    plotly was fixed.

#### MAJOR UPDATES
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

#### MINOR UPDATES
  * Updating text in Data section.
  * Editing boxes for data upload in Data section.

----------

### Changes in version 1.2.8-3. (2018-11-20)

#### BUG FIXING
  * Bug in Regression/Nonlinear 4P IRT Z (rounding in interpretation)
    was fixed.
  * Bug in IRT models/Training/Dichotomous models text of theta in
    plotly was fixed.
  * Bug in IRT models/Training in plotly a ModeBar was fixed.

#### MAJOR UPDATES
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

#### MINOR UPDATES
  * Data upload now better points to "Upload data" button.
  * Precision was re-set for IRT models/Training tab. Now = 0.1.

----------

### Changes in version 1.2.8-2. (2018-11-05)

#### MAJOR UPDATES
  * `gDiscrim()` function has been extended to calculate Upper-Lower
    Index (ULI) also for ordinal items

----------

### Changes in version 1.2.8-1. (2018-10-01)

#### BUG FIXING
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

#### MAJOR UPDATES
  * Global setting for figure downloads was created. This can be changed
    in Setting section.

----------

### Changes in version 1.2.8 (2018-09-27)

**_THIS IS A CRAN VERSION_**

*It includes versions 1.2.7-1 - 1.2.7-7*

----------

### Changes in version 1.2.7-7. (2018-09-27)

#### MAJOR UPDATES
  * Section Reliability was added.
    - Split-half methods including also Revelle's beta were added
    - Cronbach's alpha method with CI was moved to section Reliability.
    - Spearman-Brown formula was added.
  * New function ggWrightMap replaced wrightMap from WrightMap package.
  * Section IRT models
    - Selected R code for Rasch and 1PL models was updated.

#### MINOR UPDATES
  * Reference list was updated.

----------

### Changes in version 1.2.7-6. (2018-08-29)

#### MAJOR UPDATES
  * Section DIF/Fairness
    - Subsection Logistic IRT Z is deprecated.
    - Subsection Nonlinear IRT Z is now called Generalized logistic.
    - Subsection Generalized logistic includes now wide range of models
      and different specification of DIF type.
    - Subsection SIBTEST was added, including SIBTEST method and
      Crossing-SIBTEST method.

#### MINOR UPDATES
  * Reference list was updated.

----------

### Changes in version 1.2.7-5. (2018-08-22)

#### MINOR UPDATES
  * Some wording was updated.
  * Selected R code was updated.
  * Function theme_shiny is deprecated.
    New function `theme_app()` was created and applied for all
    graphics, including R functions in package.

----------

### Changes in version 1.2.7-4. (2018-08-10)

#### MINOR UPDATES
  * Paddings were adjusted.
  * Acknowledgments were updated.

----------

### Changes in version 1.2.7-3. (2018-07-27)

#### BUG FIXING
  * Crashing of the app when upload no data was fixed.

#### MAJOR UPDATES
  * Data page
    - Design of data page was changed.
    - User can now specify encoding of missing values.
    - Basic summary was added.

----------

### Changes in version 1.2.7-2. (2018-07-04)

#### MAJOR UPDATES
  * Phone version
    - collapsible menu closes when clicked

----------

### Changes in version 1.2.7-1. (2018-07-02)

#### BUG FIXING
  * Plotly files in IRT training section are now displayed with
    mode bar due a bug in plotly package.

#### MINOR UPDATES
  * Some typos were fixed.
  * Link to google form for feedback was added into About section.
  * New R icon was added into footer.

----------

### Changes in version 1.2.7. (2018-05-03)

**_THIS IS A CRAN VERSION_**

*It includes versions 1.2.6-1 - 1.2.6-12*

----------

### Changes in version 1.2.6-12. (2018-04-26)

#### MINOR UPDATES
  * Mirror at Charles University is no longer in use.

----------

### Changes in version 1.2.6-11. (2018-04-24)

#### MAJOR UPDATES
  * Function `DDplot()`
    - Limits for y-axis were adjusted when negative values of discrimination
      are detected.
    - Warning for negative discrimination values were added.
  * Reports
    - Some unused parts of reports were excluded.

----------

### Changes in version 1.2.6-10. (2018-04-19)

#### MAJOR UPDATES
  * UI
    - Some parts were splitted from main ui.R file
  * IRT
    - Exercises in IRT training were extended.
  * Data
    - Bug in data upload was fixed. Now length of criterion variable
      is correctly checked.

----------

### Changes in version 1.2.6-9. (2018-04-16)

#### MAJOR UPDATES
  * Packages - shinyBS is now required.
  * Data
    - HCI dataset was added into package.
    - Description of dataMedical was improved.
    - Description of training datasets was improved.
  * About
    - Page was improved.
  * IRT
    - Helps for interactive exercises were added.

#### MINOR UPDATES
  * Typos were fixed.

----------

### Changes in version 1.2.6-8. (2018-04-13)

#### MAJOR UPDATES
  * IRT
    - Expected item scores were removed from IRT training NRM.
    - Interactive exercises were added into  IRT training - dichotomous
      models.

----------

### Changes in version 1.2.6-7. (2018-04-12)

#### MAJOR UPDATES
  * Data
    - Graded data od Medical data set was added.
    - In all dataMedical data sets the criterion variable was renamed to
      StudySuccess.
  * IRT
    - Expected item scores were added into polytomous models in IRT training.
    - Selected R code was updated in IRT training.

#### MINOR UPDATES
  * IRT section
    - Colors of sliders and curves in IRT training section were adjusted.

----------

### Changes in version 1.2.6-6. (2018-04-10)

#### MAJOR UPDATES
  * Regression
    - NLR 4P model was added into model comparison.

#### MINOR UPDATES
  * README file was updated. Typos were fixed.
  * Design of figures was unified.
  * Download button options were unified.
  * Selected R code syntax was improved in some sections
    (IRT only partionally, DIF not).
  * Selected R code was improved for first 4 sections.
  * Some wording was improved.

----------

### Changes in version 1.2.6-5. (2018-04-09)

#### MAJOR UPDATES
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

#### MINOR UPDATES
  * README file was updated. Typos were fixed. Links were added.
    Hexbin was added.
  * References were updated.

----------

### Changes in version 1.2.6-4. (2018-04-06)

#### MAJOR UPDATES
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

#### MINOR UPDATES
  * IRT section
    - Selected R code in model comparison was updated. Typos were fixed.
  * Regression section
    - Interpretation of NLR 3P model was extended.
    - Syntax for NLR 3P model was improved.

----------

### Changes in version 1.2.6-3. (2018-04-05)

#### MAJOR UPDATES
  * IRT section
    - IRT section is newly organized.
    - IRT training section was expanded by polytomous models.
      GRM, GPCM and NRM were added.
    - Syntax for IRT training was improved.

#### MINOR UPDATES
  * IRT section
    - Sliders steps are now 0.01 in dichotomous models.

----------

### Changes in version 1.2.6-2. (2018-04-03)

#### MINOR UPDATES
  * Numeric inputs were removed from IRT training section.
  * Selected R code was extended in IRT training section by item
    information function plot.
  * References were updated.
  * Mirror web link was updated.

----------

### Changes in version 1.2.6-1. (2018-03-15)

#### BUG FIXING
  * Reports with DDF but none DDF item are now correctly generated.

#### MINOR UPDATES
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
    - Some parameters were renamed. The unneecessary ones were removed.

------

### Changes in version 1.2.6 (2018-03-01)

#### MAJOR UPDATES
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

#### MINOR UPDATES
  * Reference list was updated.
  * CITATION file was modified.
  * In Summary page the description for summary table was added. The sample
    code was updated.
  * Wording in summary statistics in Summary section was improved.
  * Description of Session info section in reports was added.

------

### Changes in version 1.2.5 (2017-11-14)

#### MAJOR UPDATES
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

#### MINOR UPDATES
  * Link for Adela's website was added.
  * Columns for BIC and its adjusted version were swapped in table for
    IRT models comparison. Text above table was also updated.
  * In IRT section dpi of downloaded plots was set to 300.
  * In IRT training downloaded plots also include legend with parameters.
  * In all IRT models table with parameters includes all parameters
    a, b, c, d.
  * In IRT training equations were moved above interpretation.

------

### Changes in version 1.2.4 (2017-08-28)

#### MINOR UPDATES

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

------

### Changes in version 1.2.3 (2017-08-22)

**_THIS IS A CRAN VERSION_**

#### MAJOR UPDATES
  * Report's customized options were extended. Reports now include also
    dynamic description.
  * Author and dataset name can be now filled and add into reports.
  * Dataset `difMedical` was renamed to `MSAT-B` according to `difNLR`
    package. Its description was mildly updated.
  * Purification options were added into `Nonlinear IRT Z` and `DDF` subsections
    of `DIF/Fairness` sections.
  * Purification options was added `DDF` analysis in `Reports` section when customized
    setting is selected.
    
#### MINOR UPDATES

  * Size of plots and font in plots was changed for reports.
  * Plots in HTML report are now centered.
  * Wording of reports' default option was updated.
  * References were updated in `Reference` section and also in README file.
  * Wording of Spearman test in `Validity` section was mildly updated.

------

### Changes in version 1.2.2 (2017-07-26)

#### MAJOR UPDATES
  * Design of reports was edited.
  * Several bugs were fixed:
     - Bug when creating report for uploaded data was fixed.
     - Bug when creating report for data without predictive
       criterion or DIF group was fixed.
  * `ShinyItemAnalysis` now uses `plotly` package!

#### MINOR UPDATES
  * Design and options of `Report` page were updated.
  * Layout of IRT curves training page was mildly changed. There are now
    two curves in each plot. Also text area input was added and synchro-
    nized with sliders. Colour of sliders now correspond to colour of
    curve.

------

### Changes in version 1.2.1 (2017-07-16)

#### MAJOR UPDATES
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

#### MINOR UPDATES
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

------

### Changes in version 1.2.0 (2017-06-15)

**_THIS IS A CRAN VERSION_**

#### MAJOR UPDATES
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

#### MINOR UPDATES
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

------

### Changes in version 1.1.0 (2016-02-02)

**_THIS IS A CRAN VERSION_**

#### MAJOR UPDATES
  *  New overall appearence using shinythemes package.
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

#### MINOR UPDATES
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

------

### Changes in version 1.0.0 (2016-11-25)

**_THIS IS A CRAN VERSION_**

#### MAJOR UPDATES
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
