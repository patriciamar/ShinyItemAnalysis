uiReferences <-
  tabPanel("",
    value = "references",
    icon = icon("fas fa-book"),
    #------------------------------------------------------------------------------------#
    # Packages ####
    #------------------------------------------------------------------------------------#
    h3("R packages"),
    HTML('<ul class = "biblio">
                <li><code>cowplot</code>
                Wilke, C.O. (2020).
                cowplot: Streamlined plot theme and plot annotations for "ggplot2".
                R package version 1.1.1.
                <a href = " https://CRAN.R-project.org/package=cowplot", target = "_blank">See online.</a>
                </li>

                <li><code>data.table</code>
                Dowle, M., & Srinivasan, A. (2020).
                data.table: Extension of "data.frame".
                R package version 1.13.6.
                <a href = "https://CRAN.R-project.org/package=data.table", target = "_blank">See online.</a>
                </li>

                <li><code>deltaPlotR</code>
                Magis, D., & Facon, B. (2014).
                deltaPlotR: An R package for differential item functioning analysis with Angoff`s delta plot.
                <i>Journal of Statistical Software, Code Snippets, 59</i>(1), 1-19.
                <a href = "http://www.jstatsoft.org/v59/c01/", target = "_blank">See online.</a>
                </li>

                <li><code>difNLR</code>
                Hladka, A., Martinkova, P. (2020).
                difNLR: Generalized logistic regression models for DIF and DDF detection.
                <i>The R Journal, 12</i>(1), 300-323.
                <a href = "https://doi.org/10.32614/RJ-2020-014", target = "_blank">See online.</a>
                </li>

                <li><code>difR</code>
                Magis, D., Beland, S., Tuerlinckx, F., & De Boeck, P. (2010).
                A general framework and an R package for the detection of dichotomous differential item functioning.
                <i>Behavior Research Methods, 42</i>847-862.
                </li>

                <li><code>DT</code>
                Xie, Y., Cheng, J., & Tan, X. (2021).
                DT: A wrapper of the JavaScript library "DataTables".
                R package version 0.17.
                <a href = "https://CRAN.R-project.org/package=DT", target = "_blank">See online.</a>
                </li>

                <li><code>ggdendro</code>
                de Vries, A., & Ripley, B.D. (2020).
                ggdendro: Create dendrograms and tree diagrams using "ggplot2".
                R package version 0.1-22.
                <a href = "https://CRAN.R-project.org/package=ggdendro", target = "_blank">See online.</a>
                </li>

                <li><code>ggplot2</code>
                Wickham, H. (2016).
                ggplot2: Elegant graphics for data analysis.
                <a href = "http://ggplot2.org", target = "_blank">See online.</a>
                </li>

                <li><code>gridExtra</code>
                Auguie, B. (2017).
                gridExtra: Miscellaneous functions for "grid" graphics.
                R package version 2.3.
                <a href = "https://CRAN.R-project.org/package=gridExtra", target = "_blank">See online.</a>
                </li>

                <li><code>knitr</code>
                Xie, Y. (2020).
                knitr: A general-purpose package for dynamic report generation in R.
                R package version 1.30.
                <a href = "https://yihui.name/knitr/", target = "_blank">See online.</a>
                </li>

                <li><code>latticeExtra</code>
                Sarkar, D., & Andrews, F. (2019).
                latticeExtra: Extra graphical utilities based on lattice.
                R package version 0.6-29.
                <a href = "https://CRAN.R-project.org/package=latticeExtra", target = "_blank">See online.</a>
                </li>

                <li><code>lme4</code>
                Bates, D., Maechler, M., Bolker, B., & Walker, S. (2015).
                Mixed-effects models using lme4.
                Journal of Statistical Software, 67(1), 1-48.
                <a href = "https://doi.org/10.18637/jss.v067.i01", target = "_blank">See online.</a>
                </li>

                <li><code>ltm</code>
                Rizopoulos, D. (2006).
                ltm: An R package for latent variable modelling and item response theory analyses.
                <i>Journal of Statistical Software, 17</i>(5), 1-25.
                <a href = "http://www.jstatsoft.org/v17/i05/", target = "_blank">See online.</a>
                </li>

                <li><code>magrittr</code>
                Bache, S. M., & Wickham, H. (2020).
                magrittr: A forward-pipe operator for R.
                R package version 2.0.1.
                <a href = " https://CRAN.R-project.org/package=magrittr", target = "_blank">See online.</a>
                </li>

                <li><code>mirt</code>
                Chalmers, R., & Chalmers, P. (2012).
                mirt: A multidimensional item response theory package for the R environment.
                <i>Journal of Statistical Software, 48</i>(6), 1-29.
                </li>

                <li><code>msm</code>
                Jackson, C., & Jackson, H. (2011).
                Multi-state models for panel data: The msm package for R.
                <i>Journal of Statistical Software, 38</i>(8), 1-29.
                <a href = "http://www.jstatsoft.org/v38/i08/", target = "_blank">See online.</a>
                </li>

                <li><code>nnet</code>
                Venables, C., & Ripley, C. (2002).
                Modern applied statistics with S.
                <a href = "http://www.stats.ox.ac.uk/pub/MASS4", target = "_blank">See online.</a>
                </li>

                <li><code>plotly</code>
                Sievert, C. (2020).
                Interactive web-based data visualization with R, plotly, and shiny.
                Chapman and Hall/CRC Florida, 2020.
                <a href = "https://plotly-r.com", target = "_blank">See online.</a>
                </li>

                <li><code>psych</code>
                Revelle, W. (2020).
                psych: Procedures for psychological, psychometric, and personality research.
                R package version 2.0.12.
                <a href = "https://CRAN.R-project.org/package=psych", target = "_blank">See online.</a>
                </li>

                <li><code>purrr</code>
                Henry, L., & Wickham, H. (2020).
                purrr: Functional programming tools.
                R package version 0.3.4.
                <a href = "https://CRAN.R-project.org/package=purrr", target = "_blank">See online.</a>
                </li>

                <li><code>rlang</code>
                Henry, L., & Wickham, H. (2020).
                rlang: Functions for base types and core R and "tidyverse" features.
                R package version 0.4.10.
                <a href = " https://CRAN.R-project.org/package=rlang", target = "_blank">See online.</a>
                </li>

                <li><code>rmarkdown</code>
                Xie, Y., Allaire, J.J., & Grolemund G. (2018).
                R Markdown: The definitive guide.
                Chapman and Hall/CRC. ISBN 9781138359338.
                <a href = "https://bookdown.org/yihui/rmarkdown", target = "_blank">See online.</a>
                </li>

                <li><code>rstudioapi</code>
                Ushey, K., Allaire J.J., Wickham, H., & Ritchie G. (2018).
                rstudioapi: Safely access the RStudio API.
                R package version 0.13.
                <a href = " https://CRAN.R-project.org/package=rstudioapi", target = "_blank">See online.</a>
                </li>

                <li><code>scales</code>
                Wickham, H., & Seidel D. (2020).
                scales: Scale functions for visualization.
                R package version 1.1.1.
                <a href = " https://CRAN.R-project.org/package=scales", target = "_blank">See online.</a>
                </li>

                <li><code>shiny</code>
                Chang, W., Cheng, J., Allaire, J., Xie, Y., & McPherson, J. (2020).
                shiny: Web application framework for R.
                R package version 1.5.0.
                <a href = "https://CRAN.R-project.org/package=shiny", target = "_blank">See online.</a>
                </li>

                <li><code>shinyBS</code>
                Bailey, E. (2015).
                shinyBS: Twitter bootstrap components for shiny.
                R package version 0.61.
                <a href = "https://CRAN.R-project.org/package=shinyBS", target = "_blank">See online.</a>
                </li>

                <li><code>shinydashboard</code>
                Chang, W., & Borges Ribeiro, B. (2018).
                shinydashboard: Create dashboards with "shiny".
                R package version 0.7.1
                <a href = "https://CRAN.R-project.org/package=shinydashboard", target = "_blank">See online.</a>
                </li>

                <li><code>ShinyItemAnalysis</code>
                Martinkova, P., & Drabinova, A. (2018).
                ShinyItemAnalysis for teaching psychometrics and to enforce routine analysis of educational tests.
                <i>The R Journal, 10</i>(2), 503-515.
                <a href = "https://doi.org/10.32614/RJ-2018-074", target = "_blank">See online.</a>
                </li>

                <li><code>shinyjs</code>
                Attali, D. (2020).
                shinyjs: Easily improve the user experience of your shiny apps in seconds.
                R package version 2.0.0.
                <a href = "https://CRAN.R-project.org/package=shinyjs", target = "_blank">See online.</a>
                </li>

                <li><code>stringr</code>
                Wickham, H. (2019).
                stringr: Simple, consistent wrappers for common string operations.
                R package version 1.4.0.
                <a href = "https://CRAN.R-project.org/package=stringr", target = "_blank">See online.</a>
                </li>

                <li><code>tibble</code>
                M&uuml;ller, K., & Wickham, H. (2020).
                tibble: Simple data frames.
                R package version 3.0.4.
                <a href = " https://CRAN.R-project.org/package=tibble", target = "_blank">See online.</a>
                </li>

                <li><code>tidyr</code>
                Wickham, H. (2020).
                tidyr: Tidy messy data.
                R package version 1.1.2.
                <a href = " https://CRAN.R-project.org/package=tidyr", target = "_blank">See online.</a>
                </li>

                <li><code>VGAM</code>
                Yee, T. W. (2015).
                Vector Generalized linear and additive models: With an implementation in R.
                New York, USA: Springer.
                <a href = "https://CRAN.R-project.org/package=VGAM", target = "_blank">See online.</a>
                </li>

                <li><code>xtable</code>
                Dahl, D., Scott, D., Roosen, C., Magnusson, A., & Swinton, J. (2019).
                xtable: Export tables to LaTeX or HTML.
                R package version 1.8-4.
                <a href = "https://CRAN.R-project.org/package=xtable", target = "_blank">See online.</a>
                </li>
                </ul>'),
    #------------------------------------------------------------------------------------#
    # References ####
    #------------------------------------------------------------------------------------#
    h3("References"),
    HTML('<ul class = "biblio">
                <li>Akaike, H. (1974).
                A new look at the statistical model identification.
                <i>IEEE Transactions on Automatic Control, 19</i>(6), 716-723.
                <a href = "http://ieeexplore.ieee.org/abstract/document/1100705/",
                target = "_blank">See online.</a>
                </li>

                <li>Ames, A. J., & Penfield, R. D. (2015).
                An NCME instructional module on item-fit statistics for item
                response theory models.
                <i>Educational Measurement: Issues and Practice, 34</i>(3), 39-48.
                <a href = "http://onlinelibrary.wiley.com/doi/10.1111/emip.12067/full",
                target = "_blank">See online.</a>
                </li>

                <li>Andrich, D. (1978).
                A rating formulation for ordered response categories.
                <i>Psychometrika, 43</i>(4), 561-573.
                <a href = "https://link.springer.com/article/10.1007/BF02293814",
                target = "_blank">See online.</a>
                </li>

                <li>Angoff, W. H., & Ford, S. F. (1973).
                Item-race interaction on a test of scholastic aptitude.
                <i>Journal of Educational Measurement, 10</i>(2), 95-105.
                <a href = "https://www.jstor.org/stable/1433905?seq=1#page_scan_tab_contents",
                target = "_blank">See online.</a>
                </li>

				        <li>Bartholomew, D., Steel, F., Moustaki, I., & Galbraith, J. (2002).
				        The analysis and interpretation of multivariate data for social
				        scientists.
				        London: Chapman and Hall.
                </li>

                <li>Bartholomew, D. J, Steele, F., Moustaki, I. (Eds.) (2008).
                <i>Analysis of Multivariate Social Science Data</i> (2nd ed.)
                CRC Press.
                </li>

                <li>Bock, R. D. (1972).
                Estimating item parameters and latent ability when responses are
                scored in two or more nominal categories.
                <i>Psychometrika, 37</i>(1), 29-51.
                <a href = "http://link.springer.com/article/10.1007/BF02291411",
                target = "_blank">See online.</a>
                </li>

                <li>Brown, W. (1910).
                Some experimental results in the correlation of mental abilities.
                <i>British Journal of Psychology, 1904-1920, 3</i>(3), 296-322.
                <a href = "https://onlinelibrary.wiley.com/doi/abs/10.1111/j.2044-8295.1910.tb00207.x",
                target = "_blank">See online.</a>
                </li>

                <li>Cattell, R. B. (1966). The scree test for the number of factors.
                <i>Multivariate Behavioral Research, 1,</i> 245–276.
                <a href = "http://dx.doi.org/10.1207/s15327906mbr0102_10",
                target = "_blank">See online.</a>
                </li>

                <li>Chalmers, R. P. (2018).
                Improving the crossing-SIBTEST statistic for detecting non-uniform DIF.
                <i>Psychometrika, 83</i>(2), 376-386.
                <a href = "https://link.springer.com/article/10.1007/s11336-017-9583-8",
                target = "_blank">See online.</a>
                </li>

                <li>Cronbach, L. J. (1951).
                Coefficient alpha and the internal structure of tests.
                <i>Psychometrika, 16</i>(3), 297-334.
                <a href = "https://link.springer.com/article/10.1007/BF02310555",
                target = "_blank">See online.</a>
                </li>

                <li>Drabinova, A., & Martinkova, P. (2017).
                Detection of differential item functioning with non-linear
                regression: Non-IRT approach accounting for guessing.
                <i>Journal of Educational Measurement, 54</i>(4), 498-517.
                <a href = "https://doi.org/10.1111/jedm.12158",
                target = "_blank">See online.</a>
                </li>

                <li> Erosheva, E. A, Martinkova, P., & Lee, C. J. (2021).
                When zero may not be zero: A cautionary note on the useof inter-rater reliability in evaluating grant peer review.
                <i>Journal of the Royal Statistical Society: Series A, 184</i>(3), 904-919.
                <a href = "http://doi.org/10.1111/rssa.12681",
                target = "_blank">See online.</a>
                </li>

                <li> Feldt, L. S., Woodruff, D. J., & Salih, F. A. (1987).
                Statistical inference for coefficient alpha.
                <i>Applied Psychological Measurement 11</i>(1), 93-103.
                <a href = "http://journals.sagepub.com/doi/abs/10.1177/014662168701100107",
                target = "_blank">See online.</a>
                </li>

                <li>
                Horn, J. L. (1965). A rationale and test for the number of factors in factor analysis. <i>Psychometrika, 30,</i> 179–185. <a href="http://dx.doi.org/10.1007/BF02289447", target="_blank">See online.</a>
                </li>

                <li>
                Kaiser,  H.  F.  (1960).  The  application  of  electronic  computers  to  factor analysis. <i>Educational and Psychological Measurement, 20</i>, 141–151. <a href="http://dx.doi.org/10.1177/001316446002000116", target="_blank">See online.</a>
                </li>

                <li>Li, H.-H., & Stout, W. (1996).
                A new procedure for detection of crossing DIF.
                <i>Psychometrika, 61</i>(4), 647-677.
                <a href = "https://link.springer.com/article/10.1007/BF02294041",
                target = "_blank">See online.</a>
                </li>

                <li>Lord, F. M. (1980).
                Applications of item response theory to practical testing problems.
                Routledge.
                </li>

                <li>Magis, D., & Facon, B. (2012).
                Angoffs delta method revisited: Improving DIF detection under
                small samples.
                <i>British Journal of Mathematical and Statistical Psychology, 65</i>(2), 302-321.
                <a href = "https://www.ncbi.nlm.nih.gov/pubmed/22500570",
                target = "_blank">See online.</a>
                </li>

                <li>Magis, D., & Facon, B. (2013).
                Item purification does not always improve DIF detection: A counter-example with Angoffs Delta plot.
                <i>Educational and Psychological Measurement, 73</i>(2), 293-311.
                <a href = "https://doi.org/10.1177%2F0013164412451903",
                target = "_blank">See online.</a>
                </li>

                <li>Mantel, N., & Haenszel, W. (1959).
                Statistical aspects of the analysis of data from retrospective studies.
                <i>Journal of the National Cancer Institute, 22</i>(4), 719-748.
                <a href = "http://www.medicine.mcgill.ca/epidemiology/hanley/c634/stratified/Mantel_Haenszel_1.pdf",
                target = "_blank">See online.</a>

                </li>

                <li>Martinkova, P., Drabinova, A., & Houdek, J. (2017).
                ShinyItemAnalysis: Analyza prijimacich a jinych znalostnich ci psychologickych testu.
                [ShinyItemAnalysis: Analyzing admission and other educational and psychological tests]
                <i>TESTFORUM, 6</i>(9), 16-35.
                <a href = "https://doi.org/10.5817/TF2017-9-129",
                target = "_blank">See online.</a>
                </li>

                <li>Martinkova, P., Drabinova, A., Liaw, Y. L., Sanders, E. A., McFarland, J. L., & Price, R. M. (2017).
                Checking equity: Why differential item functioning analysis should be a routine part
                of developing conceptual Assessments.
                <i>CBE-Life Sciences Education, 16</i>(2), rm2.
                <a href = "https://doi.org/10.1187/cbe.16-10-0307",
                target = "_blank">See online</a>
                </li>

                <li>Martinkova, P., Stepanek, L., Drabinova, A., Houdek, J., Vejrazka, M., & Stuka, C. (2017).
                Semi-real-time analyses of item characteristics for medical school admission tests.
                <i>In Proceedings of the 2017 Federated Conference on Computer Science and Information Systems</i>,
                189-194.
                <a href="https://doi.org/10.15439/2017F380",
                target="_blank">See online.</a>
                </li>

                <li>Martinkova, P., Drabinova, A., & Potuznikova, E. (2020).
                Is academic tracking related to gains in learning competence?
                Using propensity score matching and differential item change functioning analysis for better understanding
                of tracking implications.
                <i>Learning and Instruction 66</i>(April).
                <a href = "https://doi.org/10.1016/j.learninstruc.2019.101286",
                target = "_blank">See online</a>
                </li>

                <li>Masters, G. N. (1982).
                A Rasch model for partial credit scoring.
                <i>Psychometrika, 47</i>(2), 149-174.
                <a href = "https://link.springer.com/article/10.1007/BF02296272",
                target = "_blank">See online.</a>
                </li>

                <li>McFarland, J. L., Price, R. M., Wenderoth, M. P., Martinkova, P., Cliff, W., Michael, J., ..., & Wright, A. (2017).
                Development and validation of the homeostasis concept inventory.
                <i>CBE-Life Sciences Education, 16</i>(2), ar35.
                <a href = "http://dx.doi.org/10.1187/cbe.16-10-0305",
                target = "_blank">See online.</a>
                </li>

                <li>Muraki, E. (1992).
                A generalized partial credit model: Application of an EM algorithm.
                <i>ETS Research Report Series, 1992</i>(1)
                <a href = "https://onlinelibrary.wiley.com/doi/abs/10.1002/j.2333-8504.1992.tb01436.x",
                target = "_blank">See online.</a>
                </li>

                <li>Orlando, M., & Thissen, D. (2000).
                Likelihood-based item-fit indices for dichotomous item response theory models.
                <i>Applied Psychological Measurement, 24</i>(1), 50-64.
                <a href = "https://doi.org/10.1177%2F01466216000241003",
                target = "_blank">See online.</a>
                </li>

                <li>Swaminathan, H., & Rogers, H. J. (1990).
                Detecting differential item functioning using logistic regression procedures.
                <i>Journal of Educational Measurement, 27</i>(4), 361-370.
                <a href = "https://www.jstor.org/stable/1434855?seq=1#page_scan_tab_contents",
                target = "_blank">See online.</a>
                </li>

                <li>Raju, N. S. (1988).
                The area between two item characteristic curves.
                <i>Psychometrika, 53</i>(4), 495-502.
                <a href = "https://link.springer.com/article/10.1007/BF02294403",
                target = "_blank">See online.</a>
                </li>

                <li>Raju, N. S. (1990).
                Determining the significance of estimated signed and unsigned areas
                between two item response functions.
                <i>Applied Psychological Measurement, 14</i>(2), 197-207.
                <a href = "http://journals.sagepub.com/doi/abs/10.1177/014662169001400208",
                target = "_blank">See online.</a>
                </li>

                <li>Rasch, G. (1960)
                Probabilistic models for some intelligence and attainment tests.
                Copenhagen: Paedagogiske Institute.
                </li>

                <li>Revelle, W. (1979).
                Hierarchical cluster analysis and the internal structure of tests.
                <i>Multivariate Behavioral Research, 14</i>(1), 57-74.
                <a href = "https://doi.org/10.1207/s15327906mbr1401_4",
                target = "_blank">See online.</a>
                </li>

                <li>Samejima, F. (1969).
                Estimation of latent ability using a response pattern of graded scores.
                <i>Psychometrika, 34</i>(1), 1-97
                <a href = "https://link.springer.com/article/10.1007%2FBF03372160",
                target = "_blank">See online.</a>
                </li>

                <li>Schwarz, G. (1978).
                Estimating the dimension of a model.
                <i>The Annals of Statistics, 6</i>(2), 461-464.
                <a href = "https://projecteuclid.org/euclid.aos/1176344136",
                target = "_blank">See online.</a>
                </li>

                <li>Shealy, R., & Stout, W. (1993).
                A model-based standardization approach that separates true bias/DIF from group ability
                differences and detect test bias/DTF
                as well as Item Bias/DIF.
                <i>Psychometrika, 58</i>(2), 159-194.
                <a href = "https://link.springer.com/article/10.1007/BF02294572",
                target = "_blank">See online.</a>
                </li>

                <li>Spearman, C. (1910).
                Correlation calculated from faulty data.
                <i>British Journal of Psychology, 1904-1920, 3</i>(3), 271-295.
                <a href = "https://onlinelibrary.wiley.com/doi/abs/10.1111/j.2044-8295.1910.tb00206.x",
                target = "_blank">See online.</a>
                </li>

                <li>Wilson, M. (2005).
                Constructing measures: An item response modeling approach.
                </li>

                <li>Wright, B. D., & Stone, M. H. (1979).
                Best test design.
                Chicago: Mesa Press.
                </li>
                </ul>')
  )
