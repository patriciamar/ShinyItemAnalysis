uiReferences <-
  tabPanel("",
           icon = icon("fas fa-book"),
           #------------------------------------------------------------------------------------#
           # Packages ####
           #------------------------------------------------------------------------------------#
           h3("R packages"),
           HTML('<ul class = "biblio">
                <li><code>corrplot</code>
                Wei, T. & Simko, V. (2017).
                R package `corrplot`: Visualization of a Correlation Matrix.
                R package version 0.84.
                <a href = "https://github.com/taiyun/corrplot", target = "_blank">See online.</a>
                </li>

                <li><code>cowplot</code>
                Claus O. Wilke (2018).
                cowplot: Streamlined Plot Theme and Plot Annotations for "ggplot2".
                R package version 0.9.3.
                <a href = " https://CRAN.R-project.org/package=cowplot", target = "_blank">See online.</a>
                </li>

                <li><code>CTT</code>
                Willse, J. & Willse, T. (2018).
                CTT: Classical Test Theory Functions.
                R package version 2.3.2.
                <a href = "https://CRAN.R-project.org/package=CTT", target = "_blank">See online.</a>
                </li>

                <li><code>data.table</code>
                Dowle, M. & Srinivasan, A. (2018).
                data.table: Extension of `data.frame`.
                R package version 1.11.4.
                <a href = "https://CRAN.R-project.org/package=data.table", target = "_blank">See online.</a>
                </li>


                <li><code>deltaPlotR</code>
                Magis, D. & Facon, B. (2014).
                deltaPlotR: An R Package for Differential Item Functioning Analysis with Angoff`s Delta Plot.
                <i>Journal of Statistical Software, Code Snippets, 59</i>(1), 1--19.
                <a href = "http://www.jstatsoft.org/v59/c01/", target = "_blank">See online.</a>
                </li>

                <li><code>difNLR</code>
                Drabinova, A., Martinkova, P. & Zvara, K. (2018).
                difNLR: DIF and DDF Detection by Non-Linear Regression Models.
                R package version 1.2.2.
                <a href = "https://CRAN.R-project.org/package=difNLR", target = "_blank">See online.</a>
                </li>


                <li><code>difR</code>
                Magis, D., Beland, S., Tuerlinckx, F. & De Boeck, P. (2010).
                A general framework and an R package for the detection of dichotomous differential item functioning.
                <i>Behavior Research Methods, 42</i>847--862.
                </li>

                <li><code>DT</code>
                Xie, Y. (2018).
                DT: A Wrapper of the JavaScript Library `DataTables`.
                R package version 0.4.
                <a href = "https://CRAN.R-project.org/package=DT", target = "_blank">See online.</a>
                </li>

                <li><code>ggdendro</code>
                Andrie de Vries & Brian D. Ripley (2018).
                ggdendro: Create Dendrograms and Tree Diagrams Using "ggplot2".
                R package version 0.1-20.
                <a href = "https://CRAN.R-project.org/package=ggdendro", target = "_blank">See online.</a>
                </li>

                <li><code>ggplot2</code>
                Wickham, H. (2016).
                ggplot2: Elegant Graphics for Data Analysis.
                <a href = "http://ggplot2.org", target = "_blank">See online.</a>
                </li>

                <li><code>gridExtra</code>
                Auguie, B. (2017).
                gridExtra: Miscellaneous Functions for `Grid` Graphics.
                R package version 2.3.
                <a href = "https://CRAN.R-project.org/package=gridExtra", target = "_blank">See online.</a>
                </li>

                <li><code>knitr</code>
                Xie, Y. (2018).
                knitr: A General-Purpose Package for Dynamic Report Generation in R.
                R package version 1.20.
                <a href = "https://yihui.name/knitr/", target = "_blank">See online.</a>
                </li>

                <li><code>lattice</code>
                Sarkar, D. (2008).
                Lattice: Multivariate Data Visualization with R.
                <a href = "http://lmdvr.r-forge.r-project.org", target = "_blank">See online.</a>
                </li>

                <li><code>latticeExtra</code>
                Sarkar, D. & Andrews, F. (2016).
                latticeExtra: Extra Graphical Utilities Based on Lattice.
                R package version 0.6-28.
                <a href = "https://CRAN.R-project.org/package=latticeExtra", target = "_blank">See online.</a>
                </li>

                <li><code>ltm</code>
                Rizopoulos, D. (2006).
                ltm: An R package for Latent Variable Modelling and Item Response Theory Analyses.
                <i>Journal of Statistical Software, 17</i>(5), 1--25.
                <a href = "http://www.jstatsoft.org/v17/i05/", target = "_blank">See online.</a>
                </li>

                <li><code>MASS</code>
                Venables, C. & Ripley, C. (2002).
                Modern Applied Statistics with S.
                <a href = "http://www.stats.ox.ac.uk/pub/MASS4", target = "_blank">See online.</a>
                </li>

                <li><code>mirt</code>
                Chalmers, R. & Chalmers, P. (2012).
                mirt: A Multidimensional Item Response Theory Package for the R Environment.
                <i>Journal of Statistical Software, 48</i>(6), 1--29.
                </li>

                <li><code>moments</code>
                Komsta, L. & Novomestky, F. (2015).
                moments: Moments, cumulants, skewness, kurtosis and related tests.
                R package version 0.14.
                <a href = "https://CRAN.R-project.org/package=moments", target = "_blank">See online.</a>
                </li>

                <li><code>msm</code>
                Jackson, C. & Jackson, H. (2011).
                Multi-State Models for Panel Data: The msm Package for R.
                <i>Journal of Statistical Software, 38</i>(8), 1--29.
                <a href = "http://www.jstatsoft.org/v38/i08/", target = "_blank">See online.</a>
                </li>

                <li><code>multilevel</code>
                Bliese, P. (2016).
                multilevel: Multilevel Functions.
                R package version 2.6.
                <a href = "https://CRAN.R-project.org/package=multilevel", target = "_blank">See online.</a>
                </li>

                <li><code>nlme</code>
                Pinheiro, J., Bates, D., DebRoy, S., Sarkar, D. & NULL, R. (2018).
                nlme: Linear and Nonlinear Mixed Effects Models.
                R package version 3.1-137.
                <a href = "https://CRAN.R-project.org/package=nlme", target = "_blank">See online.</a>
                </li>

                <li><code>nnet</code>
                Venables, C. & Ripley, C. (2002).
                Modern Applied Statistics with S.
                <a href = "http://www.stats.ox.ac.uk/pub/MASS4", target = "_blank">See online.</a>
                </li>

                <li><code>plotly</code>
                Sievert, C., Parmer, C., Hocking, T., Chamberlain, S., Ram, K., Corvellec, M. & Despouy, P. (2017).
                plotly: Create Interactive Web Graphics via `plotly.js`.
                R package version 4.7.1.
                <a href = "https://CRAN.R-project.org/package=plotly", target = "_blank">See online.</a>
                </li>

                <li><code>polycor</code>
                Fox, J. (2016).
                polycor: Polychoric and Polyserial Correlations.
                R package version 0.7-9.
                <a href = "https://CRAN.R-project.org/package=polycor", target = "_blank">See online.</a>
                </li>

                <li><code>psych</code>
                Revelle, W. (2018).
                psych: Procedures for Psychological, Psychometric, and Personality Research.
                R package version 1.8.4.
                <a href = "https://CRAN.R-project.org/package=psych", target = "_blank">See online.</a>
                </li>

                <li><code>psychometric</code>
                Fletcher, T. & Fletcher, D. (2010).
                psychometric: Applied Psychometric Theory.
                R package version 2.2.
                <a href = "https://CRAN.R-project.org/package=psychometric", target = "_blank">See online.</a>
                </li>

                <li><code>RColorBrewer</code>
                Neuwirth, E. (2014).
                RColorBrewer: ColorBrewer Palettes.
                R package version 1.1-2.
                <a href = "https://CRAN.R-project.org/package=RColorBrewer", target = "_blank">See online.</a>
                </li>

                <li><code>reshape2</code>
                Wickham, H. (2007).
                Reshaping Data with the reshape Package.
                <i>Journal of Statistical Software, 21</i>(12), 1--20.
                <a href = "http://www.jstatsoft.org/v21/i12/", target = "_blank">See online.</a>
                </li>

                <li><code>rmarkdown</code>
                Allaire, J., Xie, Y., McPherson, J., Luraschi, J., Ushey, K., Atkins, A., Wickham, H., Cheng, J. & Chang, W. (2018).
                rmarkdown: Dynamic Documents for R.
                R package version 1.10.
                <a href = "https://CRAN.R-project.org/package=rmarkdown", target = "_blank">See online.</a>
                </li>

                <li><code>shiny</code>
                Chang, W., Cheng, J., Allaire, J., Xie, Y. & McPherson, J. (2018).
                shiny: Web Application Framework for R.
                R package version 1.1.0.
                <a href = "https://CRAN.R-project.org/package=shiny", target = "_blank">See online.</a>
                </li>

                <li><code>shinyBS</code>
                Bailey, E. (2015).
                shinyBS: Twitter Bootstrap Components for Shiny.
                R package version 0.61.
                <a href = "https://CRAN.R-project.org/package=shinyBS", target = "_blank">See online.</a>
                </li>

                <li><code>ShinyItemAnalysis</code>
                Martinkova, P., & Drabinova, A. (2018).
                ShinyItemAnalysis for teaching psychometrics and to enforce routine analysis of educational tests.
                The R Journal, 10(2), 503-515.
                <a href = "https://journal.r-project.org/archive/2018/RJ-2018-074/", target = "_blank">See online.</a>
                </li>

                <li><code>shinyjs</code>
                Attali, D. (2018).
                shinyjs: Easily Improve the User Experience of Your Shiny Apps in Seconds.
                R package version 1.0.
                <a href = "https://CRAN.R-project.org/package=shinyjs", target = "_blank">See online.</a>
                </li>

                <li><code>stringr</code>
                Wickham, H. (2018).
                stringr: Simple, Consistent Wrappers for Common String Operations.
                R package version 1.3.1.
                <a href = "https://CRAN.R-project.org/package=stringr", target = "_blank">See online.</a>
                </li>

                <li><code>xtable</code>
                Dahl, D. & Dahl, B. (2016).
                xtable: Export Tables to LaTeX or HTML.
                R package version 1.8-2.
                <a href = "https://CRAN.R-project.org/package=xtable", target = "_blank">See online.</a>
                </li>
                </ul>'),
           #------------------------------------------------------------------------------------#
           # References ####
           #------------------------------------------------------------------------------------#
           h3('References'),
           HTML('<ul class = "biblio">
                <li>Akaike, H. (1974). A New Look at the Statistical Model Identification.
                <i>IEEE Transactions on Automatic Control, 19</i>(6), 716-723.
                <a href = "http://ieeexplore.ieee.org/abstract/document/1100705/",
                target = "_blank">See online.</a>
                </li>

                <li>Ames, A. J., & Penfield, R. D. (2015). An NCME Instructional Module on Item-Fit
                Statistics for Item Response Theory Models.
                <i>Educational Measurement: Issues and Practice, 34</i>(3), 39-48.
                <a href = "http://onlinelibrary.wiley.com/doi/10.1111/emip.12067/full",
                target = "_blank">See online.</a>
                </li>

                <li>Andrich, D. (1978). A Rating Formulation for Ordered Response Categories.
                <i>Psychometrika, 43</i>(4), 561-573.
                <a href = "https://link.springer.com/article/10.1007/BF02293814",
                target = "_blank">See online.</a>
                </li>

                <li>Angoff, W. H., & Ford, S. F. (1973). Item-Race Interaction on a Test of
                Scholastic Aptitude.
                <i>Journal of Educational Measurement, 10</i>(2), 95-105.
                <a href = "https://www.jstor.org/stable/1433905?seq=1#page_scan_tab_contents",
                target = "_blank">See online.</a>
                </li>

				<li>Bartholomew, D., Steel, F., Moustaki, I. and Galbraith, J. (2002). The Analysis and Interpretation of Multivariate
				Data for Social Scientists. London: Chapman and Hall.
                </li>

                <li>Bock, R. D. (1972). Estimating Item Parameters and Latent Ability when
                Responses Are Scored in Two or More Nominal Categories.
                <i>Psychometrika, 37</i>(1), 29-51.
                <a href = "http://link.springer.com/article/10.1007/BF02291411",
                target = "_blank">See online.</a>
                </li>

                <li>Brown, W. (1910).
                Some experimental results in the correlation of mental abilities.
                <i>British Journal of Psychology, 1904‐1920, 3</i>(3), 296-322.
                <a href = "https://onlinelibrary.wiley.com/doi/abs/10.1111/j.2044-8295.1910.tb00207.x",
                target = "_blank">See online.</a>
                </li>

                <li>Chalmers, R. P. (2018). Improving the Crossing-SIBTEST Statistic for Detecting Non-uniform DIF.
                <i>Psychometrika, 83</i>(2), 376–386.
                <a href = "https://link.springer.com/article/10.1007/s11336-017-9583-8",
                target = "_blank">See online.</a>
                </li>

                <li>Cronbach, L. J. (1951). Coefficient Alpha and the Internal Structure of Tests.
                <i>Psychometrika, 16</i>(3), 297-334.
                <a href = "https://link.springer.com/article/10.1007/BF02310555",
                target = "_blank">See online.</a>
                </li>

                <li>Drabinova, A., & Martinkova, P. (2017). Detection of Differential Item Functioning
                with Non-Linear Regression: Non-IRT Approach Accounting for Guessing.
                <i>Journal of Educational Measurement, 54</i>(4), 498-517
                <a href = "http://onlinelibrary.wiley.com/doi/10.1111/jedm.12158/full",
                target = "_blank">See online.</a>
                </li>

                <li> Feldt, L. S., Woodruff, D. J., & Salih, F. A. (1987).
                Statistical inference for coefficient alpha.
                <i>Applied Psychological Measurement 11</i>(1), 93-103.
                <a href = "http://journals.sagepub.com/doi/abs/10.1177/014662168701100107",
                target = "_blank">See online.</a>
                </li>

                <li>Li, H.-H., and Stout, W. (1996). A New Procedure for Detection of Crossing DIF.
                <i>Psychometrika, 61</i>(4), 647–677.
                <a href = "https://link.springer.com/article/10.1007/BF02294041",
                target = "_blank">See online.</a>
                </li>

                <li>Lord, F. M. (1980). Applications of Item Response Theory to Practical Testing Problems.
                Routledge.
                </li>

                <li>Magis, D., & Facon, B. (2012). Angoffs Delta Method Revisited: Improving DIF Detection under
                Small Samples.
                <i>British Journal of Mathematical and Statistical Psychology, 65</i>(2), 302-321.
                <a href = "https://www.ncbi.nlm.nih.gov/pubmed/22500570",
                target = "_blank">See online.</a>
                </li>

                <li>Mantel, N., & Haenszel, W. (1959). Statistical Aspects of the Analysis of Data from
                Retrospective Studies.
                <i>Journal of the National Cancer Institute, 22</i>(4), 719-748.
                <a href = "http://www.medicine.mcgill.ca/epidemiology/hanley/c634/stratified/Mantel_Haenszel_1.pdf",
                target = "_blank">See online.</a>                                                                                    )),
                </li>

                <li>Martinkova, P., Drabinova, A., & Houdek, J. (2017). ShinyItemAnalysis: Analyza Prijimacich a
                Jinych Znalostnich ci Psychologických Testu. [ShinyItemAnalysis: Analyzing Admission and Other
                Educational and Psychological Tests]
                <i>TESTFORUM, 6</i>(9), 16–35.
                <a href = "http://testforum.cz/domains/testforum.cz/index.php/testforum/article/view/TF2017-9-129",
                target = "_blank">See online.</a>
                </li>

                <li>Martinkova, P., Drabinova, A., Liaw, Y. L., Sanders, E. A., McFarland, J. L., & Price, R. M.
                (2017). Checking Equity: Why Differential Item Functioning Analysis Should Be a Routine Part
                of Developing Conceptual Assessments.
                <i>CBE-Life Sciences Education, 16</i>(2), rm2.
                <a href = "https://doi.org/10.1187/cbe.16-10-0307",
                target = "_blank">See online</a>
                </li>

                <li>Martinkova, P., Stepanek, L., Drabinova, A., Houdek, J., Vejrazka, M., & Stuka, C. (2017).
                Semi-real-time Analyses of Item Characteristics for Medical School Admission Tests.
                In
                <i>Proceedings of the 2017 Federated Conference on Computer Science and Information Systems</i>,
                189-194.
                <a href="https://doi.org/10.15439/2017F380",
                target="_blank">See online.</a>
                </li>

                <li>Masters, G. N. (1982). A Rasch model for partial credit scoring.
                <i>Psychometrika, 47</i>(2), 149-174.
                <a href = "https://link.springer.com/article/10.1007/BF02296272",
                target = "_blank">See online.</a>
                </li>

                <li>McFarland, J. L., Price, R. M., Wenderoth, M. P., Martinkova, P., Cliff, W., Michael, J., ... & Wright, A. (2017).
                Development and Validation of the Homeostasis Concept Inventory.
                <i>CBE-Life Sciences Education, 16</i>(2), ar35.
                <a href = "https://www.lifescied.org/doi/abs/10.1187/cbe.16-10-0305",
                target = "_blank">See online.</a>
                </li>

                <li>Muraki, E. (1992). A Generalized Partial Credit Model: Application of an EM Algorithm.
                <i>ETS Research Report Series, 1992</i>(1)
                <a href = "https://onlinelibrary.wiley.com/doi/abs/10.1002/j.2333-8504.1992.tb01436.x",
                target = "_blank">See online.</a>
                </li>

                <li>Orlando, M., & Thissen, D. (2000). Likelihood-based item-fit indices for dichotomous item response theory models.
                <i>Applied Psychological Measurement, 24</i>(1), 50-64.
                <a href = "https://doi.org/10.1177%2F01466216000241003",
                target = "_blank">See online.</a>
                </li>

                <li>Swaminathan, H., & Rogers, H. J. (1990). Detecting Differential Item
                Functioning Using Logistic Regression Procedures.
                <i>Journal of Educational Measurement, 27</i>(4), 361-370.
                <a href = "https://www.jstor.org/stable/1434855?seq=1#page_scan_tab_contents",
                target = "_blank">See online.</a>
                </li>

                <li>Raju, N. S. (1988). The Area between Two Item Characteristic Curves.
                <i>Psychometrika, 53</i>(4), 495-502.
                <a href = "https://link.springer.com/article/10.1007/BF02294403",
                target = "_blank">See online.</a>
                </li>

                <li>Raju, N. S. (1990). Determining the Significance of Estimated Signed and Unsigned Areas
                between Two Item Response Functions.
                <i>Applied Psychological Measurement, 14</i>(2), 197-207.
                <a href = "http://journals.sagepub.com/doi/abs/10.1177/014662169001400208",
                target = "_blank">See online.</a>
                </li>

                <li>Rasch, G. (1960) Probabilistic Models for Some Intelligence and Attainment Tests.
                Copenhagen: Paedagogiske Institute.
                </li>

                <li>Revelle, W. (1979).
                Hierarchical cluster analysis and the internal structure of tests.
                <i>Multivariate Behavioral Research, 14</i>(1), 57-74.
                <a href = "https://doi.org/10.1207/s15327906mbr1401_4",
                target = "_blank">See online.</a>
                </li>

                <li>Samejima, F. (1969). Estimation of Latent Ability Using a Response Pattern of Graded Scores.
                <i>Psychometrika, 34</i>(1), 1-97
                <a href = "https://link.springer.com/article/10.1007%2FBF03372160",
                target = "_blank">See online.</a>
                </li>

                <li>Schwarz, G. (1978). Estimating the Dimension of a Model.
                <i>The Annals of Statistics, 6</i>(2), 461-464.
                <a href = "https://projecteuclid.org/euclid.aos/1176344136",
                target = "_blank">See online.</a>
                </li>

                <li>Shealy, R. and Stout, W. (1993). A Model-Based Standardization Approach that
                Separates True Bias/DIF from Group Ability Differences and Detect Test Bias/DTF
                as well as Item Bias/DIF.
                <i>Psychometrika, 58</i>(2), 159-194.
                <a href = "https://link.springer.com/article/10.1007/BF02294572",
                target = "_blank">See online.</a>
                </li>

                <li>Spearman, C. (1910).
                Correlation calculated from faulty data.
                <i>British Journal of Psychology, 1904‐1920, 3</i>(3), 271-295.
                <a href = "https://onlinelibrary.wiley.com/doi/abs/10.1111/j.2044-8295.1910.tb00206.x",
                target = "_blank">See online.</a>
                </li>

                <li>Wilson, M. (2005). Constructing Measures: An Item Response Modeling Approach.
                </li>

                <li>Wright, B. D., & Stone, M. H. (1979). Best Test Design. Chicago: Mesa Press.
                </li>
                </ul>'),
           br()
  )
