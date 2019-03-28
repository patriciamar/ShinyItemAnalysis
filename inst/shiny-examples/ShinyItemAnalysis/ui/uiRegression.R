uiRegression <- 
  navbarMenu("Regression",
             "Dichotomous models",
             # * LOGISTIC ####
             tabPanel("Logistic",
                      h3("Logistic regression on total scores"),
                      withMathJax(),
                      p('Various regression models may be fitted to describe
                        item properties in more detail.',
                        strong('Logistic regression'),'can model dependency of probability of correct answer on total score by
                        S-shaped logistic curve. Parameter', strong( "\\(b_{0}\\)"),' describes horizontal position of the fitted curve,
                        parameter ', strong( '\\(b_{1}\\)'),' describes its slope.'),
                      br(),
                      h4("Plot with estimated logistic curve"),
                      p('Points represent proportion of correct answer with respect to total score.
                        Their size is determined by count of respondents who achieved given level of
                        total score.'),
                      sliderInput("logregSlider", "Item",
                                  min = 1, value = 1, max = 10,
                                  step = 1, animate = animationOptions(interval = 1200)),
                      plotOutput('logreg_plot'),
                      downloadButton("DB_logreg_plot", label = "Download figure"),
                      h4("Equation"),
                      withMathJax(),
                      ('$$\\mathrm{P}(Y = 1|X, b_0, b_1) = \\mathrm{E}(Y|X, b_0, b_1) = \\frac{e^{\\left( b_{0} + b_1 X\\right)}}{1+e^{\\left( b_{0} + b_1 X\\right) }} $$'),
                      h4("Table of parameters"),
                      fluidRow(column(12, align = "center",tableOutput('coef_logreg_table'))),
                      htmlOutput("logreg_interpretation"),
                      br(),
                      h4("Selected R code"),
                      div(code(HTML("library(difNLR)&nbsp;<br>library(ggplot2)<br><br>#&nbsp;loading&nbsp;data<br>data(GMAT)&nbsp;<br>data&nbsp;<-&nbsp;GMAT[,&nbsp;1:20]&nbsp;<br>score&nbsp;<-&nbsp;apply(data,&nbsp;1,&nbsp;sum)&nbsp;#&nbsp;total&nbsp;score<br><br>#&nbsp;logistic&nbsp;model&nbsp;for&nbsp;item&nbsp;1&nbsp;<br>fit&nbsp;<-&nbsp;glm(data[,&nbsp;1]&nbsp;~&nbsp;score,&nbsp;family&nbsp;=&nbsp;binomial)&nbsp;<br><br>#&nbsp;coefficients&nbsp;<br>coef(fit)&nbsp;<br><br>#&nbsp;function&nbsp;for&nbsp;plot&nbsp;<br>fun&nbsp;<-&nbsp;function(x,&nbsp;b0,&nbsp;b1){exp(b0&nbsp;+&nbsp;b1&nbsp;*&nbsp;x)&nbsp;/&nbsp;(1&nbsp;+&nbsp;exp(b0&nbsp;+&nbsp;b1&nbsp;*&nbsp;x))}&nbsp;<br><br>#&nbsp;empirical&nbsp;probabilities&nbsp;calculation<br>df&nbsp;<-&nbsp;data.frame(x&nbsp;=&nbsp;sort(unique(score)),<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;y&nbsp;=&nbsp;tapply(data[,&nbsp;1],&nbsp;score,&nbsp;mean),<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;size&nbsp;=&nbsp;as.numeric(table(score)))<br><br>#&nbsp;plot&nbsp;of&nbsp;estimated&nbsp;curve<br>ggplot(df,&nbsp;aes(x&nbsp;=&nbsp;x,&nbsp;y&nbsp;=&nbsp;y))&nbsp;+<br>&nbsp;&nbsp;geom_point(aes(size&nbsp;=&nbsp;size),<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;color&nbsp;=&nbsp;\"darkblue\",<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;fill&nbsp;=&nbsp;\"darkblue\",<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;shape&nbsp;=&nbsp;21,&nbsp;alpha&nbsp;=&nbsp;0.5)&nbsp;+<br>&nbsp;&nbsp;stat_function(fun&nbsp;=&nbsp;fun,&nbsp;geom&nbsp;=&nbsp;\"line\",<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;args&nbsp;=&nbsp;list(b0&nbsp;=&nbsp;coef(fit)[1],<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;b1&nbsp;=&nbsp;coef(fit)[2]),<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;size&nbsp;=&nbsp;1,<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;color&nbsp;=&nbsp;\"darkblue\")&nbsp;+<br>&nbsp;&nbsp;xlab(\"Total&nbsp;score\")&nbsp;+<br>&nbsp;&nbsp;ylab(\"Probability&nbsp;of&nbsp;correct&nbsp;answer\")&nbsp;+<br>&nbsp;&nbsp;ylim(0,&nbsp;1)&nbsp;+<br>&nbsp;&nbsp;ggtitle(\"Item&nbsp;1\")&nbsp;+&nbsp;<br>&nbsp;&nbsp;theme_app()"))),
                      br()
                      ),
             # * LOGISTIC Z ####
             tabPanel("Logistic Z",
                      h3("Logistic regression on standardized total scores"),
                      p('Various regression models may be fitted to describe
                        item properties in more detail.',
                        strong('Logistic regression'), 'can model dependency of probability of correct answer on
                        standardized total score (Z-score) by S-shaped logistic curve. Parameter ', strong( '\\(b_{0}\\)'), ' describes
                        horizontal position of the fitted curve (difficulty), parameter ', strong('\\(b_{1}\\)'),' describes its slope at
                        inflection point (discrimination). '),
                      br(),
                      h4("Plot with estimated logistic curve"),
                      p('Points represent proportion of correct answer with respect to standardized
                        total score. Their size is determined by count of respondents who achieved given
                        level of standardized total score.'),
                      sliderInput("zlogregSlider", "Item",
                                  min = 1, value = 1, max = 10,
                                  step = 1, animate = animationOptions(interval = 1200)),
                      plotOutput('z_logreg_plot'),
                      downloadButton("DB_z_logreg_plot", label = "Download figure"),
                      h4("Equation"),
                      ('$$\\mathrm{P}(Y = 1|Z, b_0, b_1) = \\mathrm{E}(Y|Z, b_0, b_1) = \\frac{e^{\\left( b_{0} + b_1 Z\\right) }}{1+e^{\\left( b_{0} + b_1 Z\\right) }} $$'),
                      h4("Table of parameters"),
                      fluidRow(column(12, align = "center",tableOutput('coef_z_logreg'))),
                      htmlOutput("z_logreg_interpretation"),
                      br(),
                      h4("Selected R code"),
                      div(code(HTML("library(difNLR)&nbsp;<br>library(ggplot2)<br><br>#&nbsp;loading&nbsp;data<br>data(GMAT)&nbsp;<br>data&nbsp;<-&nbsp;GMAT[,&nbsp;1:20]&nbsp;<br>zscore&nbsp;<-&nbsp;scale(apply(data,&nbsp;1,&nbsp;sum))&nbsp;#&nbsp;standardized&nbsp;total&nbsp;score<br><br>#&nbsp;logistic&nbsp;model&nbsp;for&nbsp;item&nbsp;1&nbsp;<br>fit&nbsp;<-&nbsp;glm(data[,&nbsp;1]&nbsp;~&nbsp;zscore,&nbsp;family&nbsp;=&nbsp;binomial)&nbsp;<br><br>#&nbsp;coefficients&nbsp;<br>coef(fit)&nbsp;<br><br>#&nbsp;function&nbsp;for&nbsp;plot&nbsp;<br>fun&nbsp;<-&nbsp;function(x,&nbsp;b0,&nbsp;b1){exp(b0&nbsp;+&nbsp;b1&nbsp;*&nbsp;x)&nbsp;/&nbsp;(1&nbsp;+&nbsp;exp(b0&nbsp;+&nbsp;b1&nbsp;*&nbsp;x))}&nbsp;<br><br>#&nbsp;empirical&nbsp;probabilities&nbsp;calculation<br>df&nbsp;<-&nbsp;data.frame(x&nbsp;=&nbsp;sort(unique(zscore)),<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;y&nbsp;=&nbsp;tapply(data[,&nbsp;1],&nbsp;zscore,&nbsp;mean),<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;size&nbsp;=&nbsp;as.numeric(table(zscore)))<br><br>#&nbsp;plot&nbsp;of&nbsp;estimated&nbsp;curve<br>ggplot(df,&nbsp;aes(x&nbsp;=&nbsp;x,&nbsp;y&nbsp;=&nbsp;y))&nbsp;+<br>&nbsp;&nbsp;geom_point(aes(size&nbsp;=&nbsp;size),<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;color&nbsp;=&nbsp;\"darkblue\",<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;fill&nbsp;=&nbsp;\"darkblue\",<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;shape&nbsp;=&nbsp;21,&nbsp;alpha&nbsp;=&nbsp;0.5)&nbsp;+<br>&nbsp;&nbsp;stat_function(fun&nbsp;=&nbsp;fun,&nbsp;geom&nbsp;=&nbsp;\"line\",<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;args&nbsp;=&nbsp;list(b0&nbsp;=&nbsp;coef(fit)[1],<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;b1&nbsp;=&nbsp;coef(fit)[2]),<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;size&nbsp;=&nbsp;1,<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;color&nbsp;=&nbsp;\"darkblue\")&nbsp;+<br>&nbsp;&nbsp;xlab(\"Standardized&nbsp;total&nbsp;score\")&nbsp;+<br>&nbsp;&nbsp;ylab(\"Probability&nbsp;of&nbsp;correct&nbsp;answer\")&nbsp;+<br>&nbsp;&nbsp;ylim(0,&nbsp;1)&nbsp;+<br>&nbsp;&nbsp;ggtitle(\"Item&nbsp;1\")&nbsp;+&nbsp;<br>&nbsp;&nbsp;theme_app()"))),
                      br()
                      ),
             # * LOGISTIC IRT Z ####
             tabPanel("Logistic IRT Z",
                      h3("Logistic regression on standardized total scores with IRT parameterization"),
                      p('Various regression models may be fitted to describe
                        item properties in more detail.',
                        strong('Logistic regression'), 'can model dependency of probability of correct answer on
                        standardized total score (Z-score) by s-shaped logistic curve. Note change in parametrization - the IRT parametrization
                        used here corresponds to the parametrization used in IRT models.
                        Parameter', strong('\\(b\\)') , 'describes horizontal position of the fitted curve (difficulty),
                        parameter' , strong('\\(a\\)') , ' describes its slope at inflection point (discrimination). '),
                      br(),
                      h4("Plot with estimated logistic curve"),
                      p('Points represent proportion of correct answer with respect to standardized
                        total score. Their size is determined by count of respondents who achieved given
                        level of standardized total score.'),
                      sliderInput("zlogreg_irtSlider", "Item",
                                  min = 1, value = 1, max = 10,
                                  step = 1, animate = animationOptions(interval = 1200)),
                      plotOutput('z_logreg_irt_plot'),
                      downloadButton("DB_z_logreg_irt_plot", label = "Download figure"),
                      h4("Equation"),
                      ('$$\\mathrm{P}(Y = 1|Z, a, b) = \\mathrm{E}(Y|Z, a, b) = \\frac{e^{ a\\left(Z - b\\right) }}{1+e^{a\\left(Z - b\\right)}} $$'),
                      h4("Table of parameters"),
                      fluidRow(column(12, align = "center",tableOutput('coef_z_logreg_irt'))),
                      htmlOutput("z_logreg_irt_interpretation"),
                      br(),
                      h4("Selected R code"),
                      div(code(HTML("library(difNLR)&nbsp;<br>library(ggplot2)<br><br>#&nbsp;loading&nbsp;data<br>data(GMAT)&nbsp;<br>data&nbsp;<-&nbsp;GMAT[,&nbsp;1:20]&nbsp;<br>zscore&nbsp;<-&nbsp;scale(apply(data,&nbsp;1,&nbsp;sum))&nbsp;#&nbsp;standardized&nbsp;total&nbsp;score<br><br>#&nbsp;logistic&nbsp;model&nbsp;for&nbsp;item&nbsp;1&nbsp;<br>fit&nbsp;<-&nbsp;glm(data[,&nbsp;1]&nbsp;~&nbsp;zscore,&nbsp;family&nbsp;=&nbsp;binomial)&nbsp;<br><br>#&nbsp;coefficients<br>coef&nbsp;<-&nbsp;c(a&nbsp;=&nbsp;coef(fit)[2],&nbsp;b&nbsp;=&nbsp;-&nbsp;coef(fit)[1]&nbsp;/&nbsp;coef(fit)[2])&nbsp;<br>coef&nbsp;&nbsp;<br><br>#&nbsp;function&nbsp;for&nbsp;plot&nbsp;<br>fun&nbsp;<-&nbsp;function(x,&nbsp;a,&nbsp;b){exp(a&nbsp;*&nbsp;(x&nbsp;-&nbsp;b))&nbsp;/&nbsp;(1&nbsp;+&nbsp;exp(a&nbsp;*&nbsp;(x&nbsp;-&nbsp;b)))}&nbsp;<br><br>#&nbsp;empirical&nbsp;probabilities&nbsp;calculation<br>df&nbsp;<-&nbsp;data.frame(x&nbsp;=&nbsp;sort(unique(zscore)),<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;y&nbsp;=&nbsp;tapply(data[,&nbsp;1],&nbsp;zscore,&nbsp;mean),<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;size&nbsp;=&nbsp;as.numeric(table(zscore)))<br><br>#&nbsp;plot&nbsp;of&nbsp;estimated&nbsp;curve<br>ggplot(df,&nbsp;aes(x&nbsp;=&nbsp;x,&nbsp;y&nbsp;=&nbsp;y))&nbsp;+<br>&nbsp;&nbsp;geom_point(aes(size&nbsp;=&nbsp;size),<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;color&nbsp;=&nbsp;\"darkblue\",<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;fill&nbsp;=&nbsp;\"darkblue\",<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;shape&nbsp;=&nbsp;21,&nbsp;alpha&nbsp;=&nbsp;0.5)&nbsp;+<br>&nbsp;&nbsp;stat_function(fun&nbsp;=&nbsp;fun,&nbsp;geom&nbsp;=&nbsp;\"line\",<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;args&nbsp;=&nbsp;list(a&nbsp;=&nbsp;coef[1],<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;b&nbsp;=&nbsp;coef[2]),<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;size&nbsp;=&nbsp;1,<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;color&nbsp;=&nbsp;\"darkblue\")&nbsp;+<br>&nbsp;&nbsp;xlab(\"Standardized&nbsp;total&nbsp;score\")&nbsp;+<br>&nbsp;&nbsp;ylab(\"Probability&nbsp;of&nbsp;correct&nbsp;answer\")&nbsp;+<br>&nbsp;&nbsp;ylim(0,&nbsp;1)&nbsp;+<br>&nbsp;&nbsp;ggtitle(\"Item&nbsp;1\")&nbsp;+&nbsp;<br>&nbsp;&nbsp;theme_app()"))),
                      br()
                      ),
             
             # * NONLINEAR 3P IRT Z ####
             tabPanel("Nonlinear 3P IRT Z",
                      h3("Nonlinear three parameter regression on standardized total scores with IRT parameterization"),
                      p('Various regression models may be fitted to describe
                        item properties in more detail.',
                        strong('Nonlinear regression'), 'can model dependency of probability of correct answer on
                        standardized total score (Z-score) by s-shaped logistic curve. The IRT parametrization used here corresponds
                        to the parametrization used in IRT models. Parameter ', strong( '\\(b\\)'),' describes horizontal position of the fitted curve (difficulty),
                        parameter ',strong( '\\(a\\)'), ' describes its slope at inflection point (discrimination). This model allows for nonzero lower left
                        asymptote ', strong( '\\(c\\)'), ' (pseudo-guessing parameter). '),
                      br(),
                      h4("Plot with estimated nonlinear curve"),
                      p('Points represent proportion of correct answer with respect to standardized
                        total score. Their size is determined by count of respondents who achieved given
                        level of standardized total score.'),
                      sliderInput(inputId = "slider_nlr_3P_item", label = "Item",
                                  min = 1, value = 1, max = 10, step = 1, animate = animationOptions(interval = 1200)),
                      plotOutput('nlr_3P_plot'),
                      downloadButton("DB_nlr_3P_plot", label = "Download figure"),
                      h4("Equation"),
                      ('$$\\mathrm{P}(Y = 1|Z, a, b, c) = \\mathrm{E}(Y|Z, a, b, c) = c + \\left( 1-c \\right) \\cdot \\frac{e^{a\\left(Z-b\\right) }}{1+e^{a\\left(Z-b\\right) }} $$'),
                      h4("Table of parameters"),
                      fluidRow(column(12, align = "center", tableOutput('coef_nlr_3P'))),
                      htmlOutput("nlr_3P_interpretation"),
                      br(),
                      h4("Selected R code"),
                      div(code(HTML("library(difNLR)&nbsp;<br>library(ggplot2)<br><br>#&nbsp;loading&nbsp;data<br>data(GMAT)&nbsp;<br>data&nbsp;<-&nbsp;GMAT[,&nbsp;1:20]&nbsp;<br>zscore&nbsp;<-&nbsp;scale(apply(data,&nbsp;1,&nbsp;sum))&nbsp;#&nbsp;standardized&nbsp;total&nbsp;score<br><br>#&nbsp;NLR&nbsp;3P&nbsp;model&nbsp;for&nbsp;item&nbsp;1&nbsp;<br>fun&nbsp;<-&nbsp;function(x,&nbsp;a,&nbsp;b,&nbsp;c){c&nbsp;+&nbsp;(1&nbsp;-&nbsp;c)&nbsp;*&nbsp;exp(a&nbsp;*&nbsp;(x&nbsp;-&nbsp;b))&nbsp;/&nbsp;(1&nbsp;+&nbsp;exp(a&nbsp;*&nbsp;(x&nbsp;-&nbsp;b)))}&nbsp;<br><br>fit&nbsp;<-&nbsp;nls(data[,&nbsp;1]&nbsp;~&nbsp;fun(zscore,&nbsp;a,&nbsp;b,&nbsp;c),&nbsp;<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;algorithm&nbsp;=&nbsp;\"port\",&nbsp;<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;start&nbsp;=&nbsp;startNLR(data,&nbsp;GMAT[,&nbsp;\"group\"],&nbsp;model&nbsp;=&nbsp;\"3PLcg\",&nbsp;parameterization&nbsp;=&nbsp;\"classic\")[[1]][1:3],<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;lower&nbsp;=&nbsp;c(-Inf,&nbsp;-Inf,&nbsp;0,),<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;upper&nbsp;=&nbsp;c(Inf,&nbsp;Inf,&nbsp;1))&nbsp;<br>#&nbsp;coefficients&nbsp;<br>coef(fit)&nbsp;<br><br>#&nbsp;empirical&nbsp;probabilities&nbsp;calculation<br>df&nbsp;<-&nbsp;data.frame(x&nbsp;=&nbsp;sort(unique(zscore)),<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;y&nbsp;=&nbsp;tapply(data[,&nbsp;1],&nbsp;zscore,&nbsp;mean),<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;size&nbsp;=&nbsp;as.numeric(table(zscore)))<br><br>#&nbsp;plot&nbsp;of&nbsp;estimated&nbsp;curve<br>ggplot(df,&nbsp;aes(x&nbsp;=&nbsp;x,&nbsp;y&nbsp;=&nbsp;y))&nbsp;+<br>&nbsp;&nbsp;geom_point(aes(size&nbsp;=&nbsp;size),<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;color&nbsp;=&nbsp;\"darkblue\",<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;fill&nbsp;=&nbsp;\"darkblue\",<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;shape&nbsp;=&nbsp;21,&nbsp;alpha&nbsp;=&nbsp;0.5)&nbsp;+<br>&nbsp;&nbsp;stat_function(fun&nbsp;=&nbsp;fun,&nbsp;geom&nbsp;=&nbsp;\"line\",<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;args&nbsp;=&nbsp;list(a&nbsp;=&nbsp;coef(fit)[1],<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;b&nbsp;=&nbsp;coef(fit)[2],<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;c&nbsp;=&nbsp;coef(fit)[3]),<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;size&nbsp;=&nbsp;1,<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;color&nbsp;=&nbsp;\"darkblue\")&nbsp;+<br>&nbsp;&nbsp;xlab(\"Standardized&nbsp;total&nbsp;score\")&nbsp;+<br>&nbsp;&nbsp;ylab(\"Probability&nbsp;of&nbsp;correct&nbsp;answer\")&nbsp;+<br>&nbsp;&nbsp;ylim(0,&nbsp;1)&nbsp;+<br>&nbsp;&nbsp;ggtitle(\"Item&nbsp;1\")&nbsp;+&nbsp;<br>&nbsp;&nbsp;theme_app()"))),
                      br()
                      ),
             # * NONLINEAR 4P IRT Z ####
             tabPanel("Nonlinear 4P IRT Z",
                      h3("Nonlinear four parameter regression on standardized total scores with IRT parameterization"),
                      p('Various regression models may be fitted to describe
                        item properties in more detail.',
                        strong('Nonlinear four parameter regression'), 'can model dependency of probability of correct answer on
                        standardized total score (Z-score) by s-shaped logistic curve. The IRT parametrization used here corresponds
                        to the parametrization used in IRT models. Parameter ', strong( '\\(b\\)'),' describes horizontal position of the fitted curve (difficulty),
                        parameter ', strong( '\\(a\\)'), ' describes its slope at inflection point (discrimination), pseudo-guessing parameter ', strong('\\(c\\)'), '
                        is describes lower asymptote and inattention parameter ', strong('\\(d\\)'), 'describes upper asymptote.'),
                      br(),
                      h4("Plot with estimated nonlinear curve"),
                      p('Points represent proportion of correct answer with respect to standardized
                        total score. Their size is determined by count of respondents who achieved given
                        level of standardized total score.'),
                      sliderInput(inputId = "slider_nlr_4P_item", label = "Item",
                                  min = 1, value = 1, max = 10, step = 1, animate = animationOptions(interval = 1200)),
                      plotOutput('nlr_4P_plot'),
                      downloadButton("DB_nlr_4P_plot", label = "Download figure"),
                      h4("Equation"),
                      ('$$\\mathrm{P}(Y = 1|Z, a, b, c,d) = \\mathrm{E}(Y|Z, a, b, c, d) = c + \\left( d-c \\right) \\cdot \\frac{e^{a\\left(Z-b\\right) }}{1+e^{a\\left(Z-b\\right) }} $$'),
                      h4("Table of parameters"),
                      fluidRow(column(12, align = "center", tableOutput('coef_nlr_4P'))),
                      htmlOutput("nlr_4P_interpretation"),
                      br(),
                      h4("Selected R code"),
                      div(code(HTML("library(difNLR)&nbsp;<br>library(ggplot2)<br><br>#&nbsp;loading&nbsp;data<br>data(GMAT)&nbsp;<br>data&nbsp;<-&nbsp;GMAT[,&nbsp;1:20]&nbsp;<br>zscore&nbsp;<-&nbsp;scale(apply(data,&nbsp;1,&nbsp;sum))&nbsp;#&nbsp;standardized&nbsp;total&nbsp;score<br><br>#&nbsp;NLR&nbsp;4P&nbsp;model&nbsp;for&nbsp;item&nbsp;1&nbsp;<br>fun&nbsp;<-&nbsp;function(x,&nbsp;a,&nbsp;b,&nbsp;c,&nbsp;d){c&nbsp;+&nbsp;(d&nbsp;-&nbsp;c)&nbsp;*&nbsp;exp(a&nbsp;*&nbsp;(x&nbsp;-&nbsp;b))&nbsp;/&nbsp;(1&nbsp;+&nbsp;exp(a&nbsp;*&nbsp;(x&nbsp;-&nbsp;b)))}&nbsp;<br><br>fit&nbsp;<-&nbsp;nls(data[,&nbsp;1]&nbsp;~&nbsp;fun(zscore,&nbsp;a,&nbsp;b,&nbsp;c,&nbsp;d),&nbsp;<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;algorithm&nbsp;=&nbsp;\"port\",&nbsp;<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;start&nbsp;=&nbsp;startNLR(data,&nbsp;GMAT[,&nbsp;\"group\"],&nbsp;model&nbsp;=&nbsp;\"4PLcgdg\",&nbsp;parameterization&nbsp;=&nbsp;\"classic\")[[1]][1:4],<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;lower&nbsp;=&nbsp;c(-Inf,&nbsp;-Inf,&nbsp;0,&nbsp;0),<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;upper&nbsp;=&nbsp;c(Inf,&nbsp;Inf,&nbsp;1,&nbsp;1))&nbsp;<br>#&nbsp;coefficients&nbsp;<br>coef(fit)&nbsp;<br><br>#&nbsp;empirical&nbsp;probabilities&nbsp;calculation<br>df&nbsp;<-&nbsp;data.frame(x&nbsp;=&nbsp;sort(unique(zscore)),<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;y&nbsp;=&nbsp;tapply(data[,&nbsp;1],&nbsp;zscore,&nbsp;mean),<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;size&nbsp;=&nbsp;as.numeric(table(zscore)))<br><br>#&nbsp;plot&nbsp;of&nbsp;estimated&nbsp;curve<br>ggplot(df,&nbsp;aes(x&nbsp;=&nbsp;x,&nbsp;y&nbsp;=&nbsp;y))&nbsp;+<br>&nbsp;&nbsp;geom_point(aes(size&nbsp;=&nbsp;size),<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;color&nbsp;=&nbsp;\"darkblue\",<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;fill&nbsp;=&nbsp;\"darkblue\",<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;shape&nbsp;=&nbsp;21,&nbsp;alpha&nbsp;=&nbsp;0.5)&nbsp;+<br>&nbsp;&nbsp;stat_function(fun&nbsp;=&nbsp;fun,&nbsp;geom&nbsp;=&nbsp;\"line\",<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;args&nbsp;=&nbsp;list(a&nbsp;=&nbsp;coef(fit)[1],<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;b&nbsp;=&nbsp;coef(fit)[2],<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;c&nbsp;=&nbsp;coef(fit)[3],<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;d&nbsp;=&nbsp;coef(fit)[4]),<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;size&nbsp;=&nbsp;1,<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;color&nbsp;=&nbsp;\"darkblue\")&nbsp;+<br>&nbsp;&nbsp;xlab(\"Standardized&nbsp;total&nbsp;score\")&nbsp;+<br>&nbsp;&nbsp;ylab(\"Probability&nbsp;of&nbsp;correct&nbsp;answer\")&nbsp;+<br>&nbsp;&nbsp;ylim(0,&nbsp;1)&nbsp;+<br>&nbsp;&nbsp;ggtitle(\"Item&nbsp;1\")&nbsp;+&nbsp;<br>&nbsp;&nbsp;theme_app()"))),
                      br()
                      ),
             # * MODELS COMPARISON ####
             tabPanel("Model comparison",
                      h3("Logistic regression model selection"),
                      p('Here you can compare classic 2PL logistic regression model to non-linear model
                        item by item using some information criteria: '),
                      tags$ul(
                        tags$li(strong('AIC'), 'is the Akaike information criterion (Akaike, 1974), '),
                        tags$li(strong('BIC'), 'is the Bayesian information criterion (Schwarz, 1978)')
                      ),
                      p('Another approach to nested models can be likelihood ratio chi-squared test.
                        Significance level is set to 0.05. As tests are performed item by item, it is
                        possible to use multiple comparison correction method. '),
                      selectInput("correction_method_regrmodels", "Correction method",
                                  choices = c("Benjamini-Hochberg" = "BH",
                                              "Benjamini-Yekutieli" = "BY",
                                              "Holm" = "holm",
                                              "Hochberg" = "hochberg",
                                              "Hommel" = "hommel",
                                              "None" = "none"),
                                  selected = "none"),
                      h4("Table of comparison statistics"),
                      p('Rows ', strong('BEST'), 'indicate which model has the lowest value of criterion, or is the largest
                        significant model by likelihood ratio test.'),
                      DT::dataTableOutput('regr_comp_table'),
                      br(),
                      h4("Selected R code"),
                      div(code(HTML("library(difNLR)&nbsp;<br><br>#&nbsp;loading&nbsp;data<br>data(GMAT)&nbsp;<br>Data&nbsp;<-&nbsp;GMAT[,&nbsp;1:20]&nbsp;<br>zscore&nbsp;<-&nbsp;scale(apply(Data,&nbsp;1,&nbsp;sum))&nbsp;#&nbsp;standardized&nbsp;total&nbsp;score<br><br>#&nbsp;function&nbsp;for&nbsp;fitting&nbsp;models<br>fun&nbsp;<-&nbsp;function(x,&nbsp;a,&nbsp;b,&nbsp;c,&nbsp;d){c&nbsp;+&nbsp;(d&nbsp;-&nbsp;c)&nbsp;*&nbsp;exp(a&nbsp;*&nbsp;(x&nbsp;-&nbsp;b))&nbsp;/&nbsp;(1&nbsp;+&nbsp;exp(a&nbsp;*&nbsp;(x&nbsp;-&nbsp;b)))}&nbsp;<br><br>#&nbsp;starting&nbsp;values&nbsp;for&nbsp;item&nbsp;1<br>start&nbsp;<-&nbsp;startNLR(Data,&nbsp;GMAT[,&nbsp;\"group\"],&nbsp;model&nbsp;=&nbsp;\"4PLcgdg\",&nbsp;parameterization&nbsp;=&nbsp;\"classic\")[[1]][,&nbsp;1:4]<br><br>#&nbsp;2PL&nbsp;model&nbsp;for&nbsp;item&nbsp;1&nbsp;<br>fit2PL&nbsp;<-&nbsp;nls(Data[,&nbsp;1]&nbsp;~&nbsp;fun(zscore,&nbsp;a,&nbsp;b,&nbsp;c&nbsp;=&nbsp;0,&nbsp;d&nbsp;=&nbsp;1),&nbsp;<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;algorithm&nbsp;=&nbsp;\"port\",&nbsp;<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;start&nbsp;=&nbsp;start[1:2])&nbsp;<br>#&nbsp;NLR&nbsp;3P&nbsp;model&nbsp;for&nbsp;item&nbsp;1&nbsp;<br>fit3PL&nbsp;<-&nbsp;nls(Data[,&nbsp;1]&nbsp;~&nbsp;fun(zscore,&nbsp;a,&nbsp;b,&nbsp;c,&nbsp;d&nbsp;=&nbsp;1),&nbsp;<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;algorithm&nbsp;=&nbsp;\"port\",&nbsp;<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;start&nbsp;=&nbsp;start[1:3],<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;lower&nbsp;=&nbsp;c(-Inf,&nbsp;-Inf,&nbsp;0),&nbsp;<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;upper&nbsp;=&nbsp;c(Inf,&nbsp;Inf,&nbsp;1))&nbsp;<br>#&nbsp;NLR&nbsp;4P&nbsp;model&nbsp;for&nbsp;item&nbsp;1&nbsp;<br>fit3PL&nbsp;<-&nbsp;nls(Data[,&nbsp;1]&nbsp;~&nbsp;fun(zscore,&nbsp;a,&nbsp;b,&nbsp;c,&nbsp;d),&nbsp;<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;algorithm&nbsp;=&nbsp;\"port\",&nbsp;<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;start&nbsp;=&nbsp;start,<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;lower&nbsp;=&nbsp;c(-Inf,&nbsp;-Inf,&nbsp;0,&nbsp;0),&nbsp;<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;upper&nbsp;=&nbsp;c(Inf,&nbsp;Inf,&nbsp;1,&nbsp;1))&nbsp;<br><br>#&nbsp;comparison&nbsp;<br>###&nbsp;AIC<br>AIC(fit2PL);&nbsp;AIC(fit3PL);&nbsp;AIC(fit4PL)&nbsp;<br>###&nbsp;BIC<br>BIC(fit2PL);&nbsp;BIC(fit3PL);&nbsp;BIC(fit4PL)&nbsp;<br>###&nbsp;LR&nbsp;test,&nbsp;using&nbsp;Benjamini-Hochberg&nbsp;correction<br>######&nbsp;2PL&nbsp;vs&nbsp;NLR&nbsp;3P<br>LRstat&nbsp;<-&nbsp;-2&nbsp;*&nbsp;(sapply(fit2PL,&nbsp;logLik)&nbsp;-&nbsp;sapply(fit3PL,&nbsp;logLik))&nbsp;<br>LRdf&nbsp;<-&nbsp;1&nbsp;<br>LRpval&nbsp;<-&nbsp;1&nbsp;-&nbsp;pchisq(LRstat,&nbsp;LRdf)&nbsp;<br>LRpval&nbsp;<-&nbsp;p.adjust(LRpval,&nbsp;method&nbsp;=&nbsp;\"BH\")&nbsp;<br>######&nbsp;NLR&nbsp;3P&nbsp;vs&nbsp;NLR&nbsp;4P<br>LRstat&nbsp;<-&nbsp;-2&nbsp;*&nbsp;(sapply(fit3PL,&nbsp;logLik)&nbsp;-&nbsp;sapply(fit4PL,&nbsp;logLik))&nbsp;<br>LRdf&nbsp;<-&nbsp;1&nbsp;<br>LRpval&nbsp;<-&nbsp;1&nbsp;-&nbsp;pchisq(LRstat,&nbsp;LRdf)&nbsp;<br>LRpval&nbsp;<-&nbsp;p.adjust(LRpval,&nbsp;method&nbsp;=&nbsp;\"BH\")"))),
                      br()
                      ),
             "----",
             "Polytomous models",
             # * MULTINOMIAL ####
             tabPanel("Cumulative Logistic ",
                      h3("Cumulative logistic regression on total scores"),
                      p("Various regression models may be fitted to describe item properties in more detail.", strong("Cumulative
                         logistic regression")," can model cumulative probabilities, i.e., probabilities to obtain item score higher than or equal to 1,
                        2, 3, etc. Parameters \\(b_{0k}\\) describe horizontal position of the fitted curves, where \\(k = 0, 1,2,..\\) is number
                        of obtained scores, parameter \\(b_1\\) describes their common slope. Category probabilities are then described as
                        differences of two subsequent cumulative probabilities."),
                      h4("Plot of cumulative probabilities"),
                      p("Lines determine the cumulative probabilities \\(P(Y \\geq k)\\). Circles represent proportion of answers with
                        at least k points with respect to the total score, i.e., the empirical cumulative probabilities. The size of the circles is
                        determined by the count of respondents who achieved given level of the total score."),
                      plotOutput("cum_log"),
                      h4("Plot of category probabilities"),
                      p("Lines determine the category probabilities \\(P(Y=k)\\). Circles represent proportion of answers with k
                        points with respect to the total score, i.e., the empirical category probabilities. The size of the circles is determined 
                        by the count of respondents who achieved given level of the total score."),
                      plotOutput("cum_cat_log"),
                      h4("Equation"),
                      ('$$P(Y \\geq k|X,b_{0k},b_{1}) = \\frac{e^{b_{0k} + b_{1}X}}{1 + e^{b_{0k} + b_{1}X}}$$'),
                      h4("Table of parameters"),
                      tableOutput("coef_cum_log")),
             tabPanel("Cumulative Logistic Z",
                      h3("Cumulative logistic regression on standardized total scores"),
                      p("Various regression models may be fitted to describe item properties in more detail.", strong("Cumulative
                        logistic regression")," can model cumulative probabilities, i.e., probabilities to obtain item score higher than or equal to 1,
                        2, 3, etc. Parameters \\(b_{0k}\\) describe horizontal position of the fitted curves, where \\(k = 0, 1,2,..\\) is number
                        of obtained scores, parameter \\(b_1\\) describes their common slope. Category probabilities are then described as
                        differences of two subsequent cumulative probabilities."),
                      h4("Plot of cumulative probabilities"),
                      p("Lines determine the cumulative probabilities \\(P(Y \\geq k)\\). Circles represent proportion of answers with
                        at least k points with respect to the standardized total score, i.e., the empirical cumulative probabilities. The size of the circles is
                        determined by the count of respondents who achieved given level of the standardized total score."),
                      plotOutput("cum_log"),
                      h4("Plot of category probabilities"),
                      p("Lines determine the category probabilities \\(P(Y=k)\\). Circles represent proportion of answers with k
                        points with respect to the standardized total score, i.e., the empirical category probabilities. The size of the circles is determined 
                        by the count of respondents who achieved given level of the standardized total score."),
                      plotOutput("cum_cat_log"),
                      h4("Equation"),
                      ('$$P(Y \\geq k|Z,b_{0k},b_{1}) = \\frac{e^{b_{0k} + b_{1}Z}}{1 + e^{b_{0k} + b_{1}Z}}$$'),
                      h4("Table of parameters"),
                      tableOutput("coef_cum_log")),
             tabPanel("Cumulative Logistic IRT Z",
                      h3("Cumulative logistic regression on standardized total scores with IRT parameterization"),
                      p("Various regression models may be fitted to describe item properties in more detail.", strong("Cumulative
                        logistic regression")," can model cumulative probabilities, i.e., probabilities to obtain item score higher than or equal to 1,
                        2, 3, etc. Note the change in the parametrization - the IRT parametrization used here corresponds to the
                        parametrization used in IRT models. Parameter \\(b_k\\), where \\(k = 0, 1,2,\\cdots \\) describes horizontal
                        position of the fitted curves (difficulties), parameter \\(a\\) describes their slope at inflection point
                        (discrimination). Category probabilities are then described as differences of two subsequent
                        cumulative probabilities."),
                      h4("Plot with cumulative probabilities"),
                      p("Lines determine the cumulative probabilities \\(P(Y \\geq k)\\). Circles represent proportion of answers with
                        at least k points with respect to the standardized total score, i.e. the empirical cumulative probabilities. 
                        The size of the circles is determined by the count of respondents who achieved given level of the standardized total score."),
                      plotOutput("cum_log_irt_z"),
                      h4("Plot with category probabilities"),
                      p("Lines determine the category probabilities \\(P(Y=k)\\). Circles represent proportion of answers with k
                        points with respect to the total score, i.e. the empirical category probabilities. The size of the circles is determined by
                        count of the respondents who achieved given level of the standardized total score."),
                      plotOutput("cum_cat_log_irt_z"),
                      h4("Equation"),
                      ('$$P(Y \\geq k|Z,b_{k},b_{1}) = \\frac{e^{a(Z - b_{k})}}{1 + e^{a(Z - b_{k})}}$$'),
                      h4("Table of parameters"),
                      tableOutput("coef_cum_log_irt_z")),
             tabPanel("Adjacent Logistic ",
                      h3("Adjacent logistic regression on total scores"),
                      p("Models for ordinal responses need not use cumulative probabilities.",strong("Adjacent
                        categories model"), "assumes linear form of logarithm of ratio of probabilities of two successive scores (e.g. 1 vs. 2, 2 vs. 3, etc.),
                        i.e., of the adjacent category logits."),
                      h4("Plot with category probabilities"),
                      p("Lines determine the category probabilities \\(P(Y=k)\\). Circles represent the proportion of answers with k
                        points with respect to the total score, i. e., the empirical category probabilities. The size of the circles is determined by
                        the count of respondents who achieved given level of the total score."),
                      plotOutput("adj_log"),
                      h4("Equation"),
                      ('$$P(Y = k|X,a,d_{1},d_{2},\\cdots) = \\frac{e^{\\sum_{t = 0}^{k}(d_{t} + aX)}}{\\sum_{r = 0}^{K}e^{\\sum_{t = 0}^{r}(d_{t} + aX)}}$$'),
                      h4("Table of parameters"),
                      tableOutput("coef_adj_log")),
             tabPanel("Adjacent Logistic Z",
                      h3("Adjacent logistic regression on standardized scores"),
                      p("Models for ordinal responses need not use cumulative probabilities.",strong("Adjacent
                        categories model"), "assumes linear form of logarithm of ratio of probabilities of two successive scores (e.g. 1 vs. 2, 2 vs. 3, etc.),
                        i.e., of the adjacent category logits."),
                      h4("Plot with category probabilities"),
                      p("Lines determine the category probabilities \\(P(Y=k)\\). Circles represent the proportion of answers with k
                        points with respect to the standardized total score, i. e., the empirical category probabilities. The size of the circles is determined by
                        the count of respondents who achieved given level of the standardized total score."),
                      plotOutput("adj_log_z"),
                      h4("Equation"),
                      ('$$P(Y = k|Z,a,d_{1},d_{2},\\cdots) = \\frac{e^{\\sum_{t = 0}^{k}(d_{t} + aZ)}}{\\sum_{r = 0}^{K}e^{\\sum_{t = 0}^{r}(d_{t} + aZ)}}$$'),
                      h4("Table of parameters"),
                      tableOutput("coef_adj_log_z")),
             tabPanel("Adjacent Logistic IRT Z",
                      h3("Adjacent logistic regression on standardized total scores with IRT parameterization"),
                      p("Models for ordinal responses need not use cumulative probabilities.",strong("Adjacent
                        categories model"), "assumes linear form of logarithm of ratio of probabilities of two successive scores (e.g. 1 vs. 2, 2 vs. 3, etc.),
                        i.e., of the adjacent category logits."),
                      h4("Plot with category probabilities"),
                      p("Lines determine the category probabilities \\(P(Y=k)\\). Circles represent the proportion of answers with k
                        points with respect to the standardized total score, i. e., the empirical category probabilities. The size of the circles is determined by
                        the count of respondents who achieved given level of the standardized total score."),
                      plotOutput("adj_log_irt_z"),
                      h4("Equation"),
                      ('$$P(Y = k|Z,a - \\delta_{1},\\delta_{2},\\cdots) = \\frac{e^{\\sum_{t = 0}^{k}(Z - \\delta_{t})}}{\\sum_{r = 0}^{K}e^{\\sum_{t = 0}^{r}(Z-\\delta_{t})}}$$'),
                      h4("Table of parameters"),
                      tableOutput("coef_adj_log_irt_z")),
             tabPanel("Multinomial",
                      h3("Multinomial regression on standardized total scores"),
                      p('Various regression models may be fitted to describe
                        item properties in more detail.',
                        strong('Multinomial regression'),'allows for simultaneous modelling of probability of choosing
                        given distractors on standardized total score (Z-score).'),
                      br(),
                      h4("Plot with estimated curves of multinomial regression"),
                      p('Points represent proportion of selected option with respect to standardized
                        total score. Their size is determined by count of respondents who achieved given
                        level of standardized total score and who selected given option.'),
                      sliderInput("multiSlider", "Item",
                                  min = 1, value = 1, max = 10,
                                  step = 1, animate = animationOptions(interval = 1200)),
                      plotOutput('multi_plot'),
                      downloadButton("DB_multi_plot", label = "Download figure"),
                      h4("Equation"),
                      uiOutput('multi_equation'),
                      h4("Table of parameters"),
                      fluidRow(column(12, align = "center", tableOutput('coef_multi'))),
                      strong("Interpretation:"),
                      htmlOutput("multi_interpretation"),
                      br(),
                      h4("Selected R code"),
                      div(code(HTML("library(difNLR)&nbsp;<br>library(nnet)&nbsp;<br><br>#&nbsp;loading&nbsp;data<br>data(GMAT,&nbsp;GMATtest,&nbsp;GMATkey)&nbsp;<br>zscore&nbsp;<-&nbsp;scale(apply(GMAT[,&nbsp;1:20]&nbsp;,&nbsp;1,&nbsp;sum))&nbsp;#&nbsp;standardized&nbsp;total&nbsp;score<br>data&nbsp;<-&nbsp;GMATtest[,&nbsp;1:20]&nbsp;<br>key&nbsp;<-GMATkey<br><br>#&nbsp;multinomial&nbsp;model&nbsp;for&nbsp;item&nbsp;1&nbsp;<br>fit&nbsp;<-&nbsp;multinom(relevel(data[,&nbsp;1],&nbsp;ref&nbsp;=&nbsp;paste(key[1]))&nbsp;~&nbsp;zscore)&nbsp;<br><br>#&nbsp;coefficients&nbsp;<br>coef(fit)"))),
                      br()
                      )
             )
