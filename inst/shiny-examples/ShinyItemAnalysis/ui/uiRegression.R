uiRegression <-
  navbarMenu(
    "Regression",
    "Dichotomous models",
    # * LOGISTIC ####
    tabPanel(
      "Logistic",
      h3("Logistic regression on total scores"),
      withMathJax(),
      p(
        "Various regression models may be fitted to describe item properties in more detail.", strong("Logistic regression"),
        "can model dependency of probability of correctly answering item \\(i\\) by respondent \\(p\\) on their
                        total score \\(X_p\\) by S-shaped logistic curve. Parameter", strong("\\(\\beta_{i0}\\)"), " describes
                        horizontal position of the fitted curve and parameter ", strong("\\(\\beta_{i1}\\)"), " describes its slope."
      ),
      br(),
      h4("Plot with estimated logistic curve"),
      p("Points represent proportion of correct answers with respect to total score. Their size is determined by count of respondents
                        who achieved given level of total score."),
      sliderInput(
        inputId = "regression_logistic_item_slider", label = "Item",
        min = 1, value = 1, max = 10,
        step = 1, animate = animationOptions(interval = 1200)
      ),
      uiOutput("regression_logistic_na_alert"),
      plotlyOutput("regression_logistic_plot"),
      downloadButton("regression_logistic_plot_download", label = "Download figure"),
      h4("Equation"),
      withMathJax(),
      ("$$\\mathrm{P}(Y_{pi} = 1|X_p) = \\mathrm{E}(Y_{pi}|X_p) = \\frac{e^{\\left(\\beta_{i0} + \\beta_{i1} X_p\\right)}}{1 + e^{\\left(\\beta_{i0} + \\beta_{i1} X_p\\right)}}$$"),
      h4("Table of parameters"),
      fluidRow(column(12, align = "center", tableOutput("regression_logistic_coef"))),
      htmlOutput("regression_logistic_interpretation"),
      br(),
      h4("Selected R code"),
      div(code(HTML("library(ggplot2)<br><br>#&nbsp;loading&nbsp;data<br>data(GMAT,&nbsp;package&nbsp;=&nbsp;\"difNLR\")<br>data&nbsp;<-&nbsp;GMAT[,&nbsp;1:20]<br>score&nbsp;<-&nbsp;rowSums(data)&nbsp;#&nbsp;total&nbsp;score<br><br>#&nbsp;logistic&nbsp;model&nbsp;for&nbsp;item&nbsp;1<br>fit&nbsp;<-&nbsp;glm(data[,&nbsp;1]&nbsp;~&nbsp;score,&nbsp;family&nbsp;=&nbsp;binomial)<br><br>#&nbsp;coefficients<br>coef(fit)&nbsp;#&nbsp;estimates<br>sqrt(diag(vcov(fit)))&nbsp;#&nbsp;SE<br>summary(fit)$coefficients[,&nbsp;1:2]&nbsp;#&nbsp;estimates&nbsp;and&nbsp;SE<br><br>#&nbsp;function&nbsp;for&nbsp;plot<br>fun&nbsp;<-&nbsp;function(x,&nbsp;b0,&nbsp;b1)&nbsp;{<br>&nbsp;&nbsp;exp(b0&nbsp;+&nbsp;b1&nbsp;*&nbsp;x)&nbsp;/&nbsp;(1&nbsp;+&nbsp;exp(b0&nbsp;+&nbsp;b1&nbsp;*&nbsp;x))<br>}<br><br>#&nbsp;empirical&nbsp;probabilities&nbsp;calculation<br>df&nbsp;<-&nbsp;data.frame(<br>&nbsp;&nbsp;x&nbsp;=&nbsp;sort(unique(score)),<br>&nbsp;&nbsp;y&nbsp;=&nbsp;tapply(data[,&nbsp;1],&nbsp;score,&nbsp;mean),<br>&nbsp;&nbsp;size&nbsp;=&nbsp;as.numeric(table(score))<br>)<br><br>#&nbsp;plot&nbsp;of&nbsp;estimated&nbsp;curve<br>ggplot(df,&nbsp;aes(x&nbsp;=&nbsp;x,&nbsp;y&nbsp;=&nbsp;y))&nbsp;+<br>&nbsp;&nbsp;geom_point(aes(size&nbsp;=&nbsp;size),<br>&nbsp;&nbsp;&nbsp;&nbsp;color&nbsp;=&nbsp;\"darkblue\",<br>&nbsp;&nbsp;&nbsp;&nbsp;fill&nbsp;=&nbsp;\"darkblue\",<br>&nbsp;&nbsp;&nbsp;&nbsp;shape&nbsp;=&nbsp;21,&nbsp;alpha&nbsp;=&nbsp;0.5<br>&nbsp;&nbsp;)&nbsp;+<br>&nbsp;&nbsp;stat_function(<br>&nbsp;&nbsp;&nbsp;&nbsp;fun&nbsp;=&nbsp;fun,&nbsp;geom&nbsp;=&nbsp;\"line\",<br>&nbsp;&nbsp;&nbsp;&nbsp;args&nbsp;=&nbsp;list(<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;b0&nbsp;=&nbsp;coef(fit)[1],<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;b1&nbsp;=&nbsp;coef(fit)[2]<br>&nbsp;&nbsp;&nbsp;&nbsp;),<br>&nbsp;&nbsp;&nbsp;&nbsp;size&nbsp;=&nbsp;1,<br>&nbsp;&nbsp;&nbsp;&nbsp;color&nbsp;=&nbsp;\"darkblue\"<br>&nbsp;&nbsp;)&nbsp;+<br>&nbsp;&nbsp;xlab(\"Total&nbsp;score\")&nbsp;+<br>&nbsp;&nbsp;ylab(\"Probability&nbsp;of&nbsp;correct&nbsp;answer\")&nbsp;+<br>&nbsp;&nbsp;ylim(0,&nbsp;1)&nbsp;+<br>&nbsp;&nbsp;ggtitle(\"Item&nbsp;1\")&nbsp;+<br>&nbsp;&nbsp;theme_app()<br>"))),
      br()
    ),
    # * LOGISTIC Z ####
    tabPanel(
      "Logistic Z",
      h3("Logistic regression on standardized total scores"),
      p(
        "Various regression models may be fitted to describe item properties in more detail.",
        strong("Logistic regression"), "can model dependency of probability of correctly answering item \\(i\\) by respondent
                        \\(p\\) on their standardized total score \\(Z_p\\) (Z-score) by S-shaped logistic curve. Parameter",
        strong("\\(\\beta_{i0}\\)"), " describes horizontal position of the fitted curve and parameter ",
        strong("\\(\\beta_{i1}\\)"), " describes its slope."
      ),
      br(),
      h4("Plot with estimated logistic curve"),
      p("Points represent proportion of correct answers with respect to standardized total score. Their size is determined by
                        count of respondents who achieved given level of standardized total score."),
      sliderInput("regression_logistic_Z_item_slider", "Item",
        min = 1, value = 1, max = 10,
        step = 1, animate = animationOptions(interval = 1200)
      ),
      uiOutput("regression_logistic_Z_na_alert"),
      plotlyOutput("regression_logistic_Z_plot"),
      downloadButton("regression_logistic_Z_plot_download", label = "Download figure"),
      h4("Equation"),
      ("$$\\mathrm{P}(Y_{pi} = 1|Z_p) = \\mathrm{E}(Y_{pi}|Z_p) = \\frac{e^{\\left(\\beta_{i0} + \\beta_{i1} Z_p\\right)}}{1 + e^{\\left(\\beta_{i0} + \\beta_{i1} Z_p\\right)}}$$"),
      h4("Table of parameters"),
      fluidRow(column(12, align = "center", tableOutput("regression_logistic_Z_coef"))),
      htmlOutput("regression_logistic_Z_interpretation"),
      br(),
      h4("Selected R code"),
      div(code(HTML("library(ggplot2)<br><br>#&nbsp;loading&nbsp;data<br>data(GMAT,&nbsp;package&nbsp;=&nbsp;\"difNLR\")<br>data&nbsp;<-&nbsp;GMAT[,&nbsp;1:20]<br>zscore&nbsp;<-&nbsp;scale(rowSums(data))&nbsp;#&nbsp;standardized&nbsp;total&nbsp;score<br><br>#&nbsp;logistic&nbsp;model&nbsp;for&nbsp;item&nbsp;1<br>fit&nbsp;<-&nbsp;glm(data[,&nbsp;1]&nbsp;~&nbsp;zscore,&nbsp;family&nbsp;=&nbsp;binomial)<br><br>#&nbsp;coefficients<br>coef(fit)&nbsp;#&nbsp;estimates<br>sqrt(diag(vcov(fit)))&nbsp;#&nbsp;SE<br>summary(fit)$coefficients[,&nbsp;1:2]&nbsp;#&nbsp;estimates&nbsp;and&nbsp;SE<br><br>#&nbsp;function&nbsp;for&nbsp;plot<br>fun&nbsp;<-&nbsp;function(x,&nbsp;b0,&nbsp;b1)&nbsp;{<br>&nbsp;&nbsp;exp(b0&nbsp;+&nbsp;b1&nbsp;*&nbsp;x)&nbsp;/&nbsp;(1&nbsp;+&nbsp;exp(b0&nbsp;+&nbsp;b1&nbsp;*&nbsp;x))<br>}<br><br>#&nbsp;empirical&nbsp;probabilities&nbsp;calculation<br>df&nbsp;<-&nbsp;data.frame(<br>&nbsp;&nbsp;x&nbsp;=&nbsp;sort(unique(zscore)),<br>&nbsp;&nbsp;y&nbsp;=&nbsp;tapply(data[,&nbsp;1],&nbsp;zscore,&nbsp;mean),<br>&nbsp;&nbsp;size&nbsp;=&nbsp;as.numeric(table(zscore))<br>)<br><br>#&nbsp;plot&nbsp;of&nbsp;estimated&nbsp;curve<br>ggplot(df,&nbsp;aes(x&nbsp;=&nbsp;x,&nbsp;y&nbsp;=&nbsp;y))&nbsp;+<br>&nbsp;&nbsp;geom_point(aes(size&nbsp;=&nbsp;size),<br>&nbsp;&nbsp;&nbsp;&nbsp;color&nbsp;=&nbsp;\"darkblue\",<br>&nbsp;&nbsp;&nbsp;&nbsp;fill&nbsp;=&nbsp;\"darkblue\",<br>&nbsp;&nbsp;&nbsp;&nbsp;shape&nbsp;=&nbsp;21,&nbsp;alpha&nbsp;=&nbsp;0.5<br>&nbsp;&nbsp;)&nbsp;+<br>&nbsp;&nbsp;stat_function(<br>&nbsp;&nbsp;&nbsp;&nbsp;fun&nbsp;=&nbsp;fun,&nbsp;geom&nbsp;=&nbsp;\"line\",<br>&nbsp;&nbsp;&nbsp;&nbsp;args&nbsp;=&nbsp;list(<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;b0&nbsp;=&nbsp;coef(fit)[1],<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;b1&nbsp;=&nbsp;coef(fit)[2]<br>&nbsp;&nbsp;&nbsp;&nbsp;),<br>&nbsp;&nbsp;&nbsp;&nbsp;size&nbsp;=&nbsp;1,<br>&nbsp;&nbsp;&nbsp;&nbsp;color&nbsp;=&nbsp;\"darkblue\"<br>&nbsp;&nbsp;)&nbsp;+<br>&nbsp;&nbsp;xlab(\"Standardized&nbsp;total&nbsp;score\")&nbsp;+<br>&nbsp;&nbsp;ylab(\"Probability&nbsp;of&nbsp;correct&nbsp;answer\")&nbsp;+<br>&nbsp;&nbsp;ylim(0,&nbsp;1)&nbsp;+<br>&nbsp;&nbsp;ggtitle(\"Item&nbsp;1\")&nbsp;+<br>&nbsp;&nbsp;theme_app()<br>"))),
      br()
    ),
    # * LOGISTIC IRT Z ####
    tabPanel(
      "Logistic IRT Z",
      h3("Logistic regression on standardized total scores with IRT parameterization"),
      p(
        "Various regression models may be fitted to describe item properties in more detail.", strong("Logistic regression"),
        "can model dependency of probability of correctly answering item \\(i\\) by respondent \\(p\\) on
                        their standardized total score \\(Z_p\\) (Z-score) by S-shaped logistic curve. Note change in
                        parametrization - the IRT parametrization used here corresponds to the parametrization used in IRT
                        models. Parameter", strong("\\(b_{i}\\)"), " describes horizontal position of the fitted curve
                        (difficulty) and parameter ", strong("\\(a_{i}\\)"), " describes its slope at inflection point
                        (discrimination). "
      ),
      br(),
      h4("Plot with estimated logistic curve"),
      p("Points represent proportion of correct answers with respect to standardized total score. Their size is determined by
                        count of respondents who achieved given level of standardized total score."),
      sliderInput(
        inputId = "regression_logistic_IRT_item_slider", label = "Item",
        min = 1, value = 1, max = 10,
        step = 1, animate = animationOptions(interval = 1200)
      ),
      uiOutput("regression_logistic_IRT_na_alert"),
      plotlyOutput("regression_logistic_IRT_plot"),
      downloadButton("regression_logistic_IRT_plot_download", label = "Download figure"),
      h4("Equation"),
      ("$$\\mathrm{P}(Y_{pi} = 1|Z_p) = \\mathrm{E}(Y_{pi}|Z_p) = \\frac{e^{a_i\\left(Z_p - b_i\\right)}}{1 + e^{a_i\\left(Z_p - b_i\\right)}}$$"),
      h4("Table of parameters"),
      fluidRow(column(12, align = "center", tableOutput("regression_logistic_IRT_coef"))),
      htmlOutput("regression_logistic_IRT_interpretation"),
      br(),
      h4("Selected R code"),
      div(code(HTML("library(ggplot2)<br>library(msm)<br><br>#&nbsp;loading&nbsp;data<br>data(GMAT,&nbsp;package&nbsp;=&nbsp;\"difNLR\")<br>data&nbsp;<-&nbsp;GMAT[,&nbsp;1:20]<br>zscore&nbsp;<-&nbsp;scale(rowSums(data))&nbsp;#&nbsp;standardized&nbsp;total&nbsp;score<br><br>#&nbsp;logistic&nbsp;model&nbsp;for&nbsp;item&nbsp;1<br>fit&nbsp;<-&nbsp;glm(data[,&nbsp;1]&nbsp;~&nbsp;zscore,&nbsp;family&nbsp;=&nbsp;binomial)<br><br>#&nbsp;coefficients<br>(coef&nbsp;<-&nbsp;c(a&nbsp;=&nbsp;coef(fit)[2],&nbsp;b&nbsp;=&nbsp;-coef(fit)[1]&nbsp;/&nbsp;coef(fit)[2]))&nbsp;#&nbsp;estimates<br>#&nbsp;SE&nbsp;using&nbsp;delta&nbsp;method<br>(se&nbsp;<-&nbsp;deltamethod(<br>&nbsp;&nbsp;list(~x2,&nbsp;~&nbsp;-x1&nbsp;/&nbsp;x2),<br>&nbsp;&nbsp;mean&nbsp;=&nbsp;coef(fit),<br>&nbsp;&nbsp;cov&nbsp;=&nbsp;vcov(fit),<br>&nbsp;&nbsp;ses&nbsp;=&nbsp;TRUE<br>))<br>cbind(coef,&nbsp;se)&nbsp;#&nbsp;estimates&nbsp;and&nbsp;SE<br><br>#&nbsp;function&nbsp;for&nbsp;plot<br>fun&nbsp;<-&nbsp;function(x,&nbsp;a,&nbsp;b)&nbsp;{<br>&nbsp;&nbsp;exp(a&nbsp;*&nbsp;(x&nbsp;-&nbsp;b))&nbsp;/&nbsp;(1&nbsp;+&nbsp;exp(a&nbsp;*&nbsp;(x&nbsp;-&nbsp;b)))<br>}<br><br>#&nbsp;empirical&nbsp;probabilities&nbsp;calculation<br>df&nbsp;<-&nbsp;data.frame(<br>&nbsp;&nbsp;x&nbsp;=&nbsp;sort(unique(zscore)),<br>&nbsp;&nbsp;y&nbsp;=&nbsp;tapply(data[,&nbsp;1],&nbsp;zscore,&nbsp;mean),<br>&nbsp;&nbsp;size&nbsp;=&nbsp;as.numeric(table(zscore))<br>)<br><br>#&nbsp;plot&nbsp;of&nbsp;estimated&nbsp;curve<br>ggplot(df,&nbsp;aes(x&nbsp;=&nbsp;x,&nbsp;y&nbsp;=&nbsp;y))&nbsp;+<br>&nbsp;&nbsp;geom_point(aes(size&nbsp;=&nbsp;size),<br>&nbsp;&nbsp;&nbsp;&nbsp;color&nbsp;=&nbsp;\"darkblue\",<br>&nbsp;&nbsp;&nbsp;&nbsp;fill&nbsp;=&nbsp;\"darkblue\",<br>&nbsp;&nbsp;&nbsp;&nbsp;shape&nbsp;=&nbsp;21,&nbsp;alpha&nbsp;=&nbsp;0.5<br>&nbsp;&nbsp;)&nbsp;+<br>&nbsp;&nbsp;stat_function(<br>&nbsp;&nbsp;&nbsp;&nbsp;fun&nbsp;=&nbsp;fun,&nbsp;geom&nbsp;=&nbsp;\"line\",<br>&nbsp;&nbsp;&nbsp;&nbsp;args&nbsp;=&nbsp;list(<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;a&nbsp;=&nbsp;coef[1],<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;b&nbsp;=&nbsp;coef[2]<br>&nbsp;&nbsp;&nbsp;&nbsp;),<br>&nbsp;&nbsp;&nbsp;&nbsp;size&nbsp;=&nbsp;1,<br>&nbsp;&nbsp;&nbsp;&nbsp;color&nbsp;=&nbsp;\"darkblue\"<br>&nbsp;&nbsp;)&nbsp;+<br>&nbsp;&nbsp;xlab(\"Standardized&nbsp;total&nbsp;score\")&nbsp;+<br>&nbsp;&nbsp;ylab(\"Probability&nbsp;of&nbsp;correct&nbsp;answer\")&nbsp;+<br>&nbsp;&nbsp;ylim(0,&nbsp;1)&nbsp;+<br>&nbsp;&nbsp;ggtitle(\"Item&nbsp;1\")&nbsp;+<br>&nbsp;&nbsp;theme_app()<br>"))),
      br()
    ),
    # * NONLINEAR 3P IRT Z ####
    tabPanel(
      "Nonlinear 3P IRT Z",
      h3("Nonlinear three parameter regression on standardized total scores with IRT parameterization"),
      p(
        "Various regression models may be fitted to describe item properties in more detail.", strong("Nonlinear regression"), "can model
                        dependency of probability of correctly answering item \\(i\\) by respondent \\(p\\) on their standardized total
                        score \\(Z_p\\) (Z-score) by S-shaped logistic curve. The IRT parametrization used here corresponds to the
                        parametrization used in IRT models. Parameter", strong("\\(b_{i}\\)"), " describes horizontal position of the
                        fitted curve (difficulty) and parameter ", strong("\\(a_{i}\\)"), " describes its slope at inflection point
                        (discrimination). This model allows for nonzero lower left asymptote ", strong("\\(c_i\\)"), " (pseudo-guessing
                        parameter). "
      ),
      br(),
      h4("Plot with estimated nonlinear curve"),
      p("Points represent proportion of correct answers with respect to standardized total score. Their size is determined by count of
                        respondents who achieved given level of standardized total score."),
      sliderInput(
        inputId = "regression_3pl_item_slider", label = "Item",
        min = 1, value = 1, max = 10, step = 1, animate = animationOptions(interval = 1200)
      ),
      uiOutput("regression_3pl_na_alert"),
      plotlyOutput("regression_3pl_plot"),
      downloadButton("regression_3pl_plot_download", label = "Download figure"),
      h4("Equation"),
      ("$$\\mathrm{P}(Y_{pi} = 1|Z_p) = \\mathrm{E}(Y_{pi}|Z_p) = c_i + \\left(1 - c_i\\right) \\cdot \\frac{e^{a_i\\left(Z_p - b_i\\right)}}{1 + e^{a_i\\left(Z_p - b_i\\right)}}$$"),
      h4("Table of parameters"),
      fluidRow(column(12, align = "center", tableOutput("regression_3pl_coef"))),
      htmlOutput("regression_3pl_interpretation"),
      br(),
      h4("Selected R code"),
      div(code(HTML("library(difNLR)<br>library(ggplot2)<br><br>#&nbsp;loading&nbsp;data<br>data(GMAT,&nbsp;package&nbsp;=&nbsp;\"difNLR\")<br>data&nbsp;<-&nbsp;GMAT[,&nbsp;1:20]<br>zscore&nbsp;<-&nbsp;scale(rowSums(data))&nbsp;#&nbsp;standardized&nbsp;total&nbsp;score<br><br>#&nbsp;NLR&nbsp;3P&nbsp;model&nbsp;for&nbsp;item&nbsp;1<br>fun&nbsp;<-&nbsp;function(x,&nbsp;a,&nbsp;b,&nbsp;c)&nbsp;{<br>&nbsp;&nbsp;c&nbsp;+&nbsp;(1&nbsp;-&nbsp;c)&nbsp;*&nbsp;exp(a&nbsp;*&nbsp;(x&nbsp;-&nbsp;b))&nbsp;/&nbsp;(1&nbsp;+&nbsp;exp(a&nbsp;*&nbsp;(x&nbsp;-&nbsp;b)))<br>}<br><br>fit&nbsp;<-&nbsp;nls(data[,&nbsp;1]&nbsp;~&nbsp;fun(zscore,&nbsp;a,&nbsp;b,&nbsp;c),<br>&nbsp;&nbsp;algorithm&nbsp;=&nbsp;\"port\",<br>&nbsp;&nbsp;start&nbsp;=&nbsp;startNLR(<br>&nbsp;&nbsp;&nbsp;&nbsp;data,&nbsp;GMAT[,&nbsp;\"group\"],<br>&nbsp;&nbsp;&nbsp;&nbsp;model&nbsp;=&nbsp;\"3PLcg\",<br>&nbsp;&nbsp;&nbsp;&nbsp;parameterization&nbsp;=&nbsp;\"classic\"<br>&nbsp;&nbsp;)[[1]][1:3],<br>&nbsp;&nbsp;lower&nbsp;=&nbsp;c(-Inf,&nbsp;-Inf,&nbsp;0),<br>&nbsp;&nbsp;upper&nbsp;=&nbsp;c(Inf,&nbsp;Inf,&nbsp;1)<br>)<br>#&nbsp;coefficients<br>coef(fit)&nbsp;#&nbsp;estimates<br>sqrt(diag(vcov(fit)))&nbsp;#&nbsp;SE<br>summary(fit)$coefficients[,&nbsp;1:2]&nbsp;#&nbsp;estimates&nbsp;and&nbsp;SE<br><br>#&nbsp;empirical&nbsp;probabilities&nbsp;calculation<br>df&nbsp;<-&nbsp;data.frame(<br>&nbsp;&nbsp;x&nbsp;=&nbsp;sort(unique(zscore)),<br>&nbsp;&nbsp;y&nbsp;=&nbsp;tapply(data[,&nbsp;1],&nbsp;zscore,&nbsp;mean),<br>&nbsp;&nbsp;size&nbsp;=&nbsp;as.numeric(table(zscore))<br>)<br><br>#&nbsp;plot&nbsp;of&nbsp;estimated&nbsp;curve<br>ggplot(df,&nbsp;aes(x&nbsp;=&nbsp;x,&nbsp;y&nbsp;=&nbsp;y))&nbsp;+<br>&nbsp;&nbsp;geom_point(aes(size&nbsp;=&nbsp;size),<br>&nbsp;&nbsp;&nbsp;&nbsp;color&nbsp;=&nbsp;\"darkblue\",<br>&nbsp;&nbsp;&nbsp;&nbsp;fill&nbsp;=&nbsp;\"darkblue\",<br>&nbsp;&nbsp;&nbsp;&nbsp;shape&nbsp;=&nbsp;21,&nbsp;alpha&nbsp;=&nbsp;0.5<br>&nbsp;&nbsp;)&nbsp;+<br>&nbsp;&nbsp;stat_function(<br>&nbsp;&nbsp;&nbsp;&nbsp;fun&nbsp;=&nbsp;fun,&nbsp;geom&nbsp;=&nbsp;\"line\",<br>&nbsp;&nbsp;&nbsp;&nbsp;args&nbsp;=&nbsp;list(<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;a&nbsp;=&nbsp;coef(fit)[1],<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;b&nbsp;=&nbsp;coef(fit)[2],<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;c&nbsp;=&nbsp;coef(fit)[3]<br>&nbsp;&nbsp;&nbsp;&nbsp;),<br>&nbsp;&nbsp;&nbsp;&nbsp;size&nbsp;=&nbsp;1,<br>&nbsp;&nbsp;&nbsp;&nbsp;color&nbsp;=&nbsp;\"darkblue\"<br>&nbsp;&nbsp;)&nbsp;+<br>&nbsp;&nbsp;xlab(\"Standardized&nbsp;total&nbsp;score\")&nbsp;+<br>&nbsp;&nbsp;ylab(\"Probability&nbsp;of&nbsp;correct&nbsp;answer\")&nbsp;+<br>&nbsp;&nbsp;ylim(0,&nbsp;1)&nbsp;+<br>&nbsp;&nbsp;ggtitle(\"Item&nbsp;1\")&nbsp;+<br>&nbsp;&nbsp;theme_app()<br>"))),
      br()
    ),
    # * NONLINEAR 4P IRT Z ####
    tabPanel(
      "Nonlinear 4P IRT Z",
      h3("Nonlinear four parameter regression on standardized total scores with IRT parameterization"),
      p(
        "Various regression models may be fitted to describe item properties in more detail.", strong("Nonlinear regression"),
        "can model dependency of probability of correctly answering item \\(i\\) by respondent \\(p\\) on their
                        standardized total score \\(Z_p\\) (Z-score) by S-shaped logistic curve. The IRT parametrization used here
                        corresponds to the parametrization used in IRT models. Parameter", strong("\\(b_{i}\\)"), " describes
                        horizontal position of the fitted curve (difficulty), parameter ", strong("\\(a_{i}\\)"), " describes its
                        slope at inflection point (discrimination), pseudo-guessing parameter ", strong("\\(c_i\\)"), "describes its
                        lower asymptote and inattention parameter ", strong("\\(d_i\\)"), "describes its upper asymptote."
      ),
      br(),
      h4("Plot with estimated nonlinear curve"),
      p("Points represent proportion of correct answers with respect to standardized total score. Their size is determined by count
                        of respondents who achieved given level of standardized total score."),
      sliderInput(
        inputId = "regression_4pl_item_slider", label = "Item",
        min = 1, value = 1, max = 10, step = 1, animate = animationOptions(interval = 1200)
      ),
      uiOutput("regression_4pl_na_alert"),
      plotlyOutput("regression_4pl_plot"),
      downloadButton("regression_4pl_plot_download", label = "Download figure"),
      h4("Equation"),
      ("$$\\mathrm{P}(Y_{pi} = 1|Z_p) = \\mathrm{E}(Y_{pi}|Z_p) = c_i + \\left(d_i - c_i\\right) \\cdot \\frac{e^{a_i\\left(Z_p - b_i\\right)}}{1 + e^{a_i\\left(Z_p - b_i\\right)}}$$"),
      h4("Table of parameters"),
      fluidRow(column(12, align = "center", tableOutput("regression_4pl_coef"))),
      htmlOutput("regression_4pl_interpretation"),
      br(),
      h4("Selected R code"),
      div(code(HTML("library(difNLR)<br>library(ggplot2)<br><br>#&nbsp;loading&nbsp;data<br>data(GMAT,&nbsp;package&nbsp;=&nbsp;\"difNLR\")<br>data&nbsp;<-&nbsp;GMAT[,&nbsp;1:20]<br>zscore&nbsp;<-&nbsp;scale(rowSums(data))&nbsp;#&nbsp;standardized&nbsp;total&nbsp;score<br><br>#&nbsp;NLR&nbsp;4P&nbsp;model&nbsp;for&nbsp;item&nbsp;1<br>fun&nbsp;<-&nbsp;function(x,&nbsp;a,&nbsp;b,&nbsp;c,&nbsp;d)&nbsp;{<br>&nbsp;&nbsp;c&nbsp;+&nbsp;(d&nbsp;-&nbsp;c)&nbsp;*&nbsp;exp(a&nbsp;*&nbsp;(x&nbsp;-&nbsp;b))&nbsp;/&nbsp;(1&nbsp;+&nbsp;exp(a&nbsp;*&nbsp;(x&nbsp;-&nbsp;b)))<br>}<br><br>fit&nbsp;<-&nbsp;nls(data[,&nbsp;1]&nbsp;~&nbsp;fun(zscore,&nbsp;a,&nbsp;b,&nbsp;c,&nbsp;d),<br>&nbsp;&nbsp;algorithm&nbsp;=&nbsp;\"port\",<br>&nbsp;&nbsp;start&nbsp;=&nbsp;startNLR(<br>&nbsp;&nbsp;&nbsp;&nbsp;data,&nbsp;GMAT[,&nbsp;\"group\"],<br>&nbsp;&nbsp;&nbsp;&nbsp;model&nbsp;=&nbsp;\"4PLcgdg\",<br>&nbsp;&nbsp;&nbsp;&nbsp;parameterization&nbsp;=&nbsp;\"classic\"<br>&nbsp;&nbsp;)[[1]][1:4],<br>&nbsp;&nbsp;lower&nbsp;=&nbsp;c(-Inf,&nbsp;-Inf,&nbsp;0,&nbsp;0),<br>&nbsp;&nbsp;upper&nbsp;=&nbsp;c(Inf,&nbsp;Inf,&nbsp;1,&nbsp;1)<br>)<br><br>#&nbsp;coefficients<br>coef(fit)&nbsp;#&nbsp;estimates<br>sqrt(diag(vcov(fit)))&nbsp;#&nbsp;SE<br>summary(fit)$coefficients[,&nbsp;1:2]&nbsp;#&nbsp;estimates&nbsp;and&nbsp;SE<br><br>#&nbsp;empirical&nbsp;probabilities&nbsp;calculation<br>df&nbsp;<-&nbsp;data.frame(<br>&nbsp;&nbsp;x&nbsp;=&nbsp;sort(unique(zscore)),<br>&nbsp;&nbsp;y&nbsp;=&nbsp;tapply(data[,&nbsp;1],&nbsp;zscore,&nbsp;mean),<br>&nbsp;&nbsp;size&nbsp;=&nbsp;as.numeric(table(zscore))<br>)<br><br>#&nbsp;plot&nbsp;of&nbsp;estimated&nbsp;curve<br>ggplot(df,&nbsp;aes(x&nbsp;=&nbsp;x,&nbsp;y&nbsp;=&nbsp;y))&nbsp;+<br>&nbsp;&nbsp;geom_point(aes(size&nbsp;=&nbsp;size),<br>&nbsp;&nbsp;&nbsp;&nbsp;color&nbsp;=&nbsp;\"darkblue\",<br>&nbsp;&nbsp;&nbsp;&nbsp;fill&nbsp;=&nbsp;\"darkblue\",<br>&nbsp;&nbsp;&nbsp;&nbsp;shape&nbsp;=&nbsp;21,&nbsp;alpha&nbsp;=&nbsp;0.5<br>&nbsp;&nbsp;)&nbsp;+<br>&nbsp;&nbsp;stat_function(<br>&nbsp;&nbsp;&nbsp;&nbsp;fun&nbsp;=&nbsp;fun,&nbsp;geom&nbsp;=&nbsp;\"line\",<br>&nbsp;&nbsp;&nbsp;&nbsp;args&nbsp;=&nbsp;list(<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;a&nbsp;=&nbsp;coef(fit)[1],<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;b&nbsp;=&nbsp;coef(fit)[2],<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;c&nbsp;=&nbsp;coef(fit)[3],<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;d&nbsp;=&nbsp;coef(fit)[4]<br>&nbsp;&nbsp;&nbsp;&nbsp;),<br>&nbsp;&nbsp;&nbsp;&nbsp;size&nbsp;=&nbsp;1,<br>&nbsp;&nbsp;&nbsp;&nbsp;color&nbsp;=&nbsp;\"darkblue\"<br>&nbsp;&nbsp;)&nbsp;+<br>&nbsp;&nbsp;xlab(\"Standardized&nbsp;total&nbsp;score\")&nbsp;+<br>&nbsp;&nbsp;ylab(\"Probability&nbsp;of&nbsp;correct&nbsp;answer\")&nbsp;+<br>&nbsp;&nbsp;ylim(0,&nbsp;1)&nbsp;+<br>&nbsp;&nbsp;ggtitle(\"Item&nbsp;1\")&nbsp;+<br>&nbsp;&nbsp;theme_app()<br>"))),
      br()
    ),
    # * MODELS COMPARISON ####
    tabPanel(
      "Model comparison",
      h3("Logistic regression model selection"),
      p("Here you can compare classic 2PL logistic regression model to non-linear models item by item using some information criteria: "),
      tags$ul(
        tags$li(strong("AIC"), "is the Akaike information criterion (Akaike, 1974), "),
        tags$li(strong("BIC"), "is the Bayesian information criterion (Schwarz, 1978)")
      ),
      # p('Another approach to nested models can be likelihood ratio chi-squared test.
      #   Significance level is set to 0.05. As tests are performed item by item, it is
      #   possible to use multiple comparison correction method. '),
      # selectInput("correction_method_regrmodels", "Correction method",
      #             choices = c("Benjamini-Hochberg" = "BH",
      #                         "Benjamini-Yekutieli" = "BY",
      #                         "Bonferroni" = "bonferroni",
      #                         "Holm" = "holm",
      #                         "Hochberg" = "hochberg",
      #                         "Hommel" = "hommel",
      #                         "None" = "none"),
      #             selected = "none"),
      h4("Table of comparison statistics"),
      # p('Rows ', strong('BEST'), 'indicate which model has the lowest value of criterion, or is the largest
      #   significant model by likelihood ratio test.'),
      p("Rows ", strong("BEST"), "indicate which model has the lowest value of given information criterion."),
      DT::dataTableOutput("regression_comparison_table"),
      br(),
      h4("Selected R code"),
      div(code(HTML("library(difNLR)<br><br>#&nbsp;loading&nbsp;data<br>data(GMAT,&nbsp;package&nbsp;=&nbsp;\"difNLR\")<br>Data&nbsp;<-&nbsp;GMAT[,&nbsp;1:20]<br>zscore&nbsp;<-&nbsp;scale(rowSums(Data))&nbsp;#&nbsp;standardized&nbsp;total&nbsp;score<br><br>#&nbsp;function&nbsp;for&nbsp;fitting&nbsp;models<br>fun&nbsp;<-&nbsp;function(x,&nbsp;a,&nbsp;b,&nbsp;c,&nbsp;d)&nbsp;{<br>&nbsp;&nbsp;c&nbsp;+&nbsp;(d&nbsp;-&nbsp;c)&nbsp;*&nbsp;exp(a&nbsp;*&nbsp;(x&nbsp;-&nbsp;b))&nbsp;/&nbsp;(1&nbsp;+&nbsp;exp(a&nbsp;*&nbsp;(x&nbsp;-&nbsp;b)))<br>}<br><br>#&nbsp;starting&nbsp;values&nbsp;for&nbsp;item&nbsp;1<br>start&nbsp;<-&nbsp;startNLR(<br>&nbsp;&nbsp;Data,&nbsp;GMAT[,&nbsp;\"group\"],&nbsp;model&nbsp;=&nbsp;\"4PLcgdg\",<br>&nbsp;&nbsp;parameterization&nbsp;=&nbsp;\"classic\"<br>)[[1]][,&nbsp;1:4]<br><br>#&nbsp;2PL&nbsp;model&nbsp;for&nbsp;item&nbsp;1<br>fit2PL&nbsp;<-&nbsp;nls(Data[,&nbsp;1]&nbsp;~&nbsp;fun(zscore,&nbsp;a,&nbsp;b,&nbsp;c&nbsp;=&nbsp;0,&nbsp;d&nbsp;=&nbsp;1),<br>&nbsp;&nbsp;algorithm&nbsp;=&nbsp;\"port\",<br>&nbsp;&nbsp;start&nbsp;=&nbsp;start[1:2]<br>)<br>#&nbsp;NLR&nbsp;3P&nbsp;model&nbsp;for&nbsp;item&nbsp;1<br>fit3PL&nbsp;<-&nbsp;nls(Data[,&nbsp;1]&nbsp;~&nbsp;fun(zscore,&nbsp;a,&nbsp;b,&nbsp;c,&nbsp;d&nbsp;=&nbsp;1),<br>&nbsp;&nbsp;algorithm&nbsp;=&nbsp;\"port\",<br>&nbsp;&nbsp;start&nbsp;=&nbsp;start[1:3],<br>&nbsp;&nbsp;lower&nbsp;=&nbsp;c(-Inf,&nbsp;-Inf,&nbsp;0),<br>&nbsp;&nbsp;upper&nbsp;=&nbsp;c(Inf,&nbsp;Inf,&nbsp;1)<br>)<br>#&nbsp;NLR&nbsp;4P&nbsp;model&nbsp;for&nbsp;item&nbsp;1<br>fit4PL&nbsp;<-&nbsp;nls(Data[,&nbsp;1]&nbsp;~&nbsp;fun(zscore,&nbsp;a,&nbsp;b,&nbsp;c,&nbsp;d),<br>&nbsp;&nbsp;algorithm&nbsp;=&nbsp;\"port\",<br>&nbsp;&nbsp;start&nbsp;=&nbsp;start,<br>&nbsp;&nbsp;lower&nbsp;=&nbsp;c(-Inf,&nbsp;-Inf,&nbsp;0,&nbsp;0),<br>&nbsp;&nbsp;upper&nbsp;=&nbsp;c(Inf,&nbsp;Inf,&nbsp;1,&nbsp;1)<br>)<br><br>#&nbsp;comparison<br>###&nbsp;AIC<br>AIC(fit2PL)<br>AIC(fit3PL)<br>AIC(fit4PL)<br>###&nbsp;BIC<br>BIC(fit2PL)<br>BIC(fit3PL)<br>BIC(fit4PL)<br>"))),
      br()
    ),
    "----",
    "Polytomous models",
    # * CUMULATIVE LOGIT ####
    tabPanel("Cumulative logit",
      value = "regr_cum_logit",
      h3("Cumulative logit regression"),
      p(
        "Various regression models may be fitted to describe item properties in more detail.", strong("Cumulative logit regression"),
        " can model cumulative probabilities, i.e., probabilities to obtain item score higher than or equal to 1, 2, 3, etc. "
      ),
      p("Cumulative logit model can be fitted on selected ", strong("matching criterion"), "- standardized total scores or total
                        scores, using IRT or classical (intercept/slope) ", strong("parametrization. ")),
      br(),
      fluidRow(
        column(2, selectInput(
          inputId = "regression_cumulative_matching",
          label = "Matching criterion",
          choices = c(
            "Total score" = "total",
            "Standardized score" = "zscore"
          ),
          selected = "zscore"
        )),
        column(2, selectInput(
          inputId = "regression_cumulative_parametrization",
          label = "Parametrization",
          choices = c(
            "Intercept/slope" = "classic",
            "IRT" = "irt"
          ),
          selected = "irt"
        )),
        column(2, sliderInput(
          inputId = "regression_cumulative_item_slider",
          label = "Item",
          min = 1,
          max = 10,
          value = 1,
          step = 1,
          animate = animationOptions(interval = 1200)
        ))
      ),
      uiOutput("regression_cumulative_na_alert"),
      h4("Plot of cumulative probabilities"),
      p("Lines determine the cumulative probabilities \\(\\mathrm{P}(Y_{pi} \\geq k)\\). Circles represent proportion of answers with
                        at least \\(k\\) points with respect to the matching criterion, i.e., the empirical cumulative probabilities.
                        The size of the points is determined by the count of respondents who achieved given level of the matching
                        criterion."),
      plotlyOutput("regression_cumulative_plot_cumulative"),
      downloadButton("regression_cumulative_plot_cumulative_download", label = "Download figure"),
      h4("Plot of category probabilities"),
      p("Lines determine the category probabilities \\(\\mathrm{P}(Y_{pi} = k)\\). Circles represent proportion of answers with \\(k\\)
                        points with respect to the matching criterion, i.e., the empirical category probabilities. The size of the points
                        is determined by the count of respondents who achieved given level of the matching criterion."),
      plotlyOutput("regression_cumulative_plot_category"),
      downloadButton("regression_cumulative_plot_category_download", label = "Download figure"),
      h4("Equation"),
      fluidRow(column(12, align = "center", uiOutput("regression_cumulative_equation"))),
      uiOutput("regression_cumulative_interpretation"),
      h4("Table of parameters"),
      fluidRow(column(12, align = "center", tableOutput("regression_cumulative_coef"))),
      h4("Selected R code"),
      div(code(HTML("library(msm)<br>library(ShinyItemAnalysis)<br>library(VGAM)<br><br>#&nbsp;loading&nbsp;data<br>data(Science,&nbsp;package&nbsp;=&nbsp;\"mirt\")<br><br>#&nbsp;standardized&nbsp;total&nbsp;score&nbsp;calculation<br>zscore&nbsp;<-&nbsp;scale(rowSums(Science))<br>Science[,&nbsp;1]&nbsp;<-&nbsp;factor(<br>&nbsp;&nbsp;Science[,&nbsp;1],&nbsp;levels&nbsp;=&nbsp;sort(unique(Science[,&nbsp;1])),&nbsp;ordered&nbsp;=&nbsp;TRUE<br>)<br><br>#&nbsp;cumulative&nbsp;logit&nbsp;model&nbsp;for&nbsp;item&nbsp;1<br>fit&nbsp;<-&nbsp;vglm(Science[,&nbsp;1]&nbsp;~&nbsp;zscore,<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;family&nbsp;=&nbsp;cumulative(reverse&nbsp;=&nbsp;TRUE,&nbsp;parallel&nbsp;=&nbsp;TRUE))<br><br>#&nbsp;coefficients&nbsp;under&nbsp;intercept/slope&nbsp;parametrization<br>coef(fit)&nbsp;#&nbsp;estimates<br>sqrt(diag(vcov(fit)))&nbsp;#&nbsp;SE<br><br>#&nbsp;IRT&nbsp;parametrization<br>#&nbsp;delta&nbsp;method<br>num_par&nbsp;<-&nbsp;length(coef(fit))<br>formula&nbsp;<-&nbsp;append(<br>&nbsp;&nbsp;paste0(\"~&nbsp;x\",&nbsp;num_par),<br>&nbsp;&nbsp;as.list(paste0(\"~&nbsp;-x\",&nbsp;1:(num_par&nbsp;-&nbsp;1),&nbsp;\"/\",&nbsp;\"x\",&nbsp;num_par))<br>)<br>formula&nbsp;<-&nbsp;lapply(formula,&nbsp;as.formula)<br>se&nbsp;<-&nbsp;deltamethod(<br>&nbsp;&nbsp;formula,<br>&nbsp;&nbsp;mean&nbsp;=&nbsp;coef(fit),<br>&nbsp;&nbsp;cov&nbsp;=&nbsp;vcov(fit),<br>&nbsp;&nbsp;ses&nbsp;=&nbsp;TRUE<br>)<br>#&nbsp;estimates&nbsp;and&nbsp;SE&nbsp;in&nbsp;IRT&nbsp;parametrization<br>cbind(c(coef(fit)[num_par],&nbsp;-coef(fit)[-num_par]&nbsp;/&nbsp;coef(fit)[num_par]),&nbsp;se)<br><br>#&nbsp;plot&nbsp;of&nbsp;estimated&nbsp;cumulative&nbsp;probabilities<br>plotCumulative(fit,&nbsp;type&nbsp;=&nbsp;\"cumulative\",&nbsp;matching.name&nbsp;=&nbsp;\"Standardized&nbsp;total&nbsp;&nbsp;score\")<br>#&nbsp;plot&nbsp;of&nbsp;estimated&nbsp;category&nbsp;probabilities<br>plotCumulative(fit,&nbsp;type&nbsp;=&nbsp;\"category\",&nbsp;matching.name&nbsp;=&nbsp;\"Standardized&nbsp;total&nbsp;score\")<br>"))),
      br()
    ),
    # * ADJACENT CATEGORY LOGIT ####
    tabPanel("Adjacent category logit",
      value = "regr_adjacent",
      h3("Adjacent category logit regression"),
      p("Models for ordinal responses need not use cumulative probabilities.", strong("Adjacent categories model"), "assumes linear form
                        of logarithm of ratio of probabilities of two successive scores (e.g., 1 vs. 2, 2 vs. 3, etc.), i.e., of the
                        adjacent category logits."),
      p("Adjacent category logit model can be fitted on selected ", strong("matching criterion"), "- standardized total scores or total
                        scores, using IRT or classical (intercept/slope) ", strong("parametrization. ")),
      br(),
      fluidRow(
        column(
          2,
          selectInput(
            inputId = "regression_adjacent_matching",
            choices = c(
              "Total score" = "total",
              "Standardized score" = "zscore"
            ),
            selected = "zscore",
            label = "Matching criterion"
          )
        ),
        column(
          2,
          selectInput(
            inputId = "regression_adjacent_parametrization",
            choices = c(
              "Intercept/slope" = "classic",
              "IRT" = "irt"
            ),
            selected = "irt",
            label = "Parametrization"
          )
        ),
        column(
          2,
          sliderInput(
            inputId = "regression_adjacent_item_slider",
            min = 1,
            max = 10,
            step = 1,
            value = 1,
            label = "Item",
            animate = animationOptions(interval = 1200)
          )
        )
      ),
      uiOutput("regression_adjacent_na_alert"),
      h4("Plot with category probabilities"),
      p("Lines determine the category probabilities \\(\\mathrm{P}(Y_{pi} = k)\\). Circles represent the proportion of answers with \\(k\\)
                        points with respect to the total score, i.e., the empirical category probabilities. The size of the circles is determined by
                        the count of respondents who achieved given level of the total score. "),
      plotlyOutput("regression_adjacent_plot"),
      downloadButton("regression_adjacent_plot_download", label = "Download figure"),
      h4("Equation"),
      fluidRow(column(12, align = "center", uiOutput("regression_adjacent_equation"))),
      uiOutput("regression_adjacent_interpretation"),
      h4("Table of parameters"),
      fluidRow(column(12, align = "center", tableOutput("regression_adjacent_coef"))),
      h4("Selected R code"),
      div(code(HTML("library(msm)<br>library(ShinyItemAnalysis)<br>library(VGAM)<br><br>#&nbsp;loading&nbsp;data<br>data(Science,&nbsp;package&nbsp;=&nbsp;\"mirt\")<br><br>#&nbsp;standardized&nbsp;total&nbsp;score&nbsp;calculation<br>zscore&nbsp;<-&nbsp;scale(rowSums(Science))<br>Science[,&nbsp;1]&nbsp;<-&nbsp;factor(<br>&nbsp;&nbsp;Science[,&nbsp;1],&nbsp;levels&nbsp;=&nbsp;sort(unique(Science[,&nbsp;1])),&nbsp;ordered&nbsp;=&nbsp;TRUE<br>)<br><br>#&nbsp;adjacent&nbsp;category&nbsp;logit&nbsp;model&nbsp;for&nbsp;item&nbsp;1<br>fit&nbsp;<-&nbsp;vglm(Science[,&nbsp;1]&nbsp;~&nbsp;zscore,<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;family&nbsp;=&nbsp;acat(reverse&nbsp;=&nbsp;FALSE,&nbsp;parallel&nbsp;=&nbsp;TRUE))<br><br>#&nbsp;coefficients&nbsp;under&nbsp;intercept/slope&nbsp;parametrization<br>coef(fit)&nbsp;#&nbsp;estimates<br>sqrt(diag(vcov(fit)))&nbsp;#&nbsp;SE<br><br>#&nbsp;IRT&nbsp;parametrization<br>#&nbsp;delta&nbsp;method<br>num_par&nbsp;<-&nbsp;length(coef(fit))<br>formula&nbsp;<-&nbsp;append(<br>&nbsp;&nbsp;paste0(\"~&nbsp;x\",&nbsp;num_par),<br>&nbsp;&nbsp;as.list(paste0(\"~&nbsp;-x\",&nbsp;1:(num_par&nbsp;-&nbsp;1),&nbsp;\"/\",&nbsp;\"x\",&nbsp;num_par))<br>)<br>formula&nbsp;<-&nbsp;lapply(formula,&nbsp;as.formula)<br>se&nbsp;<-&nbsp;deltamethod(<br>&nbsp;&nbsp;formula,<br>&nbsp;&nbsp;mean&nbsp;=&nbsp;coef(fit),<br>&nbsp;&nbsp;cov&nbsp;=&nbsp;vcov(fit),<br>&nbsp;&nbsp;ses&nbsp;=&nbsp;TRUE<br>)<br>#&nbsp;estimates&nbsp;and&nbsp;SE&nbsp;in&nbsp;IRT&nbsp;parametrization<br>cbind(c(coef(fit)[num_par],&nbsp;-coef(fit)[-num_par]&nbsp;/&nbsp;coef(fit)[num_par]),&nbsp;se)<br><br>#&nbsp;plot&nbsp;of&nbsp;estimated&nbsp;category&nbsp;probabilities<br>plotAdjacent(fit,&nbsp;matching.name&nbsp;=&nbsp;\"Standardized&nbsp;total&nbsp;score\")<br>"))),
      br()
    ),
    # * MULTINOMIAL ####
    tabPanel("Multinomial",
      value = "regr_multinom",
      h3("Multinomial regression on standardized total scores"),
      p(
        "Various regression models may be fitted to describe item properties in more detail.", strong("Multinomial regression"), "allows
                        for simultaneous modelling of probability of choosing given distractors on selected ", strong("matching criterion"),
                        "- standardized total scores or total scores, using IRT or classical (intercept/slope) ", strong("parametrization. ")
      ),
      br(),
      h4("Plot with estimated curves of multinomial regression"),
      p("Points represent proportion of selected option with respect to the matching criterion. Their size is determined by count of
                        respondents who achieved given level of the matching criterion and who selected given option."),
      fluidRow(
        column(
          2,
          selectInput(
            inputId = "regression_multinomial_matching",
            choices = c(
              "Total score" = "total",
              "Standardized score" = "zscore"
            ),
            selected = "zscore",
            label = "Matching criterion"
          )
        ),
        column(
          2,
          selectInput(
            inputId = "regression_multinomial_parametrization",
            choices = c(
              "Intercept/slope" = "classic",
              "IRT" = "irt"
            ),
            selected = "irt",
            label = "Parametrization"
          )
        ),
        column(
          2,
          sliderInput(
            inputId = "regression_multinomial_item_slider",
            min = 1,
            max = 10,
            step = 1,
            value = 1,
            label = "Item",
            animate = animationOptions(interval = 1200)
          )
        )
      ),
      uiOutput("regression_multinomial_na_alert"),
      plotlyOutput("regression_multinomial_plot"),
      downloadButton("regression_multinomial_plot_download", label = "Download figure"),
      h4("Equation"),
      fluidRow(column(12, align = "center", uiOutput("regression_multinomial_equation"))),
      h4("Table of parameters"),
      fluidRow(column(12, align = "center", tableOutput("regression_multinomial_coef"))),
      htmlOutput("regression_multinomial_interpretation"),
      h4("Selected R code"),
      div(code(HTML("library(msm)<br>library(nnet)<br>library(ShinyItemAnalysis)<br><br>#&nbsp;loading&nbsp;data<br>data(GMAT,&nbsp;GMATtest,&nbsp;GMATkey,&nbsp;package&nbsp;=&nbsp;\"difNLR\")<br><br>#&nbsp;standardized&nbsp;total&nbsp;score&nbsp;calculation<br>zscore&nbsp;<-&nbsp;scale(rowSums(GMAT[,&nbsp;1:20]))<br><br>#&nbsp;multinomial&nbsp;model&nbsp;for&nbsp;item&nbsp;1<br>fit&nbsp;<-&nbsp;multinom(relevel(GMATtest[,&nbsp;1],&nbsp;ref&nbsp;=&nbsp;paste(GMATkey[1]))&nbsp;~&nbsp;zscore)<br><br>#&nbsp;coefficients&nbsp;under&nbsp;intercept/slope&nbsp;parametrization<br>coef(fit)&nbsp;#&nbsp;estimates<br>sqrt(diag(vcov(fit)))&nbsp;#&nbsp;SE<br><br>#&nbsp;IRT&nbsp;parametrization<br>#&nbsp;delta&nbsp;method<br>subst_vcov&nbsp;<-&nbsp;function(vcov,&nbsp;cat)&nbsp;{<br>&nbsp;&nbsp;ind&nbsp;<-&nbsp;grep(cat,&nbsp;colnames(vcov))<br>&nbsp;&nbsp;vcov[ind,&nbsp;ind]<br>}<br>se&nbsp;<-&nbsp;t(sapply(<br>&nbsp;&nbsp;rownames(coef(fit)),<br>&nbsp;&nbsp;function(.x)&nbsp;{<br>&nbsp;&nbsp;&nbsp;&nbsp;vcov_subset&nbsp;<-&nbsp;subst_vcov(vcov(fit),&nbsp;.x)<br>&nbsp;&nbsp;&nbsp;&nbsp;msm::deltamethod(<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;list(~&nbsp;-x1&nbsp;/&nbsp;x2,&nbsp;~x2),<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;mean&nbsp;=&nbsp;coef(fit)[.x,&nbsp;],<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;cov&nbsp;=&nbsp;vcov_subset,<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;ses&nbsp;=&nbsp;TRUE<br>&nbsp;&nbsp;&nbsp;&nbsp;)<br>&nbsp;&nbsp;}<br>))<br><br>#&nbsp;estimates&nbsp;and&nbsp;SE&nbsp;in&nbsp;IRT&nbsp;parametrization<br>cbind(-coef(fit)[,&nbsp;1]&nbsp;/&nbsp;coef(fit)[,&nbsp;2],&nbsp;se[,&nbsp;1],&nbsp;coef(fit)[,&nbsp;2],&nbsp;se[,&nbsp;2])<br><br>#&nbsp;plot&nbsp;of&nbsp;estimated&nbsp;category&nbsp;probabilities<br>plotMultinomial(fit,&nbsp;zscore,&nbsp;matching.name&nbsp;=&nbsp;\"Standardized&nbsp;total&nbsp;score\")<br>"))),
      br()
    )
  )
