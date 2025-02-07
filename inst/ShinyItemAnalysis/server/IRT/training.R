# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * TRAINING ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ** DICHOTOMOUS MODELS ####

# *** Interpretation ####
output$ccIRT_interpretation <- renderUI({
  a1 <- input$ccIRTSlider_a1
  b1 <- input$ccIRTSlider_b1
  c1 <- input$ccIRTSlider_c1
  d1 <- input$ccIRTSlider_d1

  a2 <- input$ccIRTSlider_a2
  b2 <- input$ccIRTSlider_b2
  c2 <- input$ccIRTSlider_c2
  d2 <- input$ccIRTSlider_d2

  theta0 <- input$ccIRTSlider_theta

  ccirt <- function(theta, a, b, c, d) {
    return(c + (d - c) / (1 + exp(-a * (theta - b))))
  }
  iicirt <- function(theta, a, b, c, d) {
    pi <- c + (d - c) * exp(a * (theta - b)) / (1 + exp(a * (theta - b)))
    return(a^2 * (pi - c)^2 * (d - pi)^2 / (pi * (1 - pi) * (d - c)^2))
  }

  ICC1 <- ccirt(theta0, a1, b1, c1, d1)
  ICC2 <- ccirt(theta0, a2, b2, c2, d2)
  IIC1 <- iicirt(theta0, a1, b1, c1, d1)
  IIC2 <- iicirt(theta0, a2, b2, c2, d2)

  txt1 <- paste0(
    "The probability of the correct answer with the latent ability ",
    paste0("\\(\\theta= ", theta0, "\\)"),
    " in the <font color='red'>red</font> item with parameters ",
    paste0("\\(a = ", a1, "\\)"), ", ",
    paste0("\\(b = ", b1, "\\)"), ", ",
    paste0("\\(c = ", c1, "\\)"), ", and ",
    paste0("\\(d = ", d1, "\\)"),
    " is equal to <b>", sprintf("%.2f", ICC1), "</b>. "
  )
  txt2 <- paste0(
    "The probability of the correct answer with the latent ability ",
    paste0("\\(\\theta= ", theta0, "\\)"),
    " in the <font color='blue'>blue</font> item with parameters ",
    paste0("\\(a = ", a2, "\\)"), ", ",
    paste0("\\(b = ", b2, "\\)"), ", ",
    paste0("\\(c = ", c2, "\\)"), ", and ",
    paste0("\\(d = ", d2, "\\)"),
    " is equal to <b>", sprintf("%.2f", ICC2), "</b>. "
  )
  txt3 <- paste0(
    "The information for the latent ability ",
    paste0("\\(\\theta= ", theta0, "\\)"),
    " in the <font color='red'>red</font> item ",
    " is equal to <b>", sprintf("%.2f", IIC1), "</b>. "
  )
  txt4 <- paste0(
    "The information for the latent ability ",
    paste0("\\(\\theta= ", theta0, "\\)"),
    " in the <font color='blue'>blue</font> item ",
    " is equal to <b>", sprintf("%.2f", IIC2), "</b>. "
  )
  txt <- paste0("<b>Interpretation: </b>", txt1, txt3, txt2, txt4)
  HTML(txt)
})

# *** ICC ####
ccIRT_plot_Input <- reactive({
  a1 <- input$ccIRTSlider_a1
  b1 <- input$ccIRTSlider_b1
  c1 <- input$ccIRTSlider_c1
  d1 <- input$ccIRTSlider_d1

  a2 <- input$ccIRTSlider_a2
  b2 <- input$ccIRTSlider_b2
  c2 <- input$ccIRTSlider_c2
  d2 <- input$ccIRTSlider_d2

  theta0 <- input$ccIRTSlider_theta

  ccirt <- function(theta, a, b, c, d) {
    return(c + (d - c) / (1 + exp(-a * (theta - b))))
  }


  df <- data.frame(
    X1 = ccirt(seq(-4, 4, 0.01), a1, b1, c1, d1),
    X2 = ccirt(seq(-4, 4, 0.01), a2, b2, c2, d2),
    theta = seq(-4, 4, 0.01)
  )
  df <- tidyr::pivot_longer(df, -theta, names_to = "variable") |>
    mutate(variable = as.factor(variable))

  ICC1 <- ccirt(theta0, a = a1, b = b1, c = c1, d = d1)
  ICC2 <- ccirt(theta0, a = a2, b = b2, c = c2, d = d2)
  ICC <- max(ICC1, ICC2)

  g <- ggplot(data = df, aes(x = theta, y = value, col = variable)) +
    geom_line() +
    geom_segment(aes(
      y = ICC1, yend = ICC1,
      x = -4, xend = theta0
    ), color = "gray", linetype = "dashed") +
    geom_segment(aes(
      y = ICC2, yend = ICC2,
      x = -4, xend = theta0
    ), color = "gray", linetype = "dashed") +
    geom_segment(aes(
      y = 0, yend = ICC,
      x = theta0, xend = theta0
    ), color = "gray", linetype = "dashed") +
    xlim(-4, 4) +
    xlab("Ability") +
    ylab("Probability of correct answer") +
    ylim(0, 1) +
    scale_color_manual(
      name = "",
      values = c("red", "blue"),
      labels = c(
        paste(paste(letters[1:4], "=", c(a1, b1, c1, d1)),
          collapse = ", "
        ),
        paste(paste(paste(letters[1:4], "=", c(a2, b2, c2, d2))),
          collapse = ", "
        )
      )
    ) +
    theme_app() +
    ggtitle("Item characteristic curve")
  g
})

output$ccIRT_plot <- renderPlotly({
  g <- ccIRT_plot_Input()

  p <- ggplotly(g)
  theta0 <- input$ccIRTSlider_theta

  # item 1, probabilities
  text <- gsub("~", "", p$x$data[[1]]$text)
  text <- gsub("value", "Probability", text)
  text <- gsub("theta", "Ability", text)
  text <- gsub("variable: X1", "", text)
  p$x$data[[1]]$text <- text

  # item 2, probabilities
  text <- gsub("~", "", p$x$data[[2]]$text)
  text <- gsub("value", "Probability", text)
  text <- gsub("theta", "Ability", text)
  text <- gsub("variable: X2", "", text)
  p$x$data[[2]]$text <- text

  # item 1 and selected theta
  text <- gsub("~", "", p$x$data[[3]]$text)
  text <- gsub("ICC1", "Probability", text)
  text <- gsub("-4: -4<br />", "", text)
  text <- gsub("theta0", "Ability", text)
  text <- gsub("theta: -4<br />", "", text)
  text <- gsub("value", "Probability", text)
  text <- gsub("<br />variable: gray", "", text)
  pos <- gregexpr("Probability", text)[[1]][2]
  text <- substring(text, pos)
  pos <- gregexpr("Probability", text)[[1]][2]
  text <- substring(text, 1, pos - 1)
  p$x$data[[3]]$text <- text

  text <- gsub("~", "", p$x$data[[4]]$text)
  text <- gsub("ICC2", "Probability", text)
  text <- gsub("-4: -4<br />", "", text)
  text <- gsub("theta0", "Ability", text)
  text <- gsub("theta: -4<br />", "", text)
  text <- gsub("value", "Probability", text)
  text <- gsub("<br />variable: gray", "", text)
  pos <- gregexpr("Probability", text)[[1]][2]
  text <- substring(text, pos)
  pos <- gregexpr("Probability", text)[[1]][2]
  text <- substring(text, 1, pos - 1)
  p$x$data[[4]]$text <- text

  text <- gsub("~", "", p$x$data[[5]]$text)
  text <- gsub("ICC", "Probability", text)
  text <- gsub("theta0", "Ability", text)
  text <- gsub("<br />0: 0", "", text)
  text <- gsub("theta0", "Ability", text)
  text <- gsub("value", "Probability", text)
  text <- gsub("<br />variable: gray", "", text)
  text <- gsub(paste0("<br />y: ", theta0), "", text)
  pos <- gregexpr("Ability", text)[[1]][2]
  text <- substring(text, 1, pos - 1)
  p$x$data[[5]]$text <- text

  p$elementId <- NULL
  p |> plotly::config(displayModeBar = FALSE)
})

output$DB_ccIRT <- downloadHandler(
  filename = function() {
    paste("fig_CustomItemCharacteristicCurve.png", sep = "")
  },
  content = function(file) {
    ggsave(file,
      plot = ccIRT_plot_Input() +
        theme(
          legend.position = "inside",
          legend.position.inside = c(0.97, 0.03),
          legend.justification = c(0.97, 0.03)
        ) +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# *** IIC ####
iicIRT_plot_Input <- reactive({
  a1 <- input$ccIRTSlider_a1
  b1 <- input$ccIRTSlider_b1
  c1 <- input$ccIRTSlider_c1
  d1 <- input$ccIRTSlider_d1

  a2 <- input$ccIRTSlider_a2
  b2 <- input$ccIRTSlider_b2
  c2 <- input$ccIRTSlider_c2
  d2 <- input$ccIRTSlider_d2

  theta0 <- input$ccIRTSlider_theta

  iicirt <- function(theta, a, b, c, d) {
    pi <- c + (d - c) * exp(a * (theta - b)) / (1 + exp(a * (theta - b)))
    return(a^2 * (pi - c)^2 * (d - pi)^2 / (pi * (1 - pi) * (d - c)^2))
  }

  df <- data.frame(
    X1 = iicirt(seq(-4, 4, 0.01), a1, b1, c1, d1),
    X2 = iicirt(seq(-4, 4, 0.01), a2, b2, c2, d2),
    theta = seq(-4, 4, 0.01)
  )
  df <- tidyr::pivot_longer(df, -theta, names_to = "variable") |>
    mutate(variable = as.factor(variable))

  IIC1 <- iicirt(theta0, a = a1, b = b1, c = c1, d = d1)
  IIC2 <- iicirt(theta0, a = a2, b = b2, c = c2, d = d2)
  IIC <- max(IIC1, IIC2)

  g <- ggplot(data = df, aes(x = theta, y = value, col = variable)) +
    geom_line() +
    geom_segment(aes(
      y = IIC1, yend = IIC1,
      x = -4, xend = theta0
    ), color = "gray", linetype = "dashed") +
    geom_segment(aes(
      y = IIC2, yend = IIC2,
      x = -4, xend = theta0
    ), color = "gray", linetype = "dashed") +
    geom_segment(aes(
      y = 0, yend = IIC,
      x = theta0, xend = theta0
    ), color = "gray", linetype = "dashed") +
    xlim(-4, 4) +
    ylim(0, 4) +
    xlab("Ability") +
    ylab("Information") +
    scale_color_manual(
      name = "",
      breaks = c("X1", "X2"),
      values = c("red", "blue"),
      labels = c(
        paste(paste(letters[1:4], "=", c(a1, b1, c1, d1)),
          collapse = ", "
        ),
        paste(paste(paste(letters[1:4], "=", c(a2, b2, c2, d2))),
          collapse = ", "
        )
      )
    ) +
    theme_app() +
    ggtitle("Item information function")
  g
})

output$iicIRT_plot <- renderPlotly({
  g <- iicIRT_plot_Input()

  p <- ggplotly(g)
  theta0 <- input$ccIRTSlider_theta

  text <- gsub("~", "", p$x$data[[1]]$text)
  text <- gsub("value", "Information", text)
  text <- gsub("theta", "Ability", text)
  text <- gsub("variable: X1", "", text)
  p$x$data[[1]]$text <- text

  text <- gsub("~", "", p$x$data[[2]]$text)
  text <- gsub("value", "Information", text)
  text <- gsub("theta", "Ability", text)
  text <- gsub("variable: X2", "", text)
  p$x$data[[2]]$text <- text

  # item 1 and selected theta
  text <- gsub("~", "", p$x$data[[3]]$text)
  text <- gsub("IIC1", "Information", text)
  text <- gsub("-4: -4<br />", "", text)
  text <- gsub("theta0", "Ability", text)
  text <- gsub("theta: -4<br />", "", text)
  text <- gsub("value", "Information", text)
  text <- gsub("<br />variable: gray", "", text)
  pos <- gregexpr("Information", text)[[1]][2]
  text <- substring(text, pos)
  pos <- gregexpr("Information", text)[[1]][2]
  text <- substring(text, 1, pos - 1)
  p$x$data[[3]]$text <- text

  text <- gsub("~", "", p$x$data[[4]]$text)
  text <- gsub("IIC2", "Information", text)
  text <- gsub("-4: -4<br />", "", text)
  text <- gsub("theta0", "Ability", text)
  text <- gsub("theta: -4<br />", "", text)
  text <- gsub("value", "Information", text)
  text <- gsub("<br />variable: gray", "", text)
  pos <- gregexpr("Information", text)[[1]][2]
  text <- substring(text, pos)
  pos <- gregexpr("Information", text)[[1]][2]
  text <- substring(text, 1, pos - 1)
  p$x$data[[4]]$text <- text

  text <- gsub("~", "", p$x$data[[5]]$text)
  text <- gsub("IIC", "Information", text)
  text <- gsub("theta0", "Ability", text)
  text <- gsub("<br />0: 0", "", text)
  text <- gsub("theta0", "Ability", text)
  text <- gsub("value", "Information", text)
  text <- gsub("<br />variable: gray", "", text)
  text <- gsub(paste0("<br />y: ", theta0), "", text)
  pos <- gregexpr("Ability", text)[[1]][2]
  text <- substring(text, 1, pos - 1)
  p$x$data[[5]]$text <- text

  p$elementId <- NULL
  p |> plotly::config(displayModeBar = FALSE)
})

output$DB_iicIRT <- downloadHandler(
  filename = function() {
    paste("fig_CustomItemInformationCurve.png", sep = "")
  },
  content = function(file) {
    ggsave(file,
      plot = iicIRT_plot_Input() +
        theme(
          legend.position = "inside",
          legend.position.inside = c(0.97, 0.97),
          legend.justification = c(0.97, 0.97)
        ) +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)
# *** EXERCISES ####
# **** Exercises 1 ####
irt_dich1_answers <- reactive({
  ccirt <- function(theta, a, b, c, d) {
    return(c + (d - c) / (1 + exp(-a * (theta - b))))
  }
  iicirt <- function(theta, a, b, c, d) {
    pi <- c + (d - c) * exp(a * (theta - b)) / (1 + exp(a * (theta - b)))
    return(a^2 * (pi - c)^2 * (d - pi)^2 / (pi * (1 - pi) * (d - c)^2))
  }

  a1 <- 2.5
  b1 <- -0.5
  c1 <- 0
  d1 <- 1
  a2 <- 1.5
  b2 <- 0
  c2 <- 0
  d2 <- 1

  par1 <- c(a1, b1, c1, d1)
  par2 <- c(a2, b2, c2, d2)

  theta0 <- c(-2, -1, 0, 1, 2)

  cci1 <- ccirt(theta0, a1, b1, c1, d1)
  cci2 <- ccirt(theta0, a2, b2, c2, d2)

  theta <- (a1 * b1 - a2 * b2) / (a1 - a2)

  iicirt1a <- iicirt(-2, a1, b1, c1, d1)
  iicirt2a <- iicirt(-2, a2, b2, c2, d2)
  iica <- as.numeric(iicirt1a < iicirt2a) + 1
  iicirt1b <- iicirt(0, a1, b1, c1, d1)
  iicirt2b <- iicirt(0, a2, b2, c2, d2)
  iicb <- as.numeric(iicirt1b < iicirt2b) + 1
  iicirt1c <- iicirt(2, a1, b1, c1, d1)
  iicirt2c <- iicirt(2, a2, b2, c2, d2)
  iicc <- as.numeric(iicirt1c < iicirt2c) + 1

  answers <- list(
    par1 = par1,
    par2 = par2,
    cci1 = cci1,
    cci2 = cci2,
    theta = theta,
    iic = c(iica, iicb, iicc)
  )
  answers
})

irt_training_dich1_check <- eventReactive(input$irt_training_dich1_submit, {
  answers <- irt_dich1_answers()

  # answer 1
  a1 <- input$ccIRTSlider_a1
  b1 <- input$ccIRTSlider_b1
  c1 <- input$ccIRTSlider_c1
  d1 <- input$ccIRTSlider_d1

  a2 <- input$ccIRTSlider_a2
  b2 <- input$ccIRTSlider_b2
  c2 <- input$ccIRTSlider_c2
  d2 <- input$ccIRTSlider_d2

  par1 <- answers[[1]]
  par2 <- answers[[2]]

  par1input <- c(a1, b1, c1, d1)
  par2input <- c(a2, b2, c2, d2)

  ans1 <- c(all(abs(par1 - par1input) <= 0.05) & all(abs(par2 - par2input) <= 0.05))

  # answers 2, item 1
  cci1 <- answers[[3]]
  cci1input <- c(
    input$irt_training_dich1_1_2a, input$irt_training_dich1_1_2b, input$irt_training_dich1_1_2c,
    input$irt_training_dich1_1_2d, input$irt_training_dich1_1_2e
  )
  ans2_1 <- c(abs(cci1 - cci1input) <= 0.05)
  # answers 2, item 1
  cci2 <- answers[[4]]
  cci2input <- c(
    input$irt_training_dich1_2_2a, input$irt_training_dich1_2_2b, input$irt_training_dich1_2_2c,
    input$irt_training_dich1_2_2d, input$irt_training_dich1_2_2e
  )
  ans2_2 <- c(abs(cci2 - cci2input) <= 0.05)

  # answer 3
  ans3 <- c(abs(answers[[5]] - input$irt_training_dich1_3) <= 0.05)

  # answer 4
  ans4 <- c(answers[["iic"]] == c(input$irt_training_dich1_4a, input$irt_training_dich1_4b, input$irt_training_dich1_4c))


  ans <- list(
    ans1 = ans1,
    ans2_1 = ans2_1,
    ans2_2 = ans2_2,
    ans3 = ans3,
    ans4 = ans4
  )
  res <- sum(sapply(ans, sum)) / sum(sapply(ans, length))
  ans <- lapply(ans, function(x) {
    ifelse(is.na(x),
      "<b><font color = 'red'>!</font></b>",
      ifelse(x,
        "<font color='green'>&#10004;</font>",
        "<font color='red'>&#10006;</font>"
      )
    )
  })
  ans[["ans"]] <- res
  ans
})

output$irt_training_dich1_1_answer <- renderUI({
  HTML(irt_training_dich1_check()[["ans1"]])
})

output$irt_training_dich1_2a_1_answer <- renderUI({
  HTML(irt_training_dich1_check()[["ans2_1"]][1])
})

output$irt_training_dich1_2b_1_answer <- renderUI({
  HTML(irt_training_dich1_check()[["ans2_1"]][2])
})

output$irt_training_dich1_2c_1_answer <- renderUI({
  HTML(irt_training_dich1_check()[["ans2_1"]][3])
})

output$irt_training_dich1_2d_1_answer <- renderUI({
  HTML(irt_training_dich1_check()[["ans2_1"]][4])
})

output$irt_training_dich1_2e_1_answer <- renderUI({
  HTML(irt_training_dich1_check()[["ans2_1"]][5])
})

output$irt_training_dich1_2a_2_answer <- renderUI({
  HTML(irt_training_dich1_check()[["ans2_2"]][1])
})

output$irt_training_dich1_2b_2_answer <- renderUI({
  HTML(irt_training_dich1_check()[["ans2_2"]][2])
})

output$irt_training_dich1_2c_2_answer <- renderUI({
  HTML(irt_training_dich1_check()[["ans2_2"]][3])
})

output$irt_training_dich1_2d_2_answer <- renderUI({
  HTML(irt_training_dich1_check()[["ans2_2"]][4])
})

output$irt_training_dich1_2e_2_answer <- renderUI({
  HTML(irt_training_dich1_check()[["ans2_2"]][5])
})

output$irt_training_dich1_3_answer <- renderUI({
  HTML(irt_training_dich1_check()[["ans3"]])
})

output$irt_training_dich1_4a_answer <- renderUI({
  HTML(irt_training_dich1_check()[["ans4"]][1])
})

output$irt_training_dich1_4b_answer <- renderUI({
  HTML(irt_training_dich1_check()[["ans4"]][2])
})

output$irt_training_dich1_4c_answer <- renderUI({
  HTML(irt_training_dich1_check()[["ans4"]][3])
})

output$irt_training_dich1_answer <- renderUI({
  res <- irt_training_dich1_check()[["ans"]]
  HTML(ifelse(is.na(res),
    "<font color = 'red'>Check the format</font>",
    ifelse(res == 1,
      "<font color='green'>Everything correct! Well done!</font>",
      paste0("<font color='red'>", round(100 * res), "% correct. Try again.</font>")
    )
  ))
})


# **** Exercises 2 ####
irt_dich2_answers <- reactive({
  a1 <- 1.5
  b1 <- 0
  c1 <- 0
  d1 <- 1
  a2 <- 1.5
  b2 <- 0
  c2 <- 0.2
  d2 <- 1

  ans1 <- c(c1, c2)
  ans2 <- c((1 + c1) / 2, (1 + c2) / 2)
  ans3 <- 1

  answers <- list(
    ans1 = ans1,
    ans2 = ans2,
    ans3 = ans3
  )
  answers
})

irt_training_dich2_check <- eventReactive(input$irt_training_dich2_submit, {
  answers <- irt_dich2_answers()

  # answer 1
  c1cor <- answers[["ans1"]][1]
  c2cor <- answers[["ans1"]][2]
  c1inp <- input$irt_training_dich2_1a
  c2inp <- input$irt_training_dich2_1b

  ans1 <- c(
    abs(c1cor - c1inp) <= 0.05,
    abs(c2cor - c2inp) <= 0.05
  )

  # answers 2
  p1cor <- answers[["ans2"]][1]
  p2cor <- answers[["ans2"]][2]
  p1inp <- input$irt_training_dich2_2a
  p2inp <- input$irt_training_dich2_2b

  ans2 <- c(
    abs(p1cor - p1inp) <= 0.05,
    abs(p2cor - p2inp) <= 0.05
  )

  # answer 3
  itcor <- answers[["ans3"]]
  itinp <- input$irt_training_dich2_3
  ans3 <- (itcor == itinp)

  ans <- list(
    ans1 = ans1,
    ans2 = ans2,
    ans3 = ans3
  )
  res <- sum(sapply(ans, sum)) / sum(sapply(ans, length))
  ans <- lapply(ans, function(x) {
    ifelse(is.na(x),
      "<b><font color = 'red'>!</font></b>",
      ifelse(x,
        "<font color='green'>&#10004;</font>",
        "<font color='red'>&#10006;</font>"
      )
    )
  })
  ans[["ans"]] <- res
  ans
})

output$irt_training_dich2_1a_answer <- renderUI({
  HTML(irt_training_dich2_check()[["ans1"]][1])
})

output$irt_training_dich2_1b_answer <- renderUI({
  HTML(irt_training_dich2_check()[["ans1"]][2])
})

output$irt_training_dich2_2a_answer <- renderUI({
  HTML(irt_training_dich2_check()[["ans2"]][1])
})

output$irt_training_dich2_2b_answer <- renderUI({
  HTML(irt_training_dich2_check()[["ans2"]][2])
})

output$irt_training_dich2_3_answer <- renderUI({
  HTML(irt_training_dich2_check()[["ans3"]])
})

output$irt_training_dich2_answer <- renderUI({
  res <- irt_training_dich2_check()[["ans"]]
  HTML(ifelse(is.na(res),
    "<font color = 'red'>Check the format</font>",
    ifelse(res == 1,
      "<font color='green'>Everything correct! Well done!</font>",
      paste0("<font color='red'>", round(100 * res), "% correct. Try again.</font>")
    )
  ))
})

# **** Exercises 3 ####
irt_dich3_answers <- reactive({
  a1 <- 1.5
  b1 <- 0
  c1 <- 0
  d1 <- 0.9
  a2 <- 1.5
  b2 <- 0
  c2 <- 0
  d2 <- 1

  ans1 <- c(d1, d2)
  ans2 <- c(d1 / 2, d2 / 2)
  ans3 <- 2

  answers <- list(
    ans1 = ans1,
    ans2 = ans2,
    ans3 = ans3
  )
  answers
})

irt_training_dich3_check <- eventReactive(input$irt_training_dich3_submit, {
  answers <- irt_dich3_answers()

  # answer 1
  d1cor <- answers[["ans1"]][1]
  d2cor <- answers[["ans1"]][2]
  d1inp <- input$irt_training_dich3_1a
  d2inp <- input$irt_training_dich3_1b

  ans1 <- c(
    abs(d1cor - d1inp) <= 0.05,
    abs(d2cor - d2inp) <= 0.05
  )

  # answers 2
  p1cor <- answers[["ans2"]][1]
  p2cor <- answers[["ans2"]][2]
  p1inp <- input$irt_training_dich3_2a
  p2inp <- input$irt_training_dich3_2b

  ans2 <- c(
    abs(p1cor - p1inp) <= 0.05,
    abs(p2cor - p2inp) <= 0.05
  )

  # answer 3
  itcor <- answers[["ans3"]]
  itinp <- input$irt_training_dich3_3

  ans3 <- (itcor == itinp)

  ans <- list(
    ans1 = ans1,
    ans2 = ans2,
    ans3 = ans3
  )
  res <- sum(sapply(ans, sum)) / sum(sapply(ans, length))
  ans <- lapply(ans, function(x) {
    ifelse(is.na(x),
      "<b><font color = 'red'>!</font></b>",
      ifelse(x,
        "<font color='green'>&#10004;</font>",
        "<font color='red'>&#10006;</font>"
      )
    )
  })
  ans[["ans"]] <- res
  ans
})

output$irt_training_dich3_1a_answer <- renderUI({
  HTML(irt_training_dich3_check()[["ans1"]][1])
})

output$irt_training_dich3_1b_answer <- renderUI({
  HTML(irt_training_dich3_check()[["ans1"]][2])
})

output$irt_training_dich3_2a_answer <- renderUI({
  HTML(irt_training_dich3_check()[["ans2"]][1])
})

output$irt_training_dich3_2b_answer <- renderUI({
  HTML(irt_training_dich3_check()[["ans2"]][2])
})

output$irt_training_dich3_3_answer <- renderUI({
  HTML(irt_training_dich3_check()[["ans3"]])
})

output$irt_training_dich3_answer <- renderUI({
  res <- irt_training_dich3_check()[["ans"]]
  HTML(ifelse(is.na(res),
    "<font color = 'red'>Check the format</font>",
    ifelse(res == 1,
      "<font color='green'>Everything correct! Well done!</font>",
      paste0("<font color='red'>", round(100 * res), "% correct. Try again.</font>")
    )
  ))
})

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ** POLYTOMOUS MODELS ####

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# *** GRADED RESPONSE MODEL ####

output$irt_training_grm_sliders <- renderUI({
  req(input$irt_training_grm_numresp, input$irt_training_grm_numresp >= 2, input$irt_training_grm_numresp <= 6)

  num <- input$irt_training_grm_numresp

  sliders <- tagList(
    tags$div(
      class = "js-irs-red",
      style = "display: inline-block; vertical-align: middle; width: 18%;",
      sliderInput("irt_training_grm_b1",
        label = "\\(b_1\\) - difficulty",
        value = -2.5, min = -4, max = 4, step = 0.1
      )
    ),
    div(style = "display: inline-block; vertical-align: middle; width: 2.6%;", ""),
    tags$div(
      class = "js-irs-yellow",
      style = "display: inline-block; vertical-align: middle; width: 18%;",
      sliderInput("irt_training_grm_b2",
        label = "\\(b_2\\) - difficulty",
        value = -1.5, min = -4, max = 4, step = 0.1
      )
    ),
    div(style = "display: inline-block; vertical-align: middle; width: 2.6%;", ""),
    tags$div(
      class = "js-irs-green",
      style = "display: inline-block; vertical-align: middle; width: 18%;",
      sliderInput("irt_training_grm_b3",
        label = "\\(b_3\\) - difficulty",
        value = -0.5, min = -4, max = 4, step = 0.1
      )
    ),
    div(style = "display: inline-block; vertical-align: middle; width: 2.6%;", ""),
    tags$div(
      class = "js-irs-blue",
      style = "display: inline-block; vertical-align: middle; width: 18%;",
      sliderInput("irt_training_grm_b4",
        label = "\\(b_4\\) - difficulty",
        value = 0.5, min = -4, max = 4, step = 0.1
      )
    ),
    div(style = "display: inline-block; vertical-align: middle; width: 2.6%;", ""),
    tags$div(
      class = "js-irs-purple",
      style = "display: inline-block; vertical-align: middle; width: 18%;",
      sliderInput("irt_training_grm_b5",
        label = "\\(b_5\\) - difficulty",
        value = 1.5, min = -4, max = 4, step = 0.1
      )
    ),
    div(style = "display: inline-block; vertical-align: middle; width: 2.6%;", ""),
    tags$div(
      class = "js-irs-orange",
      style = "display: inline-block; vertical-align: middle; width: 18%;",
      sliderInput("irt_training_grm_b6",
        label = "\\(b_6\\) - difficulty",
        value = 2.5, min = -4, max = 4, step = 0.1
      )
    ),
    div(style = "display: inline-block; vertical-align: middle; width: 2.6%;", "")
  )

  sliders <- sliders[1:(2 * num)]

  sliders
})
# *** Cumulative ####
irt_training_grm_plot_cumulative_Input <- reactive({
  req(input$irt_training_grm_numresp, input$irt_training_grm_numresp >= 2, input$irt_training_grm_numresp <= 6)

  input$irt_training_grm_numresp

  num <- input$irt_training_grm_numresp

  a <- input$irt_training_grm_a

  if (is.null(input$irt_training_grm_b1)) {
    b <- c(-2.5, -1.5, -0.5, 0.5, 1.5, 2.5)
    b <- b[1:num]
  } else {
    b <- c(input$irt_training_grm_b1, input$irt_training_grm_b2)
    b <- switch(paste(num),
      "2" = b,
      "3" = c(b, input$irt_training_grm_b3),
      "4" = c(b, input$irt_training_grm_b3, input$irt_training_grm_b4),
      "5" = c(b, input$irt_training_grm_b3, input$irt_training_grm_b4, input$irt_training_grm_b5),
      "6" = c(b, input$irt_training_grm_b3, input$irt_training_grm_b4, input$irt_training_grm_b5, input$irt_training_grm_b6)
    )
  }

  theta <- seq(-4, 4, 0.01)

  ccirt <- function(theta, a, b) {
    return(1 / (1 + exp(-a * (theta - b))))
  }

  df <- data.frame(sapply(1:num, function(i) ccirt(theta, a, b[i])), theta)
  df <- tidyr::pivot_longer(df, -theta, names_to = "variable") |>
    mutate(variable = as.factor(variable))

  col <- c("red", "#e6b800", "#00b300", "blue", "#990099", "#ff6600")
  col <- col[1:num]

  g <- ggplot(data = df, aes(x = theta, y = value, col = variable)) +
    geom_line() +
    xlab("Ability") +
    ylab("Cumulative probability") +
    xlim(-4, 4) +
    ylim(0, 1) +
    scale_color_manual("", values = col, labels = paste0("P(Y >= ", 1:length(col), ")")) +
    theme_app() +
    ggtitle("Cumulative probabilities")

  g
})

output$irt_training_grm_plot_cumulative <- renderPlotly({
  g <- irt_training_grm_plot_cumulative_Input()

  p <- ggplotly(g)

  for (i in 1:length(p$x$data)) {
    text <- gsub("~", "", p$x$data[[i]]$text)
    text <- gsub("value", "Cumulative probability", text)
    text <- gsub("theta", "Ability", text)
    text <- gsub(paste0("variable: X", i), paste0("P(Y >= ", i, ")"), text)
    p$x$data[[i]]$text <- text
  }

  p$elementId <- NULL
  p |> plotly::config(displayModeBar = FALSE)
})

output$DB_irt_training_grm_plot_cumulative <- downloadHandler(
  filename = function() {
    paste("fig_GRM_cumulative.png", sep = "")
  },
  content = function(file) {
    ggsave(file,
      plot = irt_training_grm_plot_cumulative_Input() +
        theme(
          legend.position = "inside",
          legend.position.inside = c(0.97, 0.7),
          legend.justification = c(0.97, 0.97)
        ) +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# *** Category probabilities ####
irt_training_grm_plot_category_Input <- reactive({
  req(input$irt_training_grm_numresp, input$irt_training_grm_numresp >= 2, input$irt_training_grm_numresp <= 6)

  num <- input$irt_training_grm_numresp

  a <- input$irt_training_grm_a

  if (is.null(input$irt_training_grm_b1)) {
    b <- c(-2.5, -1.5, -0.5, 0.5, 1.5, 2.5)
    b <- b[1:num]
  } else {
    b <- c(input$irt_training_grm_b1, input$irt_training_grm_b2)
    b <- switch(paste(num),
      "2" = b,
      "3" = c(b, input$irt_training_grm_b3),
      "4" = c(b, input$irt_training_grm_b3, input$irt_training_grm_b4),
      "5" = c(b, input$irt_training_grm_b3, input$irt_training_grm_b4, input$irt_training_grm_b5),
      "6" = c(b, input$irt_training_grm_b3, input$irt_training_grm_b4, input$irt_training_grm_b5, input$irt_training_grm_b6)
    )
  }

  theta <- seq(-4, 4, 0.01)

  ccirt <- function(theta, a, b) {
    return(1 / (1 + exp(-a * (theta - b))))
  }

  df <- data.frame(X0 = 1, sapply(1:length(b), function(i) ccirt(theta, a, b[i])))
  df <- data.frame(sapply(1:(ncol(df) - 1), function(i) df[, i] - df[, i + 1]),
    X99 = df[, ncol(df)],
    theta
  )
  df <- tidyr::pivot_longer(df, -theta, names_to = "variable") |>
    mutate(variable = as.factor(variable))

  levels(df$variable) <- paste0("X", 0:(nlevels(df$variable) - 1))

  col <- c("black", "red", "#e6b800", "#00b300", "blue", "#990099", "#ff6600")
  col <- col[1:((length(levels(df$variable)) - 1) + 1)]

  g <- ggplot(data = df, aes(x = theta, y = value, col = variable)) +
    geom_line() +
    xlab("Ability") +
    ylab("Category probability") +
    xlim(-4, 4) +
    ylim(0, 1) +
    scale_color_manual("", values = col, labels = paste0("P(Y >= ", 0:(length(col) - 1), ")")) +
    theme_app() +
    ggtitle("Category probabilities")

  g
})

output$irt_training_grm_plot_category <- renderPlotly({
  g <- irt_training_grm_plot_category_Input()

  p <- ggplotly(g)

  for (i in 1:length(p$x$data)) {
    text <- gsub("~", "", p$x$data[[i]]$text)
    text <- gsub("value", "Category probability", text)
    text <- gsub("theta", "Ability", text)
    text <- gsub(paste0("variable: X", i - 1), paste0("P(Y = ", i - 1, ")"), text)
    p$x$data[[i]]$text <- text
  }

  p$elementId <- NULL
  p |> plotly::config(displayModeBar = FALSE)
})

output$DB_irt_training_grm_plot_category <- downloadHandler(
  filename = function() {
    paste("fig_GRM_category.png", sep = "")
  },
  content = function(file) {
    ggsave(file,
      plot = irt_training_grm_plot_category_Input() +
        theme(
          legend.position = "inside",
          legend.position.inside = c(0.97, 0.7),
          legend.justification = c(0.97, 0.97)
        ) +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# *** Expected item score ####
irt_training_grm_plot_expected_Input <- reactive({
  req(input$irt_training_grm_numresp, input$irt_training_grm_numresp >= 2, input$irt_training_grm_numresp <= 6)

  num <- input$irt_training_grm_numresp

  a <- input$irt_training_grm_a

  if (is.null(input$irt_training_grm_b1)) {
    b <- c(-2.5, -1.5, -0.5, 0.5, 1.5, 2.5)
    b <- b[1:num]
  } else {
    b <- c(input$irt_training_grm_b1, input$irt_training_grm_b2)
    b <- switch(paste(num),
      "2" = b,
      "3" = c(b, input$irt_training_grm_b3),
      "4" = c(b, input$irt_training_grm_b3, input$irt_training_grm_b4),
      "5" = c(b, input$irt_training_grm_b3, input$irt_training_grm_b4, input$irt_training_grm_b5),
      "6" = c(b, input$irt_training_grm_b3, input$irt_training_grm_b4, input$irt_training_grm_b5, input$irt_training_grm_b6)
    )
  }

  theta <- seq(-4, 4, 0.01)

  ccirt <- function(theta, a, b) {
    return(1 / (1 + exp(-a * (theta - b))))
  }

  df <- data.frame(1, sapply(1:length(b), function(i) ccirt(theta, a, b[i])))
  df <- data.frame(
    sapply(1:(ncol(df) - 1), function(i) df[, i] - df[, i + 1]),
    df[, ncol(df)]
  )
  df <- data.frame(exp = as.matrix(df) %*% c(0:(dim(df)[2] - 1)), theta)

  g <- ggplot(data = df, aes(x = theta, y = exp)) +
    geom_line() +
    xlab("Ability") +
    ylab("Expected item score") +
    xlim(-4, 4) +
    ylim(0, num) +
    theme_app() +
    ggtitle("Expected item score")

  g
})

output$irt_training_grm_plot_expected <- renderPlotly({
  g <- irt_training_grm_plot_expected_Input()

  p <- ggplotly(g)

  for (i in 1:length(p$x$data)) {
    text <- gsub("~", "", p$x$data[[i]]$text)
    text <- gsub("theta", "Ability", text)
    text <- gsub("exp", "Expected score", text)
    text <- paste0(text, "<br />E(Y)")
    p$x$data[[i]]$text <- text
  }

  p$elementId <- NULL
  p |> plotly::config(displayModeBar = FALSE)
})

output$DB_irt_training_grm_plot_expected <- downloadHandler(
  filename = function() {
    paste("fig_GRM_expected.png", sep = "")
  },
  content = function(file) {
    ggsave(file,
      plot = irt_training_grm_plot_expected_Input() +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)
# *** Exercise ###
irt_training_grm_answer <- reactive({
  cdf_k1 <- function(theta, a, b1) {
    return(exp(a * (theta - b1)) / (1 + exp(a * (theta - b1))))
  }
  cdf_k2 <- function(theta, a, b2) {
    return(exp(a * (theta - b2)) / (1 + exp(a * (theta - b2))))
  }
  cdf_k3 <- function(theta, a, b3) {
    return(exp(a * (theta - b3)) / (1 + exp(a * (theta - b3))))
  }

  theta0 <- c(-2, -1, 0, 1, 2)
  a <- 1
  b1 <- -0.5
  b2 <- 1
  b3 <- 1.5

  ck0 <- rep(1, 5)
  ck1 <- cdf_k1(theta0, a, b1)
  ck2 <- cdf_k2(theta0, a, b2)
  ck3 <- cdf_k3(theta0, a, b3)

  prob_k0 <- c(1 - ck1)
  prob_k1 <- c(ck1 - ck2)
  prob_k2 <- c(ck2 - ck3)
  prob_k3 <- as.numeric(apply(as.data.frame(rbind(prob_k0, prob_k1, prob_k2)), 2, function(x) 1 - sum(x)))

  exp_v <- as.numeric(as.matrix(cbind(prob_k0, prob_k1, prob_k2, prob_k3)) %*% 0:3)
  bb <- input$irt_training_grm_numresp

  answers <- list(
    ans1_1 = ck0,
    ans1_2 = ck1,
    ans1_3 = ck2,
    ans1_4 = ck3,
    ans2_1 = prob_k0,
    ans2_2 = prob_k1,
    ans2_3 = prob_k2,
    ans2_4 = prob_k3,
    ans3 = exp_v
  )
  answers
})

irt_training_grm_check <- eventReactive(input$irt_training_grm_1_submit, {
  answers <- irt_training_grm_answer()

  # answ 1_1
  cdf_k1_input <- c(
    input$irt_training_grm_1_1a, input$irt_training_grm_1_1b,
    input$irt_training_grm_1_1c, input$irt_training_grm_1_1d,
    input$irt_training_grm_1_1e
  )
  ans1_1 <- c(abs(answers[[1]] - cdf_k1_input) <= 0.05)


  # answ 1_2
  cdf_k2_input <- c(
    input$irt_training_grm_1_2a, input$irt_training_grm_1_2b,
    input$irt_training_grm_1_2c, input$irt_training_grm_1_2d,
    input$irt_training_grm_1_2e
  )
  ans1_2 <- c(abs(answers[[2]] - cdf_k2_input) <= 0.05)

  # answ 1_3
  cdf_k3_input <- c(
    input$irt_training_grm_1_3a, input$irt_training_grm_1_3b,
    input$irt_training_grm_1_3c, input$irt_training_grm_1_3d,
    input$irt_training_grm_1_3e
  )
  ans1_3 <- c(abs(answers[[3]] - cdf_k3_input) <= 0.05)

  # answ 1_4
  cdf_k4_input <- c(
    input$irt_training_grm_1_4a, input$irt_training_grm_1_4b,
    input$irt_training_grm_1_4c, input$irt_training_grm_1_4d,
    input$irt_training_grm_1_4e
  )
  ans1_4 <- c(abs(answers[[4]] - cdf_k4_input) <= 0.05)

  # answ 2_1
  prob_k0_input <- c(
    input$irt_training_grm_2_1a, input$irt_training_grm_2_1b,
    input$irt_training_grm_2_1c, input$irt_training_grm_2_1d,
    input$irt_training_grm_2_1e
  )
  ans2_1 <- c(abs(answers[[5]] - prob_k0_input) <= 0.05)

  # answ 2_2
  prob_k1_input <- c(
    input$irt_training_grm_2_2a, input$irt_training_grm_2_2b,
    input$irt_training_grm_2_2c, input$irt_training_grm_2_2d,
    input$irt_training_grm_2_2e
  )
  ans2_2 <- c(abs(answers[[6]] - prob_k1_input) <= 0.05)

  # answ 2_3
  prob_k2_input <- c(
    input$irt_training_grm_2_3a, input$irt_training_grm_2_3b,
    input$irt_training_grm_2_3c, input$irt_training_grm_2_3d,
    input$irt_training_grm_2_3e
  )
  ans2_3 <- c(abs(answers[[7]] - prob_k2_input) <= 0.05)

  # answ 2_4
  prob_k3_input <- c(
    input$irt_training_grm_2_4a, input$irt_training_grm_2_4b,
    input$irt_training_grm_2_4c, input$irt_training_grm_2_4d,
    input$irt_training_grm_2_4e
  )
  ans2_4 <- c(abs(answers[[8]] - prob_k3_input) <= 0.05)

  # answ 3
  exp_v_input <- c(
    input$irt_training_grm_3_1a, input$irt_training_grm_3_2a,
    input$irt_training_grm_3_3a, input$irt_training_grm_3_4a,
    input$irt_training_grm_3_5a
  )
  ans3 <- c(abs(answers[[9]] - exp_v_input) <= 0.05)


  ans <- list(
    ans1 = ans1_1,
    ans2 = ans1_2,
    ans3 = ans1_3,
    ans4 = ans1_4,
    ans5 = ans2_1,
    ans6 = ans2_2,
    ans7 = ans2_3,
    ans8 = ans2_4,
    ans9 = ans3
  )

  res <- sum(sapply(ans, sum)) / sum(sapply(ans, length))
  ans <- lapply(ans, function(x) {
    ifelse(is.na(x),
      "<b><font color = 'red'>!</font></b>",
      ifelse(x,
        "<font color='green'>&#10004;</font>",
        "<font color='red'>&#10006;</font>"
      )
    )
  })
  ans[["ans"]] <- res
  ans
})

output$irt_training_grm_1_1a_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans1"]][1])
})

output$irt_training_grm_1_1b_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans1"]][2])
})

output$irt_training_grm_1_1c_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans1"]][3])
})

output$irt_training_grm_1_1d_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans1"]][4])
})

output$irt_training_grm_1_1e_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans1"]][5])
})

output$irt_training_grm_1_2a_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans2"]][1])
})

output$irt_training_grm_1_2b_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans2"]][2])
})

output$irt_training_grm_1_2c_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans2"]][3])
})

output$irt_training_grm_1_2d_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans2"]][4])
})

output$irt_training_grm_1_2e_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans2"]][5])
})

output$irt_training_grm_1_3a_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans3"]][1])
})

output$irt_training_grm_1_3b_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans3"]][2])
})

output$irt_training_grm_1_3c_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans3"]][3])
})

output$irt_training_grm_1_3d_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans3"]][4])
})

output$irt_training_grm_1_3e_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans3"]][5])
})

output$irt_training_grm_1_4a_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans4"]][1])
})

output$irt_training_grm_1_4b_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans4"]][2])
})

output$irt_training_grm_1_4c_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans4"]][3])
})

output$irt_training_grm_1_4d_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans4"]][4])
})

output$irt_training_grm_1_4e_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans4"]][5])
})

output$irt_training_grm_2_1a_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans5"]][1])
})

output$irt_training_grm_2_1b_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans5"]][2])
})

output$irt_training_grm_2_1c_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans5"]][3])
})

output$irt_training_grm_2_1d_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans5"]][4])
})

output$irt_training_grm_2_1e_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans5"]][5])
})

output$irt_training_grm_2_2a_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans6"]][1])
})

output$irt_training_grm_2_2b_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans6"]][2])
})

output$irt_training_grm_2_2c_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans6"]][3])
})

output$irt_training_grm_2_2d_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans6"]][4])
})

output$irt_training_grm_2_2e_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans6"]][5])
})

output$irt_training_grm_2_3a_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans7"]][1])
})

output$irt_training_grm_2_3b_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans7"]][2])
})

output$irt_training_grm_2_3c_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans7"]][3])
})

output$irt_training_grm_2_3d_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans7"]][4])
})

output$irt_training_grm_2_3e_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans7"]][5])
})

output$irt_training_grm_2_4a_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans8"]][1])
})

output$irt_training_grm_2_4b_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans8"]][2])
})

output$irt_training_grm_2_4c_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans8"]][3])
})

output$irt_training_grm_2_4d_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans8"]][4])
})

output$irt_training_grm_2_4e_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans8"]][5])
})

output$irt_training_grm_3_1a_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans9"]][1])
})

output$irt_training_grm_3_2a_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans9"]][2])
})

output$irt_training_grm_3_3a_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans9"]][3])
})

output$irt_training_grm_3_4a_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans9"]][4])
})

output$irt_training_grm_3_5a_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans9"]][5])
})



output$irt_training_grm_answer <- renderUI({
  res <- irt_training_grm_check()[["ans"]]
  HTML(ifelse(is.na(res),
    "<font color = 'red'>Check the format</font>",
    ifelse(res == 1,
      "<font color='green'>Everything correct! Well done!</font>",
      paste0("<font color='red'>", round(100 * res), "% correct. Try again.</font>")
    )
  ))
})


# *** GENERALIZED PARTIAL CREDIT MODEL ####

output$irt_training_gpcm_sliders <- renderUI({
  req(input$irt_training_gpcm_numresp, input$irt_training_gpcm_numresp >= 2, input$irt_training_gpcm_numresp <= 6)

  num <- input$irt_training_gpcm_numresp

  sliders <- tagList(
    tags$div(
      class = "js-irs-red",
      style = "display: inline-block; vertical-align: middle; width: 18%;",
      sliderInput("irt_training_gpcm_d1",
        label = "\\(b_1\\) - threshold",
        value = -1.5, min = -4, max = 4, step = 0.1
      )
    ),
    div(style = "display: inline-block; vertical-align: middle; width: 2.6%;", ""),
    tags$div(
      class = "js-irs-yellow",
      style = "display: inline-block; vertical-align: middle; width: 18%;",
      sliderInput("irt_training_gpcm_d2",
        label = "\\(b_2\\) - threshold",
        value = -1, min = -4, max = 4, step = 0.1
      )
    ),
    div(style = "display: inline-block; vertical-align: middle; width: 2.6%;", ""),
    tags$div(
      class = "js-irs-green",
      style = "display: inline-block; vertical-align: middle; width: 18%;",
      sliderInput("irt_training_gpcm_d3",
        label = "\\(b_3\\) - threshold",
        value = -0.5, min = -4, max = 4, step = 0.1
      )
    ),
    div(style = "display: inline-block; vertical-align: middle; width: 2.6%;", ""),
    tags$div(
      class = "js-irs-blue",
      style = "display: inline-block; vertical-align: middle; width: 18%;",
      sliderInput("irt_training_gpcm_d4",
        label = "\\(b_4\\) - threshold",
        value = 0, min = -4, max = 4, step = 0.1
      )
    ),
    div(style = "display: inline-block; vertical-align: middle; width: 2.6%;", ""),
    tags$div(
      class = "js-irs-purple",
      style = "display: inline-block; vertical-align: middle; width: 18%;",
      sliderInput("irt_training_gpcm_d5",
        label = "\\(b_5\\) - threshold",
        value = 0.5, min = -4, max = 4, step = 0.1
      )
    ),
    div(style = "display: inline-block; vertical-align: middle; width: 2.6%;", ""),
    tags$div(
      class = "js-irs-orange",
      style = "display: inline-block; vertical-align: middle; width: 18%;",
      sliderInput("irt_training_gpcm_d6",
        label = "\\(b_6\\) - threshold",
        value = 1, min = -4, max = 4, step = 0.1
      )
    ),
    div(style = "display: inline-block; vertical-align: middle; width: 2.6%;", "")
  )

  sliders <- sliders[1:(2 * num)]

  sliders
})

# *** Category probabilities ####
irt_training_gpcm_plot_Input <- reactive({
  req(input$irt_training_gpcm_numresp, input$irt_training_gpcm_numresp >= 2, input$irt_training_gpcm_numresp <= 6)

  num <- input$irt_training_gpcm_numresp

  a <- input$irt_training_gpcm_a

  if (is.null(input$irt_training_gpcm_d1)) {
    d <- c(-1.5, -1, -0.5, 0, 0.5, 1)
    d <- d[1:num]
  } else {
    d <- c(input$irt_training_gpcm_d1, input$irt_training_gpcm_d2)
    d <- switch(paste(num),
      "2" = d,
      "3" = c(d, input$irt_training_gpcm_d3),
      "4" = c(d, input$irt_training_gpcm_d3, input$irt_training_gpcm_d4),
      "5" = c(d, input$irt_training_gpcm_d3, input$irt_training_gpcm_d4, input$irt_training_gpcm_d5),
      "6" = c(d, input$irt_training_gpcm_d3, input$irt_training_gpcm_d4, input$irt_training_gpcm_d5, input$irt_training_gpcm_d6)
    )
  }

  theta <- seq(-4, 4, 0.01)

  ccgpcm <- function(theta, a, d) {
    a * (theta - d)
  }

  df <- sapply(1:length(d), function(i) ccgpcm(theta, a, d[i]))

  pk <- sapply(1:ncol(df), function(k) apply(as.data.frame(df[, 1:k]), 1, sum))

  pk <- cbind(0, pk)
  pk <- exp(pk)

  denom <- apply(pk, 1, sum)

  df <- data.frame(apply(pk, 2, function(x) x / denom), theta)
  df <- tidyr::pivot_longer(df, -theta, names_to = "variable") |>
    mutate(variable = as.factor(variable))

  col <- c("black", "red", "#e6b800", "#00b300", "blue", "#990099", "#ff6600")
  col <- col[1:(length(levels(df$variable)) + 1)]

  g <- ggplot(data = df, aes(x = theta, y = value, col = variable)) +
    geom_line() +
    xlab("Ability") +
    ylab("Category probability") +
    xlim(-4, 4) +
    ylim(0, 1) +
    scale_color_manual("", values = col, labels = paste0("P(Y = ", 0:(length(col) - 1), ")")) +
    theme_app() +
    ggtitle("Category probabilities")

  g
})

output$irt_training_gpcm_plot <- renderPlotly({
  g <- irt_training_gpcm_plot_Input()

  p <- ggplotly(g)

  for (i in 1:length(p$x$data)) {
    text <- gsub("~", "", p$x$data[[i]]$text)
    text <- gsub("value", "Category probability", text)
    text <- gsub("theta", "Ability", text)
    text <- gsub(paste0("variable: X", i), paste0("P(Y = ", i - 1, ")"), text)
    p$x$data[[i]]$text <- text
  }

  p$elementId <- NULL
  p |> plotly::config(displayModeBar = FALSE)
})

output$DB_irt_training_gpcm_plot <- downloadHandler(
  filename = function() {
    paste("fig_GPCM_category.png", sep = "")
  },
  content = function(file) {
    ggsave(file,
      plot = irt_training_gpcm_plot_Input() +
        theme(
          legend.position = "inside",
          legend.position.inside = c(0.97, 0.7),
          legend.justification = c(0.97, 0.97)
        ) +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)


# *** Expected item score ####
irt_training_gpcm_plot_expected_Input <- reactive({
  req(input$irt_training_gpcm_numresp, input$irt_training_gpcm_numresp >= 2, input$irt_training_gpcm_numresp <= 6)

  num <- input$irt_training_gpcm_numresp

  a <- input$irt_training_gpcm_a

  if (is.null(input$irt_training_gpcm_d1)) {
    d <- c(-1.5, -1, -0.5, 0, 0.5, 1)
    d <- d[1:num]
  } else {
    d <- c(input$irt_training_gpcm_d1, input$irt_training_gpcm_d2)
    d <- switch(paste(num),
      "2" = d,
      "3" = c(d, input$irt_training_gpcm_d3),
      "4" = c(d, input$irt_training_gpcm_d3, input$irt_training_gpcm_d4),
      "5" = c(d, input$irt_training_gpcm_d3, input$irt_training_gpcm_d4, input$irt_training_gpcm_d5),
      "6" = c(d, input$irt_training_gpcm_d3, input$irt_training_gpcm_d4, input$irt_training_gpcm_d5, input$irt_training_gpcm_d6)
    )
  }

  theta <- seq(-4, 4, 0.01)

  ccgpcm <- function(theta, a, d) {
    a * (theta - d)
  }

  df <- sapply(1:length(d), function(i) ccgpcm(theta, a, d[i]))

  pk <- sapply(1:ncol(df), function(k) apply(as.data.frame(df[, 1:k]), 1, sum))

  pk <- cbind(0, pk)
  pk <- exp(pk)

  denom <- apply(pk, 1, sum)

  df <- data.frame(apply(pk, 2, function(x) x / denom))
  df <- data.frame(exp = as.matrix(df) %*% c(0:(dim(df)[2] - 1)), theta)

  g <- ggplot(data = df, aes(x = theta, y = exp)) +
    geom_line() +
    xlab("Ability") +
    ylab("Expected item score") +
    xlim(-4, 4) +
    ylim(0, num) +
    theme_app() +
    ggtitle("Expected item score")

  g
})

output$irt_training_gpcm_plot_expected <- renderPlotly({
  g <- irt_training_gpcm_plot_expected_Input()

  p <- ggplotly(g)

  for (i in 1:length(p$x$data)) {
    text <- gsub("~", "", p$x$data[[i]]$text)
    text <- gsub("theta", "Ability", text)
    text <- gsub("exp", "Expected score", text)
    text <- paste0(text, "<br />E(Y)")
    p$x$data[[i]]$text <- text
  }

  p$elementId <- NULL
  p |> plotly::config(displayModeBar = FALSE)
})

output$DB_irt_training_gpcm_plot_expected <- downloadHandler(
  filename = function() {
    paste("fig_GPCM_expected.png", sep = "")
  },
  content = function(file) {
    ggsave(file,
      plot = irt_training_gpcm_plot_expected_Input() +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# *** Exercise ###
irt_gpcm_answer <- reactive({
  ans1 <- c("No", "No", "No", "Yes", "Yes", "Yes", "No", "No", "No")

  a <- 1
  d <- c(-1, 1)
  theta <- seq(-4, 4, 0.01)

  ccgpcm <- function(theta, a, d) {
    a * (theta - d)
  }
  df <- sapply(1:length(d), function(i) ccgpcm(theta, a, d[i]))
  pk <- sapply(1:ncol(df), function(k) apply(as.data.frame(df[, 1:k]), 1, sum))
  pk <- cbind(0, pk)
  pk <- exp(pk)
  denom <- apply(pk, 1, sum)
  df <- apply(pk, 2, function(x) x / denom)

  df1 <- tidyr::pivot_longer(data.frame(df, theta), -theta, names_to = "variable") |>
    mutate(variable = as.factor(variable))


  df2 <- data.frame(exp = as.matrix(df) %*% 0:2, theta)

  ans2 <- c(df2$exp[which(theta %in% c(-1.50, 0, 1.50))])
  ans3 <- "Yes"

  a2 <- 2

  df <- sapply(1:length(d), function(i) ccgpcm(theta, a2, d[i]))
  pk <- sapply(1:ncol(df), function(k) apply(as.data.frame(df[, 1:k]), 1, sum))
  pk <- cbind(0, pk)
  pk <- exp(pk)
  denom <- apply(pk, 1, sum)
  df <- apply(pk, 2, function(x) x / denom)
  df1 <- tidyr::pivot_longer(data.frame(df, theta), -theta, names_to = "variable") |>
    mutate(variable = as.factor(variable))

  df2 <- data.frame(exp = as.matrix(df) %*% 0:2, theta)

  ans4 <- c(df2$exp[which(theta %in% c(-1.50, 0, 1.50))])

  answers <- list(
    ans1 = ans1,
    ans2 = ans2,
    ans3 = ans3,
    ans4 = ans4
  )

  answers
})

irt_gpcm_check <- eventReactive(input$irt_training_gpcm_1_submit, {
  answers <- irt_gpcm_answer()

  # answ 1_1
  idx <- as.integer(input$irt_training_gpcm_1)
  theta_input <- rep("No", 9)
  theta_input[idx] <- "Yes"
  ans1 <- all(theta_input == answers[[1]])

  exp_theta_input_1 <- c(input$irt_training_gpcm_2_1, input$irt_training_gpcm_2_2, input$irt_training_gpcm_2_3)

  ans2 <- c(abs(answers[[2]] - exp_theta_input_1) <= 0.05)

  ans3 <- input$irt_training_gpcm_3 == answers[[3]]

  exp_theta_input_2 <- c(input$irt_training_gpcm_4_1, input$irt_training_gpcm_4_2, input$irt_training_gpcm_4_3)

  ans4 <- c(abs(answers[[4]] - exp_theta_input_2) <= 0.05)

  ans <- list(
    ans1 = ans1,
    ans2 = ans2,
    ans3 = ans3,
    ans4 = ans4
  )

  res <- sum(sapply(ans, sum)) / sum(sapply(ans, length))
  ans <- lapply(ans, function(x) {
    ifelse(is.na(x),
      "<b><font color = 'red'>!</font></b>",
      ifelse(x,
        "<font color='green'>&#10004;</font>",
        "<font color='red'>&#10006;</font>"
      )
    )
  })
  ans[["ans"]] <- res
  ans
})

output$irt_training_gpcm_1_answer <- renderUI({
  HTML(irt_gpcm_check()[["ans1"]][1])
})

output$irt_training_gpcm_2_1_answer <- renderUI({
  HTML(irt_gpcm_check()[["ans2"]][1])
})

output$irt_training_gpcm_2_2_answer <- renderUI({
  HTML(irt_gpcm_check()[["ans2"]][2])
})

output$irt_training_gpcm_2_3_answer <- renderUI({
  HTML(irt_gpcm_check()[["ans2"]][3])
})

output$irt_training_gpcm_3_answer <- renderUI({
  HTML(irt_gpcm_check()[["ans3"]][1])
})

output$irt_training_gpcm_4_1_answer <- renderUI({
  HTML(irt_gpcm_check()[["ans4"]][1])
})

output$irt_training_gpcm_4_2_answer <- renderUI({
  HTML(irt_gpcm_check()[["ans4"]][2])
})

output$irt_training_gpcm_4_3_answer <- renderUI({
  HTML(irt_gpcm_check()[["ans4"]][3])
})


output$irt_training_gpcm_answer <- renderUI({
  res <- irt_gpcm_check()[["ans"]]
  HTML(ifelse(is.na(res),
    "<font color = 'red'>Check the format</font>",
    ifelse(res == 1,
      "<font color='green'>Everything correct! Well done!</font>",
      paste0("<font color='red'>", round(100 * res), "% correct. Try again.</font>")
    )
  ))
})



# *** NOMINAL RESPONSE MODEL ####

irt_training_nrm_thetas <- seq(-6, 6, by = .1)

irt_training_nrm_category_names <- c(
  "red",
  "yellow",
  "green",
  "blue",
  "purple",
  "orange",
  "brown"
)


irt_training_nrm_slider_input_names <- reactive({
  idx <- seq_len(input$irt_training_nrm_numresp + 1)
  list(a = paste0("irt_training_a", idx), b = paste0("irt_training_b", idx))
})

output$irt_training_nrm_sliders <- renderUI({
  pars <- irt_training_nrm_slider_input_names()
  n_cats <- length(pars$a)

  init_values <- list(
    a = c(seq(-2, 0, length.out = n_cats)[seq_len(n_cats - 1)], 0),
    b = c(seq(-1.5, 1.5, length.out = n_cats - 1), 0)
  )

  slider_names <- list(
    a = paste0("\\(a_{", c(irt_training_nrm_category_names[seq_len(n_cats) - 1L], "grey"), "}\\) (discrimination)"),
    b = paste0("\\(b_{", c(irt_training_nrm_category_names[seq_len(n_cats) - 1L], "grey"), "}\\) (threshold)")
  )


  # when changing number of categories, there is is a point when new sliders
  # are generated with their initial values, but those are immediately passed
  # further to dependent reactives which are eagerly trying to compute probs
  # and show them in a plot -- we need them to "freeze" until all full round
  # of invalidation of dependent reactives or outputs
  freezeReactiveValue(input, pars$a[1])


  a_sliders <- pmap(
    list(pars$a, slider_names$a, init_values$a),
    ~ sliderInput(..1, ..2, min = -3, max = 0, value = ..3, step = .1, width = "100%")
  )

  b_sliders <- pmap(
    list(pars$b, slider_names$b, init_values$b),
    ~ sliderInput(..1, ..2, min = -5, max = 5, value = ..3, step = .1, width = "100%")
  )

  # fix last category sliders to zero
  a_sliders[[n_cats]]$children[[2L]]$attribs$`data-from-min` <- 0
  a_sliders[[n_cats]]$children[[2L]]$attribs$`data-from-max` <- 0
  b_sliders[[n_cats]]$children[[2L]]$attribs$`data-from-min` <- 0
  b_sliders[[n_cats]]$children[[2L]]$attribs$`data-from-max` <- 0

  slider_classes <- c(
    c(
      "js-irs-red", "js-irs-yellow", "js-irs-green", "js-irs-blue",
      "js-irs-purple", "js-irs-orange", "js-irs-brown"
    )[seq_len(n_cats - 1)], "js-irs-grey"
  )

  fluidRow(
    pmap(
      list(a_sliders, b_sliders, slider_classes),
      ~ {
        column(
          3,
          fluidRow(
            column(
              12,
              div(class = ..3, ..1)
            )
          ),
          fluidRow(
            column(
              12,
              div(class = ..3, ..2)
            )
          )
        )
      }
    )
  )
})

# collect pars from sliders
irt_training_nrm_slider_input_values <- reactive({
  slider_input_names <- irt_training_nrm_slider_input_names()

  a <- map_dbl(slider_input_names$a, ~ input[[.x]])
  b <- map_dbl(slider_input_names$b, ~ input[[.x]])

  list(a = a, b = b)
})


irt_training_nrm_parametrizations <- reactive({
  pars <- irt_training_nrm_slider_input_values()
  a <- pars$a
  b <- pars$b

  blis <- list(a_star = NA, a = a, b = -a * b)
  blirt <- list(a_star = NA, a = a, b = b)

  # thissen
  a_star <- (a[length(a)] - a[1L]) / (length(a) - 1L)
  thissen <- list(
    a_star = a_star,
    a = (a - a[1L]) / a_star,
    b = blis$b - blis$b[1L] # work with BLIS pars
  )

  irt_thissen <- list(
    a_star = thissen$a_star,
    a = thissen$a,
    b = c(0, thissen$b[-1] / (-thissen$a[-1] * thissen$a_star))
  )
  # TODO we are avoiding dividing by zero here, but this does not seem legit to me at all...

  # bock - naturally from thissen
  a_bock <- thissen$a_star * thissen$a # get rid of a_star
  bock <- list(
    a_star = NA,
    a = a_bock - mean(a_bock),
    b = thissen$b - mean(thissen$b)
  )

  irt_bock <- list(
    a = bock$a,
    b = bock$b / -bock$a # really don't know what those bs means to curve intercepts
  )
  # TODO when a is zero, what is the result??


  list(
    BLIS = blis, BLIRT = blirt,
    `Thissen et al.` = thissen, `Thissen et al. (IRT)` = irt_thissen,
    Bock = bock, `Bock (IRT)` = irt_bock
  )
})



irt_training_nrm_pars_list <- reactive({
  params <- irt_training_nrm_parametrizations()
  n_cats <- length(seq_along(params$BLIRT$a))

  out <- params |> map_dfr(unlist, .id = "Parametrization")

  nms <- c(irt_training_nrm_category_names[seq_len(n_cats) - 1L], "grey")
  names(out) <- c(
    names(out[1L]), "\\(a^*\\)",
    paste0("\\({apar}_{", nms, "}\\)"),
    paste0("\\({bpar}_{", nms, "}\\)")
  )

  out
})


output$irt_training_nrm_irt_parameters <- renderTable({
  out <- irt_training_nrm_pars_list() |>
    filter(Parametrization %in% c("BLIRT", "Thissen et al. (IRT)", "Bock (IRT)"))

  names(out) <- names(out) |>
    str_replace("apar", "a") |>
    str_replace("bpar", "b")

  out
})
output$irt_training_nrm_int_slope_parameters <- renderTable({
  out <- irt_training_nrm_pars_list() |>
    filter(Parametrization %in% c("BLIS", "Thissen et al.", "Bock"))

  names(out) <- names(out) |>
    str_replace("apar", "\\\\beta_1") |>
    str_replace("bpar", "\\\\beta_0")

  out
})


irt_training_nrm_cat_probs <- reactive({
  pars <- irt_training_nrm_parametrizations()

  lin_pred <- sapply(seq_along(pars$BLIRT$a), function(i) {
    pars$BLIRT$a[i] * (irt_training_nrm_thetas - pars$BLIRT$b[i])
  })

  exponentiated <- exp(lin_pred)
  out <- exponentiated / (rowSums(exponentiated))

  nms <- irt_training_nrm_category_names[seq_along(pars$BLIRT$a)]
  nms[length(nms)] <- "grey"
  colnames(out) <- nms

  out |>
    as_tibble() |>
    mutate(theta = irt_training_nrm_thetas, .before = 1)
})


irt_training_nrm_cat_probs_plot <- reactive({
  pars <- irt_training_nrm_slider_input_values()

  vlines <- pars$b[seq_len(length(pars$b) - 1)]

  d <- irt_training_nrm_cat_probs()

  d_long <- d |>
    pivot_longer(-theta, names_to = "cat")

  d_long <- d_long |> mutate(tooltip = paste0(
    str_to_title(cat), " category\n",
    "Category probability = ", round(value, 3), "\n",
    "Ability = ", theta
  ))

  d_long |>
    ggplot(aes(x = theta, y = value, col = cat, group = cat, text = tooltip)) +
    geom_vline(xintercept = vlines, col = "grey", linetype = "dashed") +
    geom_line(linewidth = .8) +
    scale_color_identity() +
    scale_x_continuous(expand = expansion()) +
    labs(x = "Ability", y = "Category probability") +
    coord_cartesian(ylim = c(0, 1)) +
    theme_app()
})


output$irt_training_nrm_cat_probs_plotly <- renderPlotly({
  irt_training_nrm_cat_probs_plot() |>
    ggplotly(tooltip = "text") |>
    layout(showlegend = FALSE) |>
    plotly::config(displayModeBar = FALSE)
})
