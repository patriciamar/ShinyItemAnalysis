#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# RELIABILITY ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * SPEARMAN-BROWN FORMULA ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Update numeric inputs ######
observe({
  rel1 <- round(as.numeric(reliability_cronbachalpha_table_Input()[, 1]), 2)
  rel2 <- min(0.99, rel1 + 0.1)

  items1 <- ncol(correct_answ())
  items2 <- items1 + 10


  updateNumericInput(session = session,
                     inputId = "reliability_SBformula_reliability_original",
                     value = rel1)
  updateNumericInput(session = session,
                     inputId = "reliability_SBformula_reliability_new",
                     value = rel2)
  updateNumericInput(session = session,
                     inputId = "reliability_SBformula_items_original",
                     value = items1)
  updateNumericInput(session = session,
                     inputId = "reliability_SBformula_items_new",
                     value = items2)
})

# ** Calculation of reliability ####
output$reliability_SBformula_reliability_text <- renderUI({
  rel_ori <- input$reliability_SBformula_reliability_original
  ite_ori <- input$reliability_SBformula_items_original

  ite_new <- input$reliability_SBformula_items_new

  m <- ite_new/ite_ori

  rel_new <- psychometric::SBrel(Nlength = m, rxx = rel_ori)

  txt <- paste("Reliability of test with", ite_new,
               ifelse(ite_new == 1, "item", "items"),
               "would be <b>",
               round(rel_new, 3), "</b>")
  HTML(txt)
})

# ** Calculation of test length ####
output$reliability_SBformula_items_text <- renderText({
  rel_ori <- input$reliability_SBformula_reliability_original
  ite_ori <- input$reliability_SBformula_items_original

  rel_new <- input$reliability_SBformula_reliability_new


  m <- psychometric::SBlength(rxxp = rel_new, rxx = rel_ori)
  ite_new <- ceiling(m*ite_ori)

  txt <- paste("It is necessary to have <b>", ite_new,
               ifelse(ite_new == 1, "item", "items"),
               "</b> to gain reliability of",
               round(rel_new, 3))
  HTML(txt)
})


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * SPLIT-HALF ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * BETA ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Split-half number of split halves heading ######
output$reliability_splithalf_number_label <- renderText({
  method <- input$reliability_splithalf_method

  if (method %in% c("average", "worst")){
    txt <- "Number of split halves"
  } else {
    txt <- "Number of split halves (for histogram)"
  }

  txt
})

# ** Split-half items ######
reliability_splithalf_items <- reactive({
  method <- input$reliability_splithalf_method

  i_num <- item_numbers()

  n <- length(i_num)

  if (method == "firstlast"){
    k <- ceiling(n/2)
    items1 <- 1:k
  } else {
    if (method == "evenodd"){
      items1 <- seq(1, n, 2)
    } else {
      if (method == "random"){
        samp <- sample(1:n, ceiling(n/2))
        items1 <- sort(samp)
      } else {
        if (method == "worst"){
          split <- reliability_splithalf_raw()
          items1 <- which(split$minAB[, "A"] == 1)
        } else {
          items1 <- NULL
        }
      }
    }
  }

  items2 <- setdiff(1:n, items1)

  list(items1 = items1, items2 = items2)
})

# ** Split-half text items ######
output$reliability_splithalf_text <- renderUI({
  items <- reliability_splithalf_items()

  items1 <- items$items1
  items2 <- items$items2

  if (is.null(items1)){
    txt <- NULL
  } else {
    txt <- paste("First subset contains items: <b>",
                 paste(item_names()[items1], collapse = ", "), "</b> <br>",
                 "Second subset contains items: <b>",
                 paste(item_names()[items2], collapse = ", "), "</b> <br>")
  }

  HTML(txt)
})

# ** Update numeric inputs ######
observe({
  data <- correct_answ()
  n <- ncol(data)

  k <- ceiling(n/2)
  num <- ifelse(k == n/2, choose(n, k)/2, choose(n, k))

  updateNumericInput(session = session,
                     inputId = "reliability_splithalf_method",
                     max = choose(n, k))
})

# ** Split-half all possible split-halves ######
output$reliability_splithalf_allpossible_text <- renderUI({
  data <- correct_answ()
  n <- ncol(data)

  k <- ceiling(n/2)
  num <- ifelse(k == n/2, choose(n, k)/2, choose(n, k))

  txt <- paste("<b>Note:</b> For dataset with <b>", n, "items</b>,
               there are <b>", num, "</b> possible split-halves")

  HTML(txt)
})

# ** Split-half correlation and reliability calculation for average/worst method ######
reliability_splithalf_raw <- reactive({
  data <- correct_answ()

  n.sample <- input$reliability_splithalf_number
  split <- psych::splitHalf(data, raw = T, n.sample = n.sample)

  split
})

# ** Split-half correlation and reliability calculation ######
reliability_splithalf_estimate <- reactive({
  items <- reliability_splithalf_items()

  items1 <- items$items1
  items2 <- items$items2

  i_num <- item_numbers()

  n <- length(i_num)

  data <- correct_answ()

  if (is.null(items1)){
    split <- reliability_splithalf_raw()

    rel.y <- mean(split$raw)
    n <- length(split$raw)
    rel.low <- rel.y - 1.96 * sd(split$raw)/sqrt(n)
    rel.upp <- rel.y + 1.96 * sd(split$raw)/sqrt(n)

  } else {
    df1 <- data[, items1, with = F]
    df2 <- data[, items2, with = F]

    ts1 <- apply(df1, 1, sum)
    ts2 <- apply(df2, 1, sum)

    cor.y <- cor(ts1, ts2)
    z.r <- 0.5*log((1 + cor.y)/(1 - cor.y))
    n <- length(ts1)
    z.low <- z.r - 1.96 * sqrt(1/(n - 3))
    z.upp <- z.r + 1.96 * sqrt(1/(n - 3))


    cor.low <- (exp(2*z.low) - 1)/(exp(2*z.low) + 1)
    cor.upp <- (exp(2*z.upp) - 1)/(exp(2*z.upp) + 1)

    rel.y <- 2*cor.y/(1 + cor.y)
    rel.low <- 2*cor.low/(1 + cor.low)
    rel.upp <- 2*cor.upp/(1 + cor.upp)
  }

  list(rel.y = rel.y,
       rel.low = rel.low,
       rel.upp = rel.upp)
})


# ** Split-half correlation and reliability calculation ######
output$reliability_splithalf_table <- renderTable({
  tab <- reliability_splithalf_estimate()
  tab <- unlist(tab)
  tab <- data.table("Estimate" = tab[1],
                    "Confidence interval" = paste0("(", sprintf("%.3f", tab[2]), ", ",
                                                   sprintf("%.3f", tab[3]), ")"))

  tab
},
digits = 3)

# ** Split-halves histogram ######
reliability_splithalf_histogram_Input <- reactive({
  data <- correct_answ()

  split <- reliability_splithalf_raw()

  val <- reliability_splithalf_estimate()[[1]]

  val <- round(val, 2)
  col <- as.factor(val - 0.005 < split$raw & split$raw < val + 0.005)
  brk <- sort(unique(round(split$raw, 2)))

  ggplot(data.frame(rel = split$raw), aes(x = rel, fill = col)) +
    geom_histogram(col = "black", binwidth = 0.01) +
    scale_fill_manual(values = c("grey45", "red")) +
    scale_x_continuous(breaks = brk) +
    ylab("Count") +
    xlab("Reliability estimate") +
    theme_app()
})

# ** Split-halves histogram output ######
output$reliability_splithalf_histogram <- renderPlot({
  reliability_splithalf_histogram_Input()
})

# ** DB for Split-halves histogram ######
output$DB_reliability_splithalf_histogram <- downloadHandler(
  filename =  function() {
    "fig_reliability_splithalf.png"
  },
  content = function(file) {
    ggsave(file, plot = reliability_splithalf_histogram_Input() +
             theme(text = element_text(size = 10)),
           device = "png",
           height = 4, width = 8, dpi = 300)
  }
)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * ALPHA ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Cronbach's alpha table ######
reliability_cronbachalpha_table_Input <- reactive({
  data <- correct_answ()

  a <- psychometric::alpha(data)
  a.low <- psychometric::alpha.CI(a, k = ncol(data), N = nrow(data), level = 0.95)[1]
  a.upp <- psychometric::alpha.CI(a, k = ncol(data), N = nrow(data), level = 0.95)[3]

  tab <- data.table("Estimate" = a,
                    "Confidence interval" = paste0("(", sprintf("%.3f", a.low, 3), ", ",
                                                   sprintf("%.3f", a.upp), ")"))

  tab
})

# ** Output Cronbach's alpha table ######
output$reliability_cronbachalpha_table <- renderTable({
  reliability_cronbachalpha_table_Input()
},
digits = 3,
include.rownames = F,
include.colnames = T)


